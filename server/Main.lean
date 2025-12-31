import Std.Internal.Http
import Lean.Data.Json
import Server.Metrics
import Server.Html

open Std Http Internal IO Async Net
open Lean

/--
Represents a single search result hit containing declaration information
-/
structure Hit where
  name: String
  type: String
  module: String
  doc: Option String
deriving ToJson, FromJson, Repr, Inhabited

/--
Response data structure for search queries
-/
structure SearchResult where
  error: Option String := none
  heartbeats: Nat := 0
  suggestions: Option (Array String) := none
  header: String := ""
  count: Nat := 0
  hits: Array Hit := #[]
deriving ToJson, FromJson, Inhabited, Repr

/--
Metrics definitions for Prometheus monitoring
-/
abbrev metricsConfig : List (String × Loogle.GraphType) :=
  [ ("versions", .map ["loogle", "mathlib"] .string),
    ("queries", .counter),
    ("errors", .counter),
    ("results", .histogram [0, 1, 2, 5, 10, 50, 100, 200, 500, 1000]),
    ("heartbeats", .histogram [0, 2^0, 2^1, 2^2, 2^3, 2^4]),
    ("clients", .map ["web", "zulip", "json", "nvim", "vscode-lean4", "vscode-loogle", "LeanSearchClient", "lean-lsp-mcp"] .nat),
    ("errors", .counter) ]

namespace Loogle

/--
Reasons why the Loogle backend process might fail
-/
inductive BackendFailureReason
  | noGreeting
  | processExited (exitCode: UInt32)
  | sandboxEscapeAttempt
  | noResponse

instance : ToString BackendFailureReason where
  toString
    | .noGreeting => "No greeting received from backend"
    | .processExited exitCode => s!"Backend process exited with code {exitCode}"
    | .sandboxEscapeAttempt => "Backend attempted to escape the sandbox"
    | .noResponse => "No response received from backend"

/--
Configuration for spawning the Loogle backend process
-/
def backendConfig : Process.SpawnArgs := {
  cmd := ".lake/build/bin/loogle",
  args := #["--json", "--interactive", "--module", "Std"],
  stdin := .piped,
  stdout := .piped
}

/--
Internal state of the database connection
-/
structure DatabaseState where
  child : Process.Child backendConfig.toStdioConfig
  isInitializing : Bool

/--
Thread-safe database connection to the Loogle backend
-/
structure Database where
  state : Std.Mutex DatabaseState

namespace Database

/--
Start a new Loogle backend process
-/
def start : IO Database := do
  let child ← Process.spawn backendConfig
  let state ← Std.Mutex.new { child := child, isInitializing := true }
  return ⟨state⟩

/--
Restart the backend process after a failure
-/
def restart (db : Database) : IO Unit := do
  db.state.atomically do
    let current ← get
    current.child.kill
    set (DatabaseState.mk (← Process.spawn backendConfig) true)

/--
Wait for the backend to signal readiness
-/
def waitForReady (db : Database) : IO Bool := do
  let dbState ← db.state.atomically get
  let response ← dbState.child.stdout.getLine

  if response = "Loogle is ready.\n" then
    db.state.atomically do modify (fun s => { s with isInitializing := false })
    return true
  else
    return false

/--
Execute a query against the backend
-/
def executeQuery (db : Database) (query : String) : IO (Except BackendFailureReason SearchResult) := do
  let dbState ← db.state.atomically get
  if dbState.isInitializing then
    return .error .noGreeting
  else
    try
      dbState.child.stdin.write (String.toUTF8 $ query ++ "\n")
      dbState.child.stdin.flush
      let outputJson ← dbState.child.stdout.getLine
      match Lean.Json.parse outputJson >>= FromJson.fromJson? with
      | Except.ok output => return .ok output
      | Except.error e => throw <| IO.userError s!"JSON decode error: {e}"
    catch _ =>
      -- Allow the process to terminate
      IO.sleep 5000
      let exitCode ← dbState.child.wait

      if exitCode == 255 then
        db.restart
        return .error .sandboxEscapeAttempt
      else if exitCode != 0 then
        db.restart
        return .error $ .processExited exitCode
      else
        dbState.child.kill
        db.restart
        return .error .noResponse

end Database

/--
Application context containing shared state for all request handlers
-/
structure Context where
  metrics : Loogle.Metrics metricsConfig
  staticAssets : Std.Mutex (HashMap String ByteArray)
  database : Database

/--
Retrieve a static asset from the context
-/
def Context.getAsset (ctx : Context) (name : String) : IO ByteArray := do
  ctx.staticAssets.atomically do
    let assets ← get
    return assets.get! name

/--
Monad type for Loogle request handlers
-/
abbrev LoogleM := ReaderT Context Async

namespace Database

/--
Execute a query with metrics tracking
-/
def query (db : Database) (queryString : String) : LoogleM (Except BackendFailureReason SearchResult) := do
  let context : Context ← MonadReader.read
  context.metrics.increment "queries"
  let output ← executeQuery db queryString

  match output with
  | .ok data => do
    context.metrics.addValue "results" data.count
    context.metrics.addValue "heartbeats" data.heartbeats
    return .ok data
  | .error err => do
    context.metrics.increment "errors"
    return .error err

end Database

/--
Track client types based on headers for metrics
-/
def trackClient (context : Context) (headers : Headers) (isJsonRequest : Bool) : Async Unit := do
  if isJsonRequest then
    let xLoogleClient := headers.get? (.new "x-loogle-client") |>.getD (.new "")
    let userAgent := headers.get? (.new "user-agent") |>.getD (.new "")

    if xLoogleClient.value.contains "lean4/" then
      context.metrics.incrementLabel "clients" "vscode-lean4"
    else if userAgent.value.contains "LeanSearchClient" then
      context.metrics.incrementLabel "clients" "LeanSearchClient"
    else if userAgent.value.contains "vscode" then
      context.metrics.incrementLabel "clients" "vscode-loogle"
    else if userAgent.value.contains "lean.nvim" || userAgent.value.contains "lean+nvim" then
      context.metrics.incrementLabel "clients" "nvim"
    else if userAgent.value.contains "lean-lsp-mcp" then
      context.metrics.incrementLabel "clients" "lean-lsp-mcp"
    else
      context.metrics.incrementLabel "clients" "json"
  else
    context.metrics.incrementLabel "clients" "web"

/--
Retrieve the current git commit hash of the application
-/
def getGitRevision : IO String := do
  let output ← IO.Process.output {cmd := "git", args := #["rev-parse", "HEAD"]}
  if output.exitCode = 0
    then pure output.stdout.trimAscii.toString
    else pure "UNKNOWN"

/--
Extract the mathlib revision from lake-manifest.json
-/
def getMathlibRevision : IO String := do
  let some manifestContent ← tryCatch (some <$> IO.FS.readFile "./lake-manifest.json") (fun _ => pure none)
    | return "UNKNOWN"

  let .ok (Json.arr packages) := Json.parse manifestContent >>= (Json.getObjVal? · "packages")
    | throw $ IO.userError "cannot parse manifest json"

  let mathlibPackage := packages.find? fun p =>
    match p.getObjVal? "name" with
    | .ok (Json.str "mathlib") => true
    | _ => false

  let some pkg := mathlibPackage
    | return "UNKNOWN"

  let .ok (Json.str rev) := pkg.getObjVal? "rev"
    | return "UNKNOWN"

  return rev

/--
Generate a local search link
-/
def makeLocalSearchLink (query : String) : String :=
  s!"?q={URI.encodeURIComponent query}"

/--
Generate an external Loogle search link
-/
def makeSearchLink (query : String) : String :=
  s!"https://loogle.lean-lang.org/?q={URI.encodeURIComponent query}"

/--
Generate a documentation link for a hit
-/
def makeDocLink (hit : Hit) : String :=
  let modpath := hit.module.replace "." "/"
  s!"https://leanprover-community.github.io/mathlib4_docs/{URI.encodeURIComponent modpath}.html#{URI.encodeURIComponent hit.name}"

/--
Format a hit for Zulip with Markdown link
-/
def formatZulipHit (hit : Hit) : String :=
  s!"[{hit.name}]({makeDocLink hit})"

/--
Format a query suggestion for Zulip
-/
def formatZulipQueryLink (suggestion : String) : String :=
  s!"[`{suggestion}`]({makeSearchLink suggestion})"

/--
Error display component
-/
def errorComponent (errorMsg: String) : Html :=
  {{
      <div>
        <h2>"Error"</h2>
        <pre>{{errorMsg}}</pre>
      </div>
  }}

/--
Header display component
-/
def headerComponent (headerText: String) : Html :=
 {{
    <div>
      <h2>"Result"</h2>
      <p>{{headerText}}</p>
    </div>
 }}

/--
Individual hit display component
-/
def hitComponent (hit: Hit) : Html :=
  {{
    <li>
      <a href={{makeDocLink hit}}>
        {{hit.name}}
      </a>
      {{" "}}
      <small>
        {{hit.module}}
      </small>
      <br />
      <tt>
        {{hit.type}}
      </tt>
    </li>
  }}

/--
Suggestion item component
-/
def suggestionComponent (suggestion: String) : Html :=
  {{
    <li>
      "🔍"
      <a href={{makeLocalSearchLink suggestion}}>
        <code> {{suggestion}} </code>
      </a>
    </li>
  }}

/--
Suggestions section component
-/
def suggestionsComponent (suggestions: Array String) : Html :=
  let suggestionItems := suggestions.map suggestionComponent
  {{
    <h2>{{"Did you maybe mean"}}</h2>
    <ul> [[[ suggestionItems ]]] </ul>
  }}

/--
Main HTTP request handler that routes requests to appropriate endpoints
-/
def handleRequest (request : Request Body) : LoogleM (Response Body) := do
  let context ← read

  match (request.head.method, toString request.head.uri.path) with
  | (.get, "/") => do
    let query := request.head.uri.query.find? "q" |>.getD ""
    let isLucky := request.head.uri.query.find? "lucky" |>.isSome

    trackClient context request.head.headers false

    let result : Except BackendFailureReason SearchResult ← do
      if query.isEmpty then
        pure (Except.ok (SearchResult.mk none 0 none "" 0 #[]))
      else
        let normalizedQuery := query.replace " " " "
        context.database.query normalizedQuery

    -- Handle "I'm Feeling Lucky" redirect
    if isLucky then
      match result with
      | .ok data =>
        if data.hits.size > 0 then
          return Response.new
            |>.status .found
            |>.header! "Location" (makeDocLink data.hits[0]!)
            |>.body ByteArray.empty
      | _ => pure ()

    -- Build HTML response
    let htmlContent :=
      let errorSection := match result with
        | .ok data =>
          if let some err := data.error then
            errorComponent err
          else
            Html.empty
        | .error reason => errorComponent (toString reason)

      let headerSection := match result with
        | .ok data =>
          if data.header.isEmpty then Html.empty
          else headerComponent data.header
        | _ => Html.empty

      let hitsSection := match result with
        | .ok data =>
          if data.hits.isEmpty then Html.empty
          else {{
            <ul>
              [[[ data.hits.map hitComponent ]]]
            </ul>
          }}
        | _ => Html.empty

      let suggestionsSection := match result with
        | .ok data =>
          if let some suggestions := data.suggestions then
            suggestionsComponent suggestions
          else
            Html.empty
        | _ => Html.empty

      {{
        <html lang="en">
          <head>
            <meta charset="utf-8" />
            <meta name="viewport" content="width=device-width, initial-scale=1" />
                <link rel="stylesheet"
                      href="https://unpkg.com/chota@0.9.2/dist/chota.min.css"
                      integrity="sha384-A2UBIkgVTcNWgv+snhw7PKvU/L9N0JqHwgwDwyNcbsLiVhGG5KAuR64N4wuDYd99"
                      crossorigin="anonymous" />
                <link
                    rel="modulepreload"
                    href="https://cdn.skypack.dev/pin/@leanprover/unicode-input-component@v0.1.4-26sLE5METYYO2xtxbFPA/optimized/@leanprover/unicode-input-component.js"
                    crossorigin="anonymous"
                  />

                <link rel="modulepreload"
                      href="https://cdn.skypack.dev/pin/@leanprover/unicode-input-component@v0.1.4-26sLE5METYYO2xtxbFPA/mode=raw,min/optimized/@leanprover/unicode-input-component.js"
                      integrity="sha384-IA8mae633t0WQFKjWyfKqeVMEsBqJX0L9+qTk7wAslS+h6NI5CkEX7rYNjV7Etmo"
                      crossorigin="anonymous"/>
            <style>
              {{
                "@import url('https://cdnjs.cloudflare.com/ajax/libs/juliamono/0.051/juliamono.css');
                :root {
                  --font-family-mono: 'JuliaMono', monospace;
                }
                .textinput { white-space: -moz-pre-space; }
                .textinput {
                  font-family: inherit;
                  padding: 0.8rem 1rem;
                  border-radius: 4px;
                  border: 1px solid var(--color-lightGrey);
                  font-size: 1em;
                  -webkit-transition: all 0.2s ease;
                  transition: all 0.2s ease;
                  display: block;
                  width: 100%;
                }
                .textinput:focus {
                  outline: none;
                  border-color: var(--color-primary);
                  box-shadow: 0 0 1px var(--color-primary);
                }
                span.copy { cursor: pointer; }"
              }}
            </style>
            <link rel="icon" type="image/png" href="loogle.png" />
            <meta name="twitter:card" content="summary_large_image" />
            <meta name="twitter:title" content="Loogle - Search Lean and Mathlib" />
            <meta name="twitter:description" content="Loogle is a search tool for finding definitions, theorems, and lemmas in Lean 4 and Mathlib." />
            <meta name="twitter:image" content="https://loogle.lean-lang.org/loogle-banner.png" />
            <meta property="og:title" content="Loogle - Search Lean and Mathlib" />
            <meta property="og:description" content="Loogle is a search tool for finding definitions, theorems, and lemmas in Lean 4 and Mathlib." />
            <meta property="og:image" content="https://loogle.lean-lang.org/loogle-banner.png" />
            <meta property="og:url" content="https://loogle.lean-lang.org/" />
            <title>"Loogle!"</title>
          </head>
          <body autocomplete="off" autocorrect="off" autocapitalize="off" spellcheck="false">
            <main className="container">
              <section>
                <h1><a href="." style="color:#333;">"Loogle!"</a></h1>
                <form method="GET" id="queryform">
                  <div className="grouped">
                    <input id="hiddenquery" type="hidden" name="q" value=""/>
                    <div className="textinput" id="query" name="q" contenteditable="true" autofocus="true" autocorrect="false">{{query}}</div>
                    <button type="submit" id="submit">"#find"</button>
                    <button type="submit" name="lucky" value="yes" title="Directly jump to the documentation of the first hit.">"#lucky"</button>
                  </div>
                </form>
              </section>
              {{ errorSection }}
              {{ headerSection }}
              {{ hitsSection }}
              {{ suggestionsSection }}
              <p><small>"This is Loogle serving Lean and Mathlib"</small></p>
            </main>
            <script type="module">
              {{
                .text false "import \"@leanprover/unicode-input-component\";
                import { InputAbbreviationRewriter } from \"https://cdn.skypack.dev/pin/@leanprover/unicode-input-component@v0.1.0-cAcOWoqAnOWevp4vHscs/mode=imports,min/optimized/@leanprover/unicode-input-component.js\";
                const queryInput = document.getElementById('query');
                const hiddenInput = document.getElementById('hiddenquery');
                const form = document.getElementById('queryform');
                const submitButton = document.getElementById('submit');
                const rewriter = new InputAbbreviationRewriter(
                  { abbreviationCharacter: \"\\\\\\\\\",
                    customTranslations: [],
                    eagerReplacementEnabled: true },
                  queryInput,
                )
                queryInput.addEventListener('keydown', event => {
                  if (event.key === 'Enter') {
                    event.preventDefault();
                    submitButton.click();
                  }
                })
                form.addEventListener('submit', event => {
                  hiddenInput.value = queryInput.innerText;
                })
                document.querySelectorAll('span.copy').forEach(element => {
                  element.addEventListener('click', () => {
                    navigator.clipboard.writeText(element.getAttribute('data-text'));
                  });
                });"
              }}
            </script>
          </body>
        </html>
      }}

    dbg_trace "sending"
    return Response.ok
      |>.header! "Content-type" "text/html"
      |>.header! "Access-Control-Allow-Origin" "*"
      |>.header! "Access-Control-Allow-Methods" "GET, POST, OPTIONS"
      |>.header! "Access-Control-Allow-Headers" "Content-Type"
      |>.body ("<!DOCTYPE html>\n" ++ htmlContent.asString)

  | (.get, "/json") => do
    let query := request.head.uri.query.find? "q" |>.getD ""
    let isLucky := request.head.uri.query.find? "lucky" |>.isSome

    trackClient context request.head.headers true

    let result : Except BackendFailureReason SearchResult ← do
      if query.isEmpty then
        pure (Except.ok (SearchResult.mk none 0 none "" 0 #[]))
      else
        let normalizedQuery := query.replace " " " "
        context.database.query normalizedQuery

    match result with
    | .ok data =>
      if isLucky && data.hits.size > 0 then
        return Response.new
          |>.status .found
          |>.header! "Location" (makeDocLink data.hits[0]!)
          |>.header! "Access-Control-Allow-Origin" "*"
          |>.header! "Access-Control-Allow-Headers" "User-Agent, X-Loogle-Client"
          |>.body ByteArray.empty
      else
        return Response.ok
          |>.header! "Content-type" "application/json"
          |>.header! "Access-Control-Allow-Origin" "*"
          |>.header! "Access-Control-Allow-Headers" "User-Agent, X-Loogle-Client"
          |>.body (toString <| toJson data)
    | .error reason =>
      let errorData : SearchResult := { error := some (toString reason) }
      return Response.ok
        |>.header! "Content-type" "application/json"
        |>.header! "Access-Control-Allow-Origin" "*"
        |>.header! "Access-Control-Allow-Headers" "User-Agent, X-Loogle-Client"
        |>.body (toString <| toJson errorData)

  | (.get, "/loogle.png") =>
    return Response.ok
      |>.header! "Content-type" "image/png"
      |>.header! "Access-Control-Allow-Origin" "*"
      |>.header! "Access-Control-Allow-Headers" "User-Agent, X-Loogle-Client"
      |>.body (← context.getAsset "loogle.png")

  | (.get, "/loogle-banner.png") => do
    return Response.ok
      |>.header! "Content-type" "image/png"
      |>.header! "Access-Control-Allow-Origin" "*"
      |>.header! "Access-Control-Allow-Headers" "User-Agent, X-Loogle-Client"
      |>.body (← context.getAsset "loogle-banner.png")

  | (.get, "/metrics") =>
    return Response.ok
      |>.header! "Content-type" "text/plain"
      |>.text (← context.metrics.render)

  | (.post, "/zulipbot") => do
    context.metrics.incrementLabel "clients" "zulip"

    let some body ← request.body.collectString (some 123456)
      | return Response.badRequest |>.text "Invalid request."

    let .ok json := Json.parse body
      | return Response.badRequest |>.text "Invalid JSON."

    let .ok messageData := json.getObjVal? "data"
      | return Response.badRequest |>.text "Missing data field."

    let (Json.str data) := messageData
      | return Response.badRequest |>.text "Invalid data field."

    -- Extract query from Zulip message
    let query := if let some matc := (data.splitOn "@**loogle**")[1]? then
      matc.trimAscii.dropPrefix ":" |>.dropPrefix "?" |>.trimAscii |>.toString
    else
      data.splitOn "\n" |>.head!

    let result ← Database.query context.database query

    let reply := match result with
    | .ok data =>
      if let some error := data.error then
        let baseReply := if error.contains "\n" then
          s!"❗\n```\n{error}\n```"
        else
          s!"❗ {error}"

        match data.suggestions with
        | none => baseReply
        | some suggestions =>
          let suggestionText :=
            if suggestions.size == 1 then
              s!"Did you mean {formatZulipQueryLink suggestions[0]!}?"
            else if suggestions.size == 2 then
              s!"Did you mean {formatZulipQueryLink suggestions[0]!} or {formatZulipQueryLink suggestions[1]!}?"
            else
              s!"Did you mean {formatZulipQueryLink suggestions[0]!}, {formatZulipQueryLink suggestions[1]!}, or [something else]({makeSearchLink query})?"
          baseReply ++ "\n" ++ suggestionText
      else
        let hits := data.hits
        if hits.size == 0 then
          "🤷 nothing found"
        else if hits.size == 1 then
          s!"🔍 {formatZulipHit hits[0]!}"
        else if hits.size == 2 then
          s!"🔍 {formatZulipHit hits[0]!}, {formatZulipHit hits[1]!}"
        else
          let remainingCount := data.count - 2
          s!"🔍 {formatZulipHit hits[0]!}, {formatZulipHit hits[1]!}, and [{remainingCount} more]({makeSearchLink query})"
    | .error reason =>
      s!"❗ {toString reason}"

    let responseJson := Json.mkObj [("content", Json.str reply)]
    return Response.ok
      |>.header! "Content-type" "application/json"
      |>.body (toString responseJson)

  | (.options, "/json") =>
    return Response.ok
      |>.header! "Content-type" "application/json"
      |>.header! "Access-Control-Allow-Origin" "*"
      |>.header! "Access-Control-Allow-Methods" "GET"
      |>.header! "Access-Control-Allow-Headers" "User-Agent, X-Loogle-Client"
      |>.body ByteArray.empty

  | (_, _) =>
    return Response.notFound |>.text "Not found."

end Loogle

/--
Application entry point that starts the HTTP server and handles graceful shutdown
-/
def main : IO Unit := Async.block do
  let serverAddress := SocketAddressV4.mk (.ofParts 0 0 0 0) 8080

  -- Load static assets
  let assets := HashMap.emptyWithCapacity
  let assets := assets.insert "loogle.png" (← IO.FS.readBinFile "./assets/loogle.png")
  let assets := assets.insert "loogle-banner.png" (← IO.FS.readBinFile "./assets/loogle-banner.png")

  -- Initialize application context
  let context ← Loogle.Context.mk
    <$> Loogle.Metrics.init
    <*> Std.Mutex.new assets
    <*> Loogle.Database.start

  -- Set version information in metrics
  context.metrics.setLabel "versions" "loogle" (← Loogle.getGitRevision)
  context.metrics.setLabel "versions" "mathlib" (← Loogle.getMathlibRevision)

  dbg_trace "[database] starting"
  let start ← context.database.waitForReady

  if ¬start then
    throw (.userError "cannot start database")

  dbg_trace "[database] started"

  -- Start the HTTP server
  let server ← Server.serve serverAddress (fun req => Loogle.handleRequest req context)
  IO.println "[server] ready!"

  let sigintWaiter ← Signal.Waiter.mk Signal.sigint true

  while true do
    let shouldCancel ← Selectable.one #[
      .case server.waitShutdownSelector fun _ => pure false,
      .case sigintWaiter.selector fun _ => pure true,
    ]

    if shouldCancel then
      IO.println "[server] shutting down!"
      server.shutdownAndWait
    else
      break
