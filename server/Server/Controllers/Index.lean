import Std.Internal.Http
import Server
import Server.Views.Index
import Server.Query
import Server.Handlers.Metrics

namespace Server.Controllers

open Lean
open Std Http Internal IO Async

def indexPage (conn: Request Body) : Handler (Response Body) := do
  let query := queryParam =<< conn.head.uri.query?
  let lucky := luckyParam =<< conn.head.uri.query?

  if query.isSome then
    Handler.metricsAddAgent false conn

  let result ← queryData query

  dbg_trace "--> {repr query}"
  dbg_trace "--> {repr result}"


  -- Handle "I'm feeling lucky" redirect
  if lucky.isSome then
    if let some hit := result.bind (·.hits[0]?) then
      return redirect (Components.docLink hit)

  -- Render the index page
  let data := Server.Views.index
    query
    result
    (← Handler.get (·.loogleRev))
    (← Handler.get (·.mathlibRev))

  return html (String.toUTF8 (toString data))

where
  queryParam (params: URI.Query) : Option String :=
    let raw := params.find? (·.fst == "q") |>.bind (·.snd)
    raw

  luckyParam (params: URI.Query) : Option String :=
    params.find? (·.fst == "lucky") |>.bind (·.snd)
