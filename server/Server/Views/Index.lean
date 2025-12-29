import Server.Components.Blurb
import Server.Components.Error
import Server.Components.Header
import Server.Components.Hit
import Server.Components.Suggestions

import Server.Context
import Server.Jsx

namespace Server.Views
open Server.Components
open Server.Jsx
open Lean

private def escape (s: String) : String := s

private def style : String :=
"@import url('https://cdnjs.cloudflare.com/ajax/libs/juliamono/0.051/juliamono.css');
:root {
  --font-family-mono: 'JuliaMono', monospace;
}

/* Browser fix for unicode editing */
.textinput { white-space: -moz-pre-space; }

/* Copied from chota for textinput */
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
}"

private def script : String :=
"import { InputAbbreviationRewriter } from \"https://cdn.skypack.dev/pin/@leanprover/unicode-input-component@v0.1.0-cAcOWoqAnOWevp4vHscs/mode=imports,min/optimized/@leanprover/unicode-input-component.js\";
const queryInput = document.getElementById('query');
const hiddenInput = document.getElementById('hiddenquery');
const form = document.getElementById('queryform');
const submitButton = document.getElementById('submit');
const rewriter = new InputAbbreviationRewriter(
    { abbreviationCharacter: \"\\\\\",
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
})"

private def indexHead : Element :=
  <head>
    <meta charset="utf-8" />
    <meta name="viewport" content="width=device-width, initial-scale=1" />
    <link rel="stylesheet"
            href="https://unpkg.com/chota@0.9.2/dist/chota.min.css"
            integrity="sha384-A2UBIkgVTcNWgv+snhw7PKvU/L9N0JqHwgwDwyNcbsLiVhGG5KAuR64N4wuDYd99"
            crossorigin="anonymous" />
    <link rel="modulepreload"
          href="https://cdn.skypack.dev/-/@leanprover/unicode-input@v0.1.0-8CqsXR89dtD7hIU0c6wN/dist=es2020,mode=imports,min/optimized/@leanprover/unicode-input.js"
          integrity="sha384-IR290YsDJ1+Rzd4fgZM4K+v1E2kG2NzQ3XYgGhSxE4cNodMg0JQ9BWn/0JMgk5Xa"
          crossorigin="anonymous" />
    <link rel="modulepreload"
          href="https://cdn.skypack.dev/-/@leanprover/unicode-input-component@v0.1.0-cAcOWoqAnOWevp4vHscs/dist=es2020,mode=imports,min/optimized/@leanprover/unicode-input-component.js"
          integrity="sha384-yX1xThTPuhw06x65fHJAV8FLLmaKxiyeKkBTGsI5R1dHRQmHbXU0ylBBu2+56Bc/"
          crossorigin="anonymous" />
    <link rel="modulepreload"
          href="https://cdn.skypack.dev/pin/@leanprover/unicode-input-component@v0.1.0-cAcOWoqAnOWevp4vHscs/mode=imports,min/optimized/@leanprover/unicode-input-component.js"
          integrity="sha384-3jdol3AWL3guud0hEWfo7ysqRxI6grINH5ElwPuStNcMb887NNz08TqkGNgy+2q6"
          crossorigin="anonymous" />
    <style> {style} </style>
    <link rel="icon" type="image/png" href="/loogle.png" />
    <title>Loogle!</title>
  </head>

def index (query: Option String) (result: Option Data) (rev1: String) (rev2: String) : Element := Id.run do
  let error := errComponent <$> (Data.error =<< result)
  let header := (headerComponent ∘ Data.header) <$> result
  let hits := ((Data.hits <$> result).getD #[]).map hitComponent
  let suggestions := suggestionsComponent <$> (Data.suggestions =<< result)

  dbg_trace "Str: {String.quote (query.getD "")}"

  pure
    <html lang="en">
      { indexHead }
      <body>
        <main class="container">
          <"section">
            <h1><a href="/" style="color:#333;">Loogle!</a></h1>
            <form method="GET" id="queryform">
              <div class="grouped">
                <input id="hiddenquery" type="hidden" name="q" value=""/>
                <div class="textinput" id="query" name="q" contenteditable="true" autofocus="true">
                  {query.getD ""}
                </div>
                <button type="submit" id="submit">#find</button>
                <button type="submit" name="lucky" value="yes" title="Directly jump to the documentation of the first hit.">#lucky</button>
              </div>
            </form>
          </"section">
          { toFragment error }
          { toFragment header }
          { if !hits.isEmpty then <ul>[hits.map Content.Element]</ul> else <></> }
          { toFragment suggestions }
          {blurbComponent}
          <p>
            <small>
              This is Loogle revision
              <a href="https://github.com/nomeata/loogle/commit/{rev1}">
                <code>{rev1}</code>
              </a>
              serving mathlib revision
              <a href="https://github.com/leanprover-community/mathlib4/commit/{rev2}">
                <code>{rev2}</code>
              </a>
            </small>
          </p>
        </main>
        <script type="module">
          {script}
        </script>
      </body>
    </html>
