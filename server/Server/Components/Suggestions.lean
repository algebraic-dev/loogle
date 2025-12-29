import Server.Jsx
import Server.Context
import Std.Internal.Http

namespace Server.Components
open Server.Jsx
open Std Http

def localLink (query: String) : String :=
  s!"/?q={URI.encodeURIComponent query}"

def suggestionComponent (sugg: String) : Content :=
  <li>
    "🔍"
    <a href={localLink sugg}>
      <code> {sugg} </code>
    </a>
  </li>

def suggestionsComponent (suggestions: Array String) : Element :=
  let suggs := suggestions.map suggestionComponent
  <>
    <h2>{"Did you maybe mean"}</h2>
    <ul> [suggs] </ul>
  </>
