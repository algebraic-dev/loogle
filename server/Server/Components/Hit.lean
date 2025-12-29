import Server.Jsx
import Server.Context
import Std.Internal.Http

namespace Server.Components
open Server.Jsx
open Std Http

def docLink (hit : Hit) : URI :=
  let name := hit.name.split (· == '.') |>.map toString |>.toArray

  URI.Builder.empty
    |>.setScheme "http"
    |>.setHost "leanprover-community.github.io"
    |>.setPath (name.take (name.size - 1))
    |>.appendPathSegment (name[name.size - 1]! ++ ".html")
    |>.setFragment hit.module
    |>.buildWithDefaults

def hitComponent (hit: Hit) : Element :=
  <li>
    <a href={toString <| docLink hit}>
      {hit.name}
    </a>
    {" "}
    <small>
      {hit.module}
    </small>
    <br />
    <tt>
      {hit.type}
    </tt>
  </li>
