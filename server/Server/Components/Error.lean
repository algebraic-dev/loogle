import Server.Jsx

namespace Server.Components
open Server.Jsx

def errComponent (err: String) : Element :=
  <div>
    <h2>Error</h2>
    <pre>{err}</pre>
  </div>
