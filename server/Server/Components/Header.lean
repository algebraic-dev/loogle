import Server.Jsx

namespace Server.Components
open Server.Jsx

def headerComponent (header: String) : Element :=
  <div>
    <h2>Result</h2>
    <p>{header}</p>
  </div>
