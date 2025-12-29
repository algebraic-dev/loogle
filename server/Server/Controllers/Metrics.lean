import Std.Internal.Http
import Server
import Server.Views.Index
import Server.Query

namespace Server.Controllers

open Lean
open Std Http Internal IO Async

def metricsPage (_: Request Body) : Handler (Response Body) := do
  let metrics ← Handler.metrics
  return plain (toString (← metrics.get))
