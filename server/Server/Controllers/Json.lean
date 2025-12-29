import Server
import Server.Views.Index
import Server.Query
import Server.Handlers.Metrics
import Std.Internal.Http

namespace Server.Controllers

open Lean
open Std Http Internal IO Async

def jsonPage (conn: Request Body) : Handler (Response Body) := do
  Handler.metricsAddAgent true conn

  let query := queryParam =<< conn.head.uri.query?

  if query.isNone then
    return json (Json.obj ∅)

  let result ← queryData query

  match result with
  | some res => return json (toJson res)
  | none => return internalError
