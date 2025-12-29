import Std.Internal.Http
import Server
import Server.Views.Index

open Lean
open Std Http

def queryParam (query: URI.Query) : Option String := do
  query.find? (fun (k, _) => k == "q")
  |>.map ((Option.getD · "") ∘ Prod.snd)

def queryData (query: Option String) : Handler (Option Data) := do
  if let some query := query
    then do
      let metrics ← Handler.metrics
      match ← Handler.request query with
      | .ok res => do
        Metrics.updateHeartbeats metrics res.heartbeats
        Metrics.updateResults metrics res.count
        pure res
      | .error err => do
        dbg_trace "err: {err}"
        Metrics.updateErrors metrics
        pure none
    else pure none
