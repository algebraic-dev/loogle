import Server
import Server.Metrics
import Lean.Data.Json
import Std.Internal.Http

import Server.Controllers.Index
import Server.Controllers.Zulip
import Server.Controllers.Metrics
import Server.Controllers.Json

open Lean
open Server.Controllers
open Std Http Internal IO Async

def readError (_: Request Body) (data: Handler (Response Body)) : Handler (Response Body) :=
  try
    data
  catch err => do
    IO.println s!"internal error: {err}"
    return internalError

def router (conn: Request Body) : Handler (Response Body) := do
  dbg_trace "--> {conn.head.method}, {toString conn.head.uri.path?}"
  match (conn.head.method, toString <$> conn.head.uri.path?) with
  | (.get, some "/")           => indexPage conn
  | (.get, some "/json")       => jsonPage conn
  | (.get, some "/metrics")    => metricsPage conn
  | (.post,some "/zulipbot")  => zulipBotPage conn
  | (.get, some "/loogle.png") => return png (← Handler.static! "icon")
  | _ =>

    return notFound

def main : IO Unit := Async.block do
  IO.println "Starting Worker"
  let chan ← startWorker
  IO.println "Starting Server"

  IO.println "Static Gets"
  let staticData ← Static.ofFiles #[("icon", "./assets/loogle.png")]

  IO.println "Getting Git Revision"
  let loogleRev ← getGitRevision

  IO.println "Getting Mathlib Revision"
  let mathlibRev ← getMathlibRevision

  IO.println "Getting Metrics"
  let metrics ← initializeMetrics loogleRev mathlibRev
  let ctx := Context.mk metrics staticData chan loogleRev mathlibRev

  IO.println s!"Starting server {loogleRev} {mathlibRev}"

  let server ← Http.Server.serve
    (addr := Net.SocketAddressV4.mk (.ofParts 0 0 0 0) 8081)
    (fun req => router req ctx)

  server.waitShutdown
