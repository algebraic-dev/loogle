import Lean.Data.Json
import Server.Loogle
import Server.Data

open Lean Parser
open Server.Loogle
open Std Internal IO Async


/-- The Chan type is used to transport requests back and forth -/
structure Chan where
  requestChannel : Std.Channel String
  responseChannel : Std.Channel (Except String Data)

def Chan.new : IO Chan :=
  Chan.mk
  <$> Std.Channel.new
  <*> Std.Channel.new

def Chan.request (channel: Chan) (query: String) : IO (Except String Data) := do
  discard <| channel.1.send query
  channel.2.sync.recv

def runQuery (child : IO.Process.Child Server.Loogle.config.toStdioConfig) (query : String) : IO String := do
  -- Write the query to the process's stdin
  child.stdin.write ((query ++ "\n").toUTF8)
  -- Read the response from the process's stdout
  let response ← child.stdout.readToEnd

  dbg_trace "response: {response}"

  pure response

/--
Runs a Task that will do queries and send to another channel. Its used because queries
only perform well inside the `work` function.
-/
def startWorker : Async Chan := do
  let chan ← Chan.new
  let ready ← IO.Promise.new

  background (prio := .dedicated) $ do
    let loogle ← startLoogle
    let loogleRef ← IO.mkRef loogle
    let result ← waitStart loogleRef

    ready.resolve result

    while true do
      let queryStr ← await (← chan.1.recv)
      let res ← query loogleRef queryStr.trimAscii.toString

      match res with
      | .ok res => do
        dbg_trace "res"
        discard <| discard <| chan.responseChannel.send (FromJson.fromJson? res)
      | .killed res =>
        dbg_trace "KILLED"
        discard <| discard <| chan.responseChannel.send (.error $ toString res)

  IO.println "waiting"
  let answer ← await ready.result!

  unless answer do
    throw $ IO.userError "Cannot start worker process for loogle."

  return chan
