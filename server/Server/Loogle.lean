import Lean

open IO

namespace Server.Loogle

inductive Reason
  | noGreeting
  | died (exitCode: UInt32)
  | triedToEscape
  | noResponse

inductive Result
  | ok (s: Lean.Json)
  | killed (reason: Reason)

instance : ToString Reason where
  toString
    | Reason.noGreeting => "No greeting received"
    | Reason.died exitCode => s!"Backend died with exit code {exitCode}"
    | Reason.triedToEscape => "Backend died trying to escape the sandbox"
    | Reason.noResponse => "No response from backend"

instance : ToString Result where
  toString
    | Result.ok s => s!"Success: {s}"
    | Result.killed reason => s!"Killed: {reason}"

def config : Process.SpawnArgs := {
    cmd := ".lake/build/bin/loogle",
    args := #["--json", "--module", "Init", "--interactive"],
    stdin := .piped,
    stdout := .piped
}

structure Loogle where
  child : Process.Child config.toStdioConfig
  starting : Bool

def startLoogle : IO Loogle := do
  let child ← Process.spawn config
  pure { child := child, starting := true }

def restartLoogle (loogleRef : IO.Ref Loogle) : IO Unit := do
  let loogle ← loogleRef.get
  loogle.child.kill
  let loogle ← startLoogle
  loogleRef.set loogle

def waitStart (loogleRef : IO.Ref Loogle) : IO Bool := do
  let loogle ← loogleRef.get
  let available ← loogle.child.stdout.getLine
  if available = "Loogle is ready.\n" then
    let _ ← loogleRef.modify (fun l => { l with starting := false })
    pure true
  else
    pure false

def query (loogleRef : IO.Ref Loogle) (query : String) : IO Result := do
  let loogle ← loogleRef.get
  if loogle.starting then
    pure $ .killed .noGreeting
  else
    try
      loogle.child.stdin.write (String.toUTF8 $ query ++ "\n")
      loogle.child.stdin.flush
      let outputJson ← loogle.child.stdout.getLine
      match Lean.Json.parse outputJson with
      | Except.ok output => pure <| .ok output
      | Except.error e => throw <| IO.userError s!"JSON decode error: {e}"
    catch e =>
      IO.sleep 5000
      let exitCode ← loogle.child.wait
      if exitCode == 255 then
        restartLoogle loogleRef
        IO.eprintln "Backend died trying to escape the sandbox."
        pure $ .killed .triedToEscape
      else if exitCode != 0 then
        restartLoogle loogleRef
        IO.eprintln s!"Backend died with code {exitCode}."
        pure $ .killed $ .died exitCode
      else
        IO.eprintln s!"Backend did not respond ({e})."
        loogle.child.kill
        restartLoogle loogleRef
        pure $ .killed .noResponse
