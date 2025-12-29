import Server.Static
import Server.Worker
import Server.Metrics
import Lean.Data.Json
import Std.Internal.Async

open Lean
open Std Internal IO Async

structure Context where
  metrics: IO.Ref Metrics
  static : Static
  chan: Chan
  loogleRev: String
  mathlibRev: String

abbrev Handler (α: Type) := ReaderT Context ContextAsync α

def Handler.metrics : Handler (IO.Ref Metrics) := do
  let ctx ← ReaderT.read
  return ctx.metrics

def Handler.get (query: Context → α) : Handler α := do
  let ctx ← ReaderT.read
  return query ctx

def Handler.request (query: String) : Handler (Except String Data) := do
  let ctx ← ReaderT.read
  let data ← ctx.chan.request query
  return data

def Handler.static! (name: String) : Handler ByteArray := do
  let ctx ← ReaderT.read
  return ctx.static.data.get! name

def getGitRevision : IO String := do
  let output ← IO.Process.output {cmd := "git rev-parse HEAD"}
  if output.exitCode = 0
    then pure output.stdout.trimAscii.toString
    else pure "UNKNOWN"

def getMathlibRevision : IO String := do
  let rev2 := "UNKNOWN"
  try
    let manifestContent ← IO.FS.readFile "./lake-manifest.json"

    let manifest ←
      match Json.parse manifestContent with
      | .ok res => pure res
      | .error _ => throw $ IO.userError "cannot parse mathlib json"

    match manifest.getObjVal? "packages" with
    | .ok (Json.arr packages) =>
      let mathlibPackage := packages.find? fun p =>
        match p.getObjVal? "name" with
        | .ok (Json.str "mathlib") => true
        | _ => false

      match mathlibPackage with
      | none => pure rev2
      | some pkg =>
        match pkg.getObjVal? "rev" with
        | .ok (Json.str rev) => pure rev
        | _ => pure rev2
    | _ => return rev2
  catch _ =>
    pure rev2
