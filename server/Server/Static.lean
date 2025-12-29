import Std.Data.HashMap

open Std
open Lean

structure Static where
  data: HashMap String ByteArray
  deriving Inhabited

def Static.load (static: Static) (name: String) (file: String) : IO Static := do
  let data ← IO.FS.readBinFile file
  return { data := static.data.insert name data }

def Static.ofFiles (files: Array (String × String)) : IO Static := do
  let mut static : Static := Inhabited.default

  for (name, path) in files do
    static ← static.load name path

  return static
