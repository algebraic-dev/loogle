import Lean.Data.Json
open Lean Parser

structure Hit where
  name: String
  type: String
  module: String
  doc: Option String
deriving ToJson, FromJson, Repr, Inhabited

structure Data where
  error: Option String := none
  heartbeats: Nat := 0
  suggestions: Option (Array String) := none
  header: String := ""
  count: Nat := 0
  hits: Array Hit := #[]
deriving ToJson, FromJson, Inhabited, Repr
