import Lean
import Init.System.IO
import Lean.Data.Json
import Std.Internal.Parsec
import Std.Data.HashMap

open Lean Std

namespace Loogle

/-
A wrapper around HashMap with convenient operations for managing key-value pairs with efficient
insertion and modification.
-/
structure Map (labels : List String) (v: Type) where
  inner: HashMap String v
  proof : ∀l, l ∈ labels → l ∈ inner

namespace Map

/-
Modify the value associated with a key by applying a function to it. Requires an Inhabited instance
for the value type as a default.
-/
def modify (m: Map lbls v) (key: String) (f: v → v) : Map lbls v :=
  { inner := m.inner.modify key f, proof := fun f => fun l => HashMap.mem_modify.mpr (m.proof f l) }

/-
Create an empty map with default capacity.
-/
def empty (default : v) : Map lbl v :=
  let f := HashMap.ofList (lbl.map (·, default))
  have mem (k : String) (p : k ∈ lbl) : k ∈ f := HashMap.mem_ofList.mpr (by simp; exact p)
  ⟨f, mem⟩

/-
Increment a numeric value in the map by 1. If the key doesn't exist, it will be initialized to the
default value first.
-/
def increment [OfNat v (nat_lit 1)] [Inhabited v] [Add v] (m: Map lbls v) (key: String) : Map lbls v :=
  modify m key (· + 1)

end Map

/-
Histogram for tracking value distributions across predefined buckets. Values are categorized into
buckets based on upper bounds.
-/
structure Histogram (buckets : List Nat) where
  counts : Array Nat
  deriving Repr

namespace Histogram

/-
Create an empty histogram with the given bucket boundaries.
Initializes all counts to zero.
-/
def empty (buckets : List Nat) : Histogram buckets :=
  ⟨Array.replicate (buckets.length + 1) 0⟩

/-
Find the index of the appropriate bucket for a given value. Returns the index of the first bucket
whose upper bound is >= value, or the overflow bucket index if value exceeds all bounds.
-/
def findBucketIndex (buckets : List Nat) (value : Nat) : Nat :=
  Id.run do
    for (i, b) in buckets.mapIdx (·, ·) do
      if value ≤ b then
        return i
    return buckets.length

/-
Add a value to the histogram by incrementing the appropriate bucket.
-/
def add (h : Histogram buckets) (bucket : Nat) : Histogram buckets :=
  let idx := findBucketIndex buckets bucket
  { h with counts := h.counts.modify idx (· + 1) }

/-
Get the number of buckets (excluding the overflow bucket).
-/
def size (h: Histogram buckets) : Nat :=
  h.counts.size - 1

/-
Calculate the total sum of all counts across all buckets.
-/
def sum (h: Histogram buckets) : Nat :=
  h.counts.foldl (· + ·) 0

instance : ToJson (Histogram buckets) where
  toJson hist :=
    Json.mkObj [
      ("buckets", toJson buckets),
      ("counts", toJson hist.counts)
    ]

instance : FromJson (Σ buckets, Histogram buckets) where
  fromJson? obj := do
    let buckets ← obj.getObjValAs? (List Nat) "buckets"
    let counts ← obj.getObjValAs? (Array Nat) "counts"
    return ⟨buckets, Histogram.mk counts⟩

end Histogram

/-
Counter for simple incrementing values.
-/
structure Counter where
  value : Nat
  deriving Repr

namespace Counter

/-
Create a counter initialized to zero.
-/
def zero : Counter := ⟨0⟩

/-
Increment the counter by 1.
-/
def increment (c : Counter) : Counter :=
  { value := c.value + 1 }

instance : ToJson Counter where
  toJson c := toJson c.value

instance : FromJson Counter where
  fromJson? json := do
    let val ← fromJson? json
    return Counter.mk val

end Counter

inductive Typ
  | string
  | nat
deriving Repr, BEq, Hashable

abbrev Typ.toType (x : Typ) : Type :=
  match x with
  | string => String
  | nat => Nat


/-
Graph type enumeration defining the available metric types.
-/
inductive GraphType where
  | histogram (buckets : List Nat) : GraphType
  | counter : GraphType
  | map (buckets : List String) (typ : Typ) : GraphType
  deriving Repr, BEq, Hashable

namespace GraphType

/-
Convert a GraphType to its corresponding Lean type.
-/
def toType : GraphType → Type
  | histogram buckets => Histogram buckets
  | counter => Counter
  | map labels ty => Map labels ty.toType

def init : (x : GraphType) → x.toType
  | histogram buckets => Histogram.empty buckets
  | counter => Counter.zero
  | map _ .nat => Map.empty (by unfold Typ.toType; exact 0)
  | map _ .string => Map.empty (by unfold Typ.toType; exact "")

instance : ToString GraphType where
  toString
    | histogram _ => "histogram"
    | counter => "counter"
    | map _ _ => "map"

instance : ToJson GraphType where
  toJson gt := Json.str (toString gt)

end GraphType

/-
Graph metadata and data bundled together. Combines a description with typed data based on the graph type.
-/
structure Graph (name : String) (gt : GraphType) where
  description : String
  data : gt.toType

/-
Apply a function to modify the data within a graph.
-/
def Graph.modify (graph : Graph name gt) (fn : gt.toType → gt.toType) : Graph name gt :=
  { description := graph.description, data := fn graph.data }

namespace Graph

/-
Format a graph as a Prometheus-compatible metric string. Handles histogram, counter, and map types
with appropriate formatting.
-/
def format (g : Graph name gt) : String :=
  match gt with
  | GraphType.histogram buckets =>
    let h := g.data
    let bucketStrings := buckets.mapIdx fun idx count =>
      let idxC := h.counts[idx]!
      s!"{name}_bucket" ++ "{{" ++ s!"le=\"{count}\"" ++ "}}" ++ s!"{idxC}"
    let overflowString := s!"{name}_bucket" ++ "{{le=\"+Inf\"}}" ++ toString h.counts[buckets.length]!
    let bucketsFormatted := String.intercalate "\n" (bucketStrings ++ [overflowString])
    s!"# HELP {name} {g.description}\n# TYPE {name} histogram\n{bucketsFormatted}\n" ++
    s!"{name}_count {h.size}\n" ++
    s!"{name}_sum {h.sum}"
  | GraphType.counter =>
    let c := g.data
    s!"# HELP {name} {g.description}\n# TYPE {name} counter\n{name}_total {c.value}"
  | GraphType.map _ .nat =>
    let m := g.data
    let entries := m.inner.toArray
      |>.map (fun (k, v) => s!"{name}_total" ++ "{{" ++ s!"label=\"{k}\"" ++ "}}" ++ s!"{v}")
      |>.toList
      |> String.intercalate "\n"
    s!"# HELP {name} {g.description}\n# TYPE {name} counter\n{entries}"
  | GraphType.map _ .string =>
    let m := g.data
    let entries := m.inner.toArray
      |>.map (fun (k, v) => s!"{name}_total" ++ "{{" ++ s!"label=\"{k}\"" ++ "}}" ++ s!"{v}")
      |>.toList
      |> String.intercalate "\n"
    s!"# HELP {name} {g.description}\n# TYPE {name} counter\n{entries}"

instance : ToString (Graph name gt) where
  toString := format

end Graph

/-
Specification for a graph, containing its name and type.
-/
structure GraphSpec where
  name : String
  graphType : GraphType

/-
Heterogeneous list of graphs with their types tracked at the type level.
-/
inductive HList : (t : List (String × GraphType)) → Type where
  | nil : HList []
  | cons (head : Graph name x) (tail : HList xs) : HList ((name, x) :: xs)

/-
Type class for proving that a named graph of a specific type exists in a list.
-/
class inductive HList.In (ls : String) (x : outParam GraphType) : (l : List (String × GraphType)) → Type where
  | exactly : In ls x ((ls, x) :: tail)
  | post (i : In ls x tail) : In ls x (y :: tail)

instance [i : HList.In s x tl] : HList.In s x (y :: tl) := i.post
instance : HList.In s x ((s, x) :: tl) := HList.In.exactly

class inductive In (ls : String) : (l : List String) → Type where
  | exactly : In ls (ls :: tail)
  | post (i : In ls tail) : In ls (y :: tail)

instance [i : In s tl] : In s (y :: tl) := i.post
instance : In s (s :: tl) := In.exactly

/-
Find a graph by name in a heterogeneous list. The type checker ensures the graph exists at compile time.
-/
def HList.find (h : HList l) (name : String) [place : In name s l] : Graph name s :=
  match l, place, h with
  | _ :: _, .exactly, .cons h _ => h
  | _ :: _, .post h1, .cons _ t => find t name (place := h1)

/-
Update a graph in a heterogeneous list by applying a function to it. The type checker ensures the
graph exists and has the correct type.
-/
def HList.update (h : HList l) (name : String) [place : In name ty l] (fn: Graph name ty → Graph name ty) : HList l :=
  match l, place, h with
  | _ :: _, .exactly, .cons h t => .cons (fn h) t
  | _ :: _, .post h1, .cons h t => .cons h (update t name (place := h1) fn)

/-
Dependent type for metrics state based on a list of graph specifications. Stores version information
and a heterogeneous list of graphs.
-/
structure Metrics.State (specs : List (String × GraphType)) where
  graphs : HList specs

/-
Thread-safe metrics container with atomic operations.
-/
structure Metrics (specs : List (String × GraphType)) where
  inner : Std.Mutex (Metrics.State specs)

namespace Metrics

def initHList : HList lbls :=
  match lbls with
  | [] => HList.nil
  | (name, typ) :: _ => HList.cons (Graph.mk name typ.init) (initHList)

def init : IO (Metrics lbls) := do
  return Metrics.mk (← Std.Mutex.new (State.mk initHList))

/-
Atomically increment a counter metric by 1.
-/
def increment (metrics : Metrics ls) (name : String) [HList.In name .counter ls] : IO Unit := do
  metrics.inner.atomically do
    modify (fun s => { s with graphs := s.graphs.update (ty := .counter) name (Graph.modify · Counter.increment) })

/-
Atomically add a value to a histogram metric. The value is placed in the appropriate bucket based on
the histogram's boundaries.
-/
def addValue (metrics : Metrics ls) (name : String) [HList.In name (.histogram e) ls] (value : Nat) : IO Unit := do
  metrics.inner.atomically do
    modify (fun s => { s with graphs := s.graphs.update (ty := .histogram e) name (Graph.modify · (Histogram.add · value)) })

/-
Atomically increment a labeled counter in a map metric. Creates the label with value 0 if it doesn't
exist before incrementing.
-/
def incrementLabel (metrics : Metrics ls) (name : String) [HList.In name (.map lbls .nat) ls] (label : String) : IO Unit := do
  metrics.inner.atomically do
    modify (fun s => { s with graphs := s.graphs.update (ty := (.map lbls .nat)) name (Graph.modify · (Map.increment · label)) })

/-
Atomically set a specific value for a labeled counter in a map metric.
-/
def setLabel (metrics : Metrics ls) (name : String) [HList.In name (.map lbls ty) ls] (label : String) (value : ty.toType) [In label lbls] : IO Unit := do
  metrics.inner.atomically do
    modify (fun s => { s with graphs := s.graphs.update (ty := (.map lbls ty)) name (Graph.modify · (Map.modify · label (fun _ => value))) })

/-
Render all metrics to Prometheus text exposition format.
Formats each graph according to its type (histogram, counter, map).
-/
def render (metrics : Metrics ls) : IO String := do
  let state ← metrics.inner.atomically get
  return renderHList state.graphs

where
  renderHList {graphs : List (String × GraphType)} : HList graphs → String
    | HList.nil => ""
    | HList.cons head tail =>
      let headStr := Graph.format head
      let tailStr := renderHList tail
      if tailStr.isEmpty then
        headStr
      else
        headStr ++ "\n" ++ tailStr

end Loogle.Metrics
