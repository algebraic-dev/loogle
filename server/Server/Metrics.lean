import Lean
import Init.System.IO
import Std.Internal.Parsec
import Lean.Data.Json

open Std
open Lean

structure Map (k: Type) (v: Type) [BEq k] [Hashable k] where
  map: HashMap k v

def Map.add [BEq k] [Hashable k] (map: Map k β) (key: k) (v: β) : Map k β :=
  { map := map.map.insert key v }

def Map.modify [Inhabited β] [BEq k] [Hashable k] (map: Map k β) (key: k) (v: β → β) : Map k β :=
  { map := map.map.insert key (v $ map.map.get! key) }

def Map.empty [BEq k] [Hashable k] : Map k β :=
  { map := HashMap.emptyWithCapacity }

def Map.fromLabels [Inhabited α] (labels: List String) : Map String α :=
  labels.foldl (Map.add · · Inhabited.default) Map.empty

instance [ToJson k] [ToJson v] [BEq k] [Hashable k] : ToJson (Map k v) where
  toJson m :=
    let entries := m.map.toList.map (λ (k, v) => (toString (toJson k), toJson v))
    Json.mkObj entries

instance [FromJson k] [FromJson v] [BEq k] [Hashable k] : FromJson (Map k v) where
  fromJson? json := do
    let obj ← json.getObj?
    let entries ← obj.toArray.mapM (λ ⟨k, v⟩ => do
      let key ← fromJson? (Json.str k)
      let value ← fromJson? v
      pure (key, value))
    let hashMap := HashMap.ofList entries.toList
    pure { map := hashMap }

structure Histogram (name: String) where
  buckets : Array Nat
  counts : Array Nat
  deriving Repr

instance : ToJson (Histogram name) where
  toJson hist :=
    Json.mkObj [
      ("name", Json.str name),
      ("buckets", toJson hist.buckets),
      ("counts", toJson hist.counts)
    ]

instance : FromJson (Histogram name) where
  fromJson? obj := do
    let buckets ← obj.getObjValAs? (Array Nat) "buckets"
    let counts ← obj.getObjValAs?  (Array Nat) "counts"
    pure { buckets, counts }

namespace Histogram

def empty (buckets : Array Nat) : Histogram name :=
  ⟨buckets, Array.ofFn (n := buckets.size + 1) (fun _ => 0)⟩

def findBucketIndex (buckets : Array Nat) (value : Nat) : Nat :=
  Id.run do
    for (i, b) in buckets.mapIdx (·, ·) do
      if value ≤ b then
        return i
    return buckets.size

def add (h : Histogram α) (value : Nat) : Histogram α :=
  let idx := findBucketIndex h.buckets value
  { h with counts := h.counts.modify idx (· + 1) }

def getCounts (h : Histogram α) : Array (Nat × Nat) :=
  h.buckets.zip h.counts

def size (h: Histogram α) : Nat := h.counts.size - 1

def formatHistogram (h : Histogram name) : String :=
  let bucketStrings := h.buckets.mapIdx (λ idx count => s!"{name}_bucket\{\{le=\"{count}\"}} {h.counts[idx]!}")
  let overflowString := s!"{name}_bucket\{\{le=\"+Inf\"}} {h.counts[h.buckets.size]!}"
  String.intercalate "\n" ((bucketStrings.push overflowString |>.toList))

instance : ToString (Histogram name) where
  toString := formatHistogram

end Histogram

structure Metrics where
  versions : String × String
  queries : Nat
  errors : Nat
  results : Histogram "results"
  heartbeats : Histogram "heartbeats"
  clients : Map String Nat
  deriving ToJson, FromJson

instance : ToString Metrics where
  toString metrics :=
    let versionsInfo := s!"versions_info\{\{loogle=\"{metrics.versions.fst}\",mathlib={metrics.versions.snd}\"}} 1.0"
    let queriesTotal := s!"queries_total {metrics.queries}"
    let errorsTotal := s!"errors_total {metrics.errors}"

    let resultsBuckets := toString metrics.results
    let heartbeatsBuckets := toString metrics.heartbeats

    let clientsTotal
      := metrics.clients.map.toArray
      |>.map (λ (client, count) => s!"clients_total\{\{client=\"{client}\"}} {count}")
      |>.toList
      |> String.intercalate "\n"

    s!"# HELP versions_info Lean and mathlib versions\n# TYPE versions_info gauge\n{versionsInfo}\n\n" ++
    s!"# HELP queries_total Total number of queries\n# TYPE queries_total counter\n{queriesTotal}\n\n" ++
    s!"# HELP errors_total Total number of failing queries\n# TYPE errors_total counter\n{errorsTotal}\n\n" ++
    s!"# HELP results Results per query\n# TYPE results histogram\n{resultsBuckets}\n" ++
    s!"results_count {metrics.results.size}\n" ++
    s!"results_sum {metrics.results.counts.foldl (· + ·) 0}\n\n" ++
    s!"# HELP heartbeats Heartbeats per query\n# TYPE heartbeats histogram\n{heartbeatsBuckets}\n" ++
    s!"heartbeats_count {metrics.heartbeats.size}\n" ++
    s!"heartbeats_sum {metrics.heartbeats.counts.foldl (· + ·) 0}\n\n" ++
    s!"# HELP clients_total Clients used\n# TYPE clients_total counter\n{clientsTotal}\n"

def initializeMetrics (loogleVersion: String) (mathlibVersion: String) : IO (IO.Ref Metrics) := do
  let versions := (loogleVersion, mathlibVersion)
  let resultsBuckets := Histogram.empty #[0, 1, 2, 5, 10, 50, 100, 200, 500, 1000]
  let heartbeatsBuckets := Histogram.empty #[0, 2, 20, 200, 2000, 20000]
  let clients := Map.fromLabels ["web", "zulip", "json", "nvim", "vscode"]

  let metrics := Metrics.mk versions 0 0 resultsBuckets heartbeatsBuckets clients
  IO.mkRef metrics

namespace Metrics

def updateQueries (metricsRef : IO.Ref Metrics) : IO Unit := do
  let metrics <- metricsRef.get
  metricsRef.set { metrics with queries := metrics.queries + 1 }

def updateErrors (metricsRef : IO.Ref Metrics) : IO Unit := do
  let metrics <- metricsRef.get
  metricsRef.set { metrics with errors := metrics.errors + 1 }

def updateResults (metricsRef : IO.Ref Metrics) (resultCount : Nat) : IO Unit := do
  let metrics <- metricsRef.get
  let results := metrics.results.add resultCount
  metricsRef.set { metrics with results := results }

def updateHeartbeats (metricsRef : IO.Ref Metrics) (heartbeatCount : Nat) : IO Unit := do
  let metrics <- metricsRef.get
  let heartbeats := metrics.heartbeats.add heartbeatCount
  metricsRef.set { metrics with heartbeats := heartbeats }

def updateClients (metricsRef : IO.Ref Metrics) (client : String) : IO Unit := do
  let metrics <- metricsRef.get
  let clients := metrics.clients.modify client (· + 1)
  metricsRef.set { metrics with clients := clients }
