import Server
import Server.Views.Zulip
import Server.Query
import Server.Handlers.Metrics
import Std.Internal.Http

namespace Server.Controllers

open Lean
open Std Http Internal IO Async
open Server.Views

structure ZulipMessage where
  data: String
  deriving ToJson, FromJson


def extractLoogleCommand (message : String) : Option String := do
  let prefix_ := "**loogle**"
  let data := message
  let startIndex := data.find? prefix_
  match startIndex with
  | none => none
  | some i => do
    let startIdx := i.offset.byteIdx + prefix_.length
    let mut commandStart := startIdx

    for idx in [startIdx:data.length] do
      match (String.Pos.Raw.mk idx).get? (data) with
      | some ' ' | some ':' | some '?' | some ',' => continue
      | _ =>
        commandStart := idx
        break

    let command := data.drop commandStart
    if command.isEmpty then none else some command.trimAscii.toString

def zulipBotPage (conn: Request Body) : Handler (Response Body) := do
  -- Validate content type

  let fromHeaders := conn.head.headers.get? (.ofString! "content-type")
  let appJson := HeaderValue.ofString! "application/json"

  if fromHeaders ≠ appJson then
    return badRequest

  let some bodyText ← conn.body.collectString
    | return badRequest

  let .ok bodyJson := Json.parse bodyText
    | return badRequest

  let .ok message := fromJson? bodyJson
    | return badRequest

  -- Update metrics
  Handler.metricsAddAgent true conn

  let metrics ← Handler.metrics
  Metrics.updateClients metrics "zulip"

  -- Extract and process command
  let data := ZulipMessage.data message
  let f := data.split ('\n' == ·) |>.toList |>.headD ""
  let data := extractLoogleCommand data |>.getD f.toString

  -- Query data
  let result ← queryData (some data)

  match result with
  | some res =>
    return json (json% { "content": $(zulipView data res) })
  | none =>
    return internalError
