import Server.Context
import Std.Internal.Http

namespace Server.Handler
open Std Http
open Lean

def metricsAddAgent (isJson: Bool) (request: Request Body) : Handler Unit := do
  let metrics ← Handler.metrics
  let loogleClient := request.head.headers.getD (.ofString! "x-loogle-client") (.ofString! "")
  let userAgent := request.head.headers.getD (.ofString! "user-agent") (.ofString! "")

  if isJson then
    if loogleClient.value.startsWith "lean4/" then
      Metrics.updateClients metrics "vscode-lean4"
    else if userAgent.value = "vscode" then
      Metrics.updateClients metrics "vscode-loogle"
    else if userAgent.value= "lean.nvim" then
      Metrics.updateClients metrics "nvim"
    else if userAgent.value = "lean+nvim" then
      Metrics.updateClients metrics "nvim"
    else
      Metrics.updateClients metrics "json"
  else
    Metrics.updateClients metrics "web"
