import Std.Internal.Http
import Server.Context
import Lean.Data.Json

open Std Http Internal IO Async

def internalError : Response Body :=
  Response.new
    |>.status .internalServerError
    |>.body "Internal Error."

def redirect (url: URI) : Response Body :=
  Response.new
    |>.status .found
    |>.header! "Location" (toString url)
    |>.build

def json (json: Lean.Json) : Response Body :=
  Response.new
    |>.header! "Content-Type" "application/json"
    |>.body (toString json)

def plain (json: String) : Response Body :=
  Response.new
    |>.header! "Content-Type" "text/plain"
    |>.body json

def png (data: ByteArray) : Response Body :=
  Response.new
    |>.status .ok
    |>.header! "Content-Type" "image/png"
    |>.body data

def html (data: ByteArray) : Response Body :=
  Response.new
    |>.status .ok
    |>.header! "Content-Type" "text/html"
    |>.body data

def notFound : Response Body :=
  Response.new
    |>.status .notFound
    |>.header! "Content-Type" "text/plain"
    |>.body "Not found."

def badRequest : Response Body :=
  Response.new
    |>.status .badRequest
    |>.header! "Content-Type" "text/plain"
    |>.body "Bad Request."

def getJson (conn: Request Body) : Async (Except String Lean.Json) := do
  let some data ← conn.body.collectString (some (1024 * 1024 * 5))
    | return .error "cannot parse string"
  return (Lean.Json.parse data)
