import Lean
import LeanSearchClient
import Markov
import Lean.Data.Json.Parser

structure Content where
  body : String
  msgtype : String
  deriving Lean.ToJson, Lean.FromJson, Inhabited, Repr

structure Messages where
  content : Content
  type : String
  deriving Lean.ToJson, Lean.FromJson, Inhabited, Repr

structure Matrix where
  room_name : String
  messages : List Messages
  deriving Lean.ToJson, Lean.FromJson, Inhabited, Repr

def main : IO Unit := do
  let data ← IO.FS.readFile "matrix.json"
  match Lean.Json.parse data with
  | Except.error e => IO.println s!"Error occurred: {e}"
  | Except.ok a => 
    let validated : Except String Matrix := Lean.FromJson.fromJson? a
    if let Except.error e := validated then do
      IO.println s!"Error validating JSON: {e}"
      return
  IO.println <| String.take data 50
