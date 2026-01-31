import Lean
import LeanSearchClient
import Markov
import Lean.Data.Json.Parser
import Std.Data.HashMap

structure Matrix.Content where
  body : Option String
  msgtype : Option String
  deriving Lean.ToJson, Lean.FromJson, Inhabited, Repr

structure Matrix.Messages where
  content : Matrix.Content
  type : String
  event_id : String
  deriving Lean.ToJson, Lean.FromJson, Inhabited, Repr

structure Matrix where
  room_name : String
  messages : List Matrix.Messages
  deriving Lean.ToJson, Lean.FromJson, Inhabited, Repr

def Matrix.Messages.textOnly : List Matrix.Messages → List Matrix.Messages := List.filter λ x =>
  match x.content.msgtype with
  | Option.some msg => msg = "m.text"
  | Option.none => false

def Matrix.Messages.deduplicate : List Matrix.Messages → List Matrix.Messages := List.eraseDupsBy λ x y => x.event_id = y.event_id

def Matrix.Messages.getNormalized : List Matrix.Messages → List String := (List.map λ x => 
  match x.content.body with
  | Option.some msg => toString <| String.Slice.trimAsciiEnd <| String.trimAsciiEnd <| msg
  | Option.none => ""
  ) ∘ Matrix.Messages.textOnly

def main : IO Unit := do
  let new_data ← IO.FS.readFile "matrix-new.json"
  let old_data ← IO.FS.readFile "matrix-old.json"
  match Lean.Json.parse new_data, Lean.Json.parse old_data with
  | Except.error e, _ => IO.println s!"Error occurred: {e}"
  | _, Except.error e => IO.println s!"Error occurred: {e}"
  | Except.ok a, Except.ok b => 
    match Lean.FromJson.fromJson? a, Lean.FromJson.fromJson? b with
    | Except.error e, _ => IO.println s!"Error validating JSON: {e}"
    | _, Except.error e => IO.println s!"Error validating JSON: {e}"
    | Except.ok (a : Matrix), Except.ok (b : Matrix) =>
      let messages := Matrix.Messages.deduplicate <| (Matrix.Messages.textOnly b.messages) ++ (Matrix.Messages.textOnly a.messages)
      let model := Markov.train <| String.intercalate ". " <| Matrix.Messages.getNormalized messages
      let mut next := model.getStarter
      let mut i := 0
      while i <= 2000 do
        next >>= IO.print
        IO.print " "
        let possibilities := model.get? (← next)
        next := match possibilities with
        | Option.none => model.getStarter
        | Option.some words =>
          if h : words.length > 0 then do
            pure <| words.get (← IO.randFin (words.length) h)
          else
            model.getStarter
        i := i + 1
