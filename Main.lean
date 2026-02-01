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
  sender : String
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
  | Option.some msg => String.toLower <| toString <| String.Slice.trimAsciiEnd <| String.trimAsciiEnd <| msg
  | Option.none => ""
  ) ∘ Matrix.Messages.textOnly

def Matrix.Messages.filterByUsers (users : List String) (messages : List Matrix.Messages) : List Matrix.Messages :=
  messages.filter (users.contains ·.sender)

/- def main : IO Unit := do -/
/-   let corpus ← IO.FS.readFile "wotw.txt" -/
/-   let model := Markov.train corpus -/
/-   Markov.inference model 1000 Option.none >>= IO.println -/

structure Config where
  files : List System.FilePath
  users : Option (List String)

def parseArgs (args : List String) : Config :=
  let args := match args.findIdx? (· == "--users") with
  | Option.some i =>
    if i = args.length - 1 then
      (args.drop 1, Option.none)
    else
      let x := (args.splitAt i)
      (x.fst, Option.some <| x.snd.drop 1)
  | Option.none => (args, Option.none)

  let users := args.snd
  let files := args.fst.map System.FilePath.mk
  Config.mk files users

def main (args : List String) :  IO Unit := do
  let config := parseArgs args

  if config.files.length = 0 then
    IO.println "You need to specify at least one filepath."
    return

  let data : List String ← config.files.mapM IO.FS.readFile
  let parsed := data.map Lean.Json.parse

  let parsed : List Lean.Json := parsed.map λ x => match x with
  | Except.ok a => a
  | Except.error _ => Lean.ToJson.toJson "{}"

  let parsed : List (Except String Matrix) := parsed.map Lean.FromJson.fromJson?

  let parsed : List Matrix := parsed.map λ x => match x with
  | Except.ok a => a
  | Except.error _ => Matrix.mk "" [] 

  let messages : List Matrix.Messages := Matrix.Messages.deduplicate <| Matrix.Messages.textOnly <| List.foldl List.append [] <| parsed.map (·.messages)
  let messages := match config.users with
  | Option.some users => Matrix.Messages.filterByUsers users messages
  | Option.none => messages

  let model := Markov.train <| String.intercalate ". " <| Matrix.Messages.getNormalized messages
  Markov.inference model 1000 Option.none >>= IO.println
