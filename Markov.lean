-- This module serves as the root of the `Markov` library.
-- Import modules here that should be built as part of the library.
import Markov.Basic
import Std

def IO.randFin (n : Nat) (h : n > 0) : IO (Fin n) := do
  let v ← IO.rand 0 (n - 1)
  if h' : v < n then
    pure ⟨v, h'⟩
  else
    pure ⟨0, h⟩

namespace Markov

def MarkovChain := Std.HashMap String (List String) deriving Repr

def train (source : String) : MarkovChain :=
  let tokens := source.splitOn " "
  let rec go (tokens : List String) (chain : MarkovChain) : MarkovChain :=
    match tokens with
    | a :: b :: c :: d :: tail =>
      let key := s!"{a} {b} {d}"
      let val := c
      let currentList := match chain.get? key with
        | some l => l
        | none => []
      let chain := chain.insert key (val :: currentList)
      go (b :: c :: tail) chain
    | _ => chain
  go tokens (Std.HashMap.emptyWithCapacity 1000)

def MarkovChain.getStarter (chain : MarkovChain) : IO String := do
  let keys := chain.keys
  if h : keys.length > 0 then
    let rng ← IO.randFin keys.length h
    pure keys[rng]
  else
    pure ""

/- def inference (chain : MarkovChain) (length : ℕ) : String :=  -/

end Markov
