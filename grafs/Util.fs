module Util
open System.Collections.Generic

module TupleArrow =
  let both f (a,b) = (f a,f b)

module Dictionary =
  let fromList pairs = 
    let d = Dictionary (Seq.length pairs)
    for k,v in pairs do
      d.[k] <- v
    d
  let item (pair : KeyValuePair<_,_>) = (pair.Key, pair.Value)
  let Key (pair : KeyValuePair<_,_>) = pair.Key