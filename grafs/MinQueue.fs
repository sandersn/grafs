module MinQueue
open Util
open Types
let left i = 2 * i + 1
let right i = 2 * i + 2
let parent i = (i - 1) / 2
type MinQueue<'a,'b when 'b : comparison> (l : seq<'a>, key : 'a -> 'b) =
  let a = ResizeArray l
  let swap i j =
    let tmp = a.[i]
    a.[i] <- a.[j]
    a.[j] <- tmp
  let rec minHeapify i =
    let l,r = left i, right i
    let smallest = [l;r;i] |> List.filter ((>)a.Count) |> List.minBy (fun i -> key a.[i])
    if smallest <> i
    then swap i smallest
         minHeapify smallest
  do for i = a.Count / 2 downto 0 do
       minHeapify i
  member this.ExtractMin () =
    if a.Count = 0 then failwith "Heap underflow"
    let min = a.[0]
    a.[0] <- a.[a.Count - 1]
    a.RemoveAt(a.Count - 1)
    if a.Count > 0 then minHeapify 0
    min
  member this.DecreaseKey i (newValue : 'a) =
    if key newValue > key a.[i] then failwith "New key is larger than current key"
    a.[i] <- newValue
    let mutable j = i
    while j > 0 && key a.[parent j] > key a.[j] do
      swap i (parent j)
      j <- parent j
    j
  member this.Count = a.Count
  member this.Contains x = a.Contains x
  // TEST HOOK
  member this.Vector = a
// supports DecreaseKey by requiring 'a to be Prim<'k>
// (maybe)
type MinQueuePrim<'a when 'a : equality> (l : seq<'a>, key : 'a -> int) =
  let a = ResizeArray (Seq.mapi (fun i v -> {vertex=v; i=i; key=key v}) l)
  let swap i j =
    let tmp = a.[i]
    a.[i] <- a.[j]
    a.[j] <- tmp
    a.[i].i <- i
    a.[j].i <- j
  let rec minHeapify i =
    let l,r = left i, right i
    let smallest = [l;r;i] |> List.filter ((>)a.Count) |> List.minBy (fun i -> a.[i].key)
    if smallest <> i
    then swap i smallest
         minHeapify smallest
  do for i = a.Count / 2 downto 0 do
       minHeapify i
  member this.ExtractMin () =
    if a.Count = 0 then failwith "Heap underflow"
    let min = a.[0]
    a.[0] <- a.[a.Count - 1]
    a.RemoveAt(a.Count - 1)
    if a.Count > 0 then minHeapify 0
    min
  member this.DecreaseKey v (newKey : int) =
    if newKey > a.[v.i].key then failwith "New key is larger than current key"
    a.[v.i] <- {v with key=newKey}
    let mutable j = v.i
    while j > 0 && a.[parent j].key > a.[j].key do
      swap j (parent j)
      j <- parent j
  member this.Count = a.Count
  member this.Contains x = Seq.exists (fun {vertex=v} -> x = v) a
  // TEST HOOK
  member this.Vector = a

// simple O(n) implementation
type MinQueueSimple<'a when 'a : equality> (l : seq<'a>, key : 'a -> int) =
  let a = ResizeArray l
  member this.ExtractMin () =
    if a.Count = 0 then failwith "Queue empty"
    let min = Seq.minBy key a
    a.Remove min |> ignore
    min
  member this.DecreaseKey v (newKey:int) =
    ()
  member this.Count = a.Count
  member this.Contains x = a.Contains x
  member this.Vector = a
