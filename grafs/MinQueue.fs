module MinQueue
let left i = 2 * i + 1
let right i = 2 * i + 2
let parent i = (i - 1) / 2
type MinQueue<'a when 'a : comparison> (l : seq<'a>) =
  let a = ResizeArray l
  let swap i j =
    let tmp = a.[i]
    a.[i] <- a.[j]
    a.[j] <- tmp
  let rec minHeapify i =
    let l,r = left i, right i
    let smallest = [l;r;i] |> List.filter ((>)a.Count) |> List.minBy (fun i -> a.[i])
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
  member this.Count = a.Count
  member this.Contains x = a.Contains x
  // TEST HOOK
  member this.Vector = a
