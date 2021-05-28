module Soevnn.Utilities.ProbabilityMap
open Soevnn.Utilities.Partitions

type ProbabilityMap<'item> (rand : System.Random, events : (int * 'item) list) =
    inherit PartitionTree<int,'item>(AddItemsLow (List.scan (fun (accumchance,_) (chance,result) -> (accumchance+chance,result)) (0,snd events.Head) events) (Item (snd events.Head)))
    member this.Maximum = match GetHighestKey this.Root with | Some v -> v | None -> System.Int32.MaxValue
    member this.Event() = this.Item(rand.Next(0,this.Maximum))
    static member Event<'eventitem> (pmap:ProbabilityMap<'eventitem>) = pmap.Event()

type PMS<'a> =
| Submap of ProbabilityMap<'a> * int list
| SubItem of 'a

type ChainedMap<'a,'b> = ProbabilityMap<'a> -> PMS<'b>

let Return (item : 'b) : ChainedMap<'a,'b> =
    let p stream = SubItem(item)
    in p

let Bind (p: ChainedMap<'s,'a>) (f: 'a -> ChainedMap<'s,'b>) : ChainedMap<'s,'b> =
    let q (stream : int list) =
        match p stream with
        | Submap(x,rest) -> (f x) rest
        | Failure -> Failure
    in q



