module Soevnn.Utilities.ProbabilityMap
open Soevnn.Utilities.Partitions

/// <summary> Creates a list of events with varying probabilities of occurring. The probabilities are defined by relative weights. </summary>
type ProbabilityMap<'event> (rand : System.Random, events : (int * 'event) list) =
    inherit PartitionTree<int,'event>(AddItemsLow (List.scan (fun (accumchance,_) (chance,result) -> (accumchance+chance,result)) (0,snd events.Head) events) (Item (snd events.Head)))
    member this.Maximum = match GetHighestKey this.Root with | Some v -> v | None -> System.Int32.MaxValue
    member this.Event() = this.Item(rand.Next(0,this.Maximum))
    static member Event<'eventitem> (pmap:ProbabilityMap<'eventitem>) = pmap.Event()

// The goal of the following code was to chain events. Probability maps only get used in a few places so it seemed not worth the effort for now. Should be split into a separate branch to do later.
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



