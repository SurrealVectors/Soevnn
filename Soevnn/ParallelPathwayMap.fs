module Soevnn.Parallel.PathwayMap
open Soevnn.Utilities.PathwayMap
open System.Collections.Concurrent



let PathMapParallel<'l,'t when 'l : comparison> (directedgraph : Map<'l,PathNode<'l,'t>>) (asyncthreshold : int) (asynclayers : int) (origin : 'l) : Map<'l,'l> * Map<int,'l array> =
    let originnode = directedgraph.[origin]
    let discovered = ConcurrentQueue<PathNode<'l,'t>*int>()
    discovered.Enqueue((originnode,0))
    let results = ConcurrentDictionary<'l,'l*int>()
    
    let rec pathmap () : unit =
        [for d in [0..discovered.Count-1] do
            yield async{
                let ni = ref((originnode,0))
                if discovered.TryDequeue ni then
                    let n,i = ni.Value
                    for adj in n.adjecent do
                        if not <| results.ContainsKey(adj) then
                            if results.TryAdd(adj,(n.location,i+1)) then
                                discovered.Enqueue((n,i+1))
            }         
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        if discovered.Count > 0 then pathmap()


    let rec clump(result : _ list) (step : int)  (list : _ array)  =
        match Array.Parallel.choose (fun (l, (n,i)) -> if i = step then Some n else None) list with
        | [||] -> result
        | nr -> 
            clump
                ((step, nr) :: result)
                (step + 1)
                (Array.filter (fun (l, (n,i)) -> i <> step) list)

    if directedgraph.Count = 0 then
        (Map.empty,Map.empty)
    else
        pathmap()
        let pathlist =
            results.ToArray()
        (
            pathlist
            |> Array.Parallel.map (fun (kvp) -> (kvp.Key,kvp.Value |> fst))
            |> Map.ofArray,
            pathlist
            |> Array.Parallel.map (fun kvp -> (kvp.Key,kvp.Value))
            |> clump [] 0
            |> Map.ofList
        )

let InvertDirectionParallel<'l, 't when 'l : comparison> (directedgraph : ('l*PathNode<'l,'t>) array) =
    let dic = ConcurrentDictionary<'l,PathNode<'l,'t>*ConcurrentQueue<'l>>()
    directedgraph 
    |> Array.Parallel.iter
        (fun (l,n) -> 
            dic.AddOrUpdate(
                l,
                System.Func<_,_>(fun _ -> (n,new ConcurrentQueue<_>())),
                System.Func<_,_,_>(fun a (_,q : 'l ConcurrentQueue) -> (n,q)))
            |> ignore
        )
    directedgraph 
    |> Array.Parallel.iter
        (fun (l,n) -> 
            for adj in n.adjecent do
                dic.AddOrUpdate(
                    adj,
                    System.Func<_,_>(fun _ -> (n,new ConcurrentQueue<_>())),
                    System.Func<_,_,_>(fun a (_,q : 'l ConcurrentQueue) -> q.Enqueue(l); (n,q)))
                |> ignore
        )
    [for pair in dic -> 
        let l,(n,q) = pair.Key,pair.Value
        async {
            return (l, {location = l; adjecent = List.ofArray(q.ToArray()); data = n})
        }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously