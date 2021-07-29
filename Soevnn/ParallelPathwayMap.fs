module Soevnn.Parallel.PathwayMap
open Soevnn.Utilities.PathwayMap
open System.Collections.Concurrent


/// <summary> Parallel version of PathMap. Creates a map that gives directions for the shortest routes to a particular destination in a directed graph. It also returns a distance map that groups the nodes by distance. </summary>
let PathMapParallel<'l,'t when 'l : comparison> (directedgraph : Map<'l,PathNode<'l,'t>>) (asyncthreshold : int) (asynclayers : int) (origin : 'l) : Map<'l,'l> * Map<int,'l array> =
    let originnode = directedgraph.[origin]
    let discovered = ConcurrentQueue<PathNode<'l,'t>*int>() // A list of already discovered locations. Marks these so they aren't redundantly processed. 
    discovered.Enqueue((originnode,0))
    let results = ConcurrentDictionary<'l,'l*int>() // A list of processed nodes.
    
    let rec pathmap () : unit = // Processes the nodes.
        [for d in [0..discovered.Count-1] do // Process all newly discovered nodes.
            yield async{ // Mark this block to be done asynchronously.
                let ni = ref((originnode,0))
                if discovered.TryDequeue ni then // Attempt to dequeue a discovered node to be processed.
                    let n,i = ni.Value
                    for adj in n.adjacent do
                        if not <| results.ContainsKey(adj) then // If the adjacent node hasn't been processed already, then do so.
                            if results.TryAdd(adj,(n.location,i+1)) then // Add it to the results.
                                discovered.Enqueue((n,i+1)) // Mark it as discovered.
            }         
        ]
        |> Async.Parallel
        |> Async.RunSynchronously
        |> ignore
        if discovered.Count > 0 then pathmap()

    // Reorganizes the path list to group the nodes by distance. Works the same as the sequential clump. Look there for more details.
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

/// <summary> Parallel version of InvertDirection. Reverses the direction of all connections for a directed graph. <summary>
let InvertDirectionParallel<'l, 't when 'l : comparison> (directedgraph : ('l*PathNode<'l,'t>) array) =
    let dic = ConcurrentDictionary<'l,PathNode<'l,'t>*ConcurrentQueue<'l>>()
    directedgraph 
    |> Array.Parallel.iter // Initialize the dictionary. 
        (fun (l,n) -> 
            dic.AddOrUpdate( 
                l, 
                System.Func<_,_>(fun _ -> (n,new ConcurrentQueue<_>())), // 
                System.Func<_,_,_>(fun a (_,q : 'l ConcurrentQueue) -> (n,q)))
            |> ignore
        )
    directedgraph
    |> Array.Parallel.iter // For each node, enqueue the ones that connect to it.
        (fun (l,n) -> 
            for adj in n.adjacent do
                dic.AddOrUpdate(
                    adj,
                    System.Func<_,_>(fun _ -> (n,new ConcurrentQueue<_>())),
                    System.Func<_,_,_>(fun a (_,q : 'l ConcurrentQueue) -> q.Enqueue(l); (n,q)))
                |> ignore
        )
    [for pair in dic -> // Replace the adjacents that connect from each node with the queue of adjacents which connect to that node.
        let l,(n,q) = pair.Key,pair.Value
        async {
            return (l, {location = l; adjacent = List.ofArray(q.ToArray()); data = n}) // TODO: Optimize the queue-to-list conversion. ; TODO: Change "data = n" to "data = n.data".
        }
    ]
    |> Async.Parallel
    |> Async.RunSynchronously