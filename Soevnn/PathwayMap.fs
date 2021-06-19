module Soevnn.Utilities.PathwayMap

/// <summary> A node used to as part of a PathMap. </summary>
type PathNode<'l, 't when 'l : comparison> = { location : 'l; adjacent : 'l list; data : 't}

/// <summary> Reverses the direction of all connections for a directed graph. <summary>
let InvertDirection<'l, 't when 'l : comparison> (directedgraph : Map<'l,PathNode<'l,'t>>) =
    // For each node, it finds the nodes with this one listed as adjacent and uses those nodes as the new list of adjacent nodes.
    directedgraph
    |> Map.map 
        (fun l n -> 
            {location = l ; adjacent = [for (fl,fn) in Map.toList directedgraph do if List.contains l fn.adjacent then yield! [fl]]; data = n.data}
        )
        

/// <summary> If the location 'fromhere' is in the pathingmap, then it finds the distance to the maps destination, otherwise it returns None. The destination is represented as the key which maps to itself. </summary>
let PathLength (pathingmap) (fromhere) =
    let rec pathlength (pm : Map<'l,'l>) (l : 'l) (dist : int) =
        if pm.[l] = l then
            Some dist
        else
            pathlength pm (pm.[l]) (dist + 1)
    if Map.containsKey fromhere pathingmap then
        None
    else
        pathlength pathingmap fromhere 0

/// <summary> Creates a map that gives directions for the shortest routes to a particular destination in a directed graph. It also returns a distance map that groups the nodes by distance. </summary>
let PathMap<'l,'t when 'l : comparison> (directedgraph : Map<'l,PathNode<'l,'t>>) (destination : 'l) =
    
    // Parses the graph. It makes a list of nodes which have possible paths to the destination. Each node is assigned which adjacent node is the quickest way to the destination node, as well as the distance there.
    // d/dis is a list of already discovered locations. Marks these so they aren't redundantly processed. 
    // n is the node currently being processed
    // r/result is the list processed nodes.
    // i is the distance from the destination as measured by the number of connections in the path.
    let rec pathmap d n r i : 'l list * PathNode<'l,'t*('l*int)> list =
        // Recursively explores the graph by following the connections from the destination. 
        List.fold 
            (fun (dis : 'l list, result : PathNode<'l,'t * ('l*int)> list) (l : 'l) -> 
                if not <| List.contains l dis then
                    pathmap (l :: dis) l ({location = l; adjacent = directedgraph.[l].adjacent; data = (directedgraph.[l].data,(n,i))} :: result) (i+1)
                else
                    (dis,result)
                ) 
            (d, r) 
            (directedgraph.[n].adjacent)

    // Reorganizes the path list to group the nodes by distance.
    let rec clump(result : _ list) (step : int)  (list : _ list)  =
        match List.choose (fun (l, (n,i)) -> if i = step then Some n else None) list with
        | [] -> result
        | nr -> 
            clump
                ((step, nr) :: result)
                (step + 1)
                (List.filter (fun (l, (n,i)) -> i <> step) list)

    if directedgraph.Count = 0 then
        (Map.empty,Map.empty)
    else
        let pathlist =
            pathmap [] destination [] 1
            |> snd
            |> List.map (fun (pn : PathNode<_,_>) -> (pn.location,snd pn.data))
            |> (fun head tail -> List.Cons(head,tail)) (destination,(destination,0))
        (
            pathlist
            |> List.map (fun (l, (n,i)) -> (l,n))
            |> Map.ofList,
            pathlist
            |> clump [] 0
            |> Map.ofList
        )

/// <summary> Checks if the path from a node to the pathing map's destination exists. </summary>
// The map only contains nodes which have a valid path. So simply checking if the map contains the node is enough to verify that there is a valid path.
let PathExists<'l when 'l : comparison> (pathingmap : Map<'l,'l>) (fromhere) = Map.containsKey fromhere pathingmap

/// <summary> Creates a list of nodes that make a the path from a particular node to the destination. This path excludes the destination and source. </summary>
let BuildPath<'l when 'l : comparison> (pathingmap : Map<'l,'l>) (fromhere) = 
    let rec buildpath l path =
        let nl = pathingmap.[l]
        if nl = l then path else buildpath nl (l::path)
    buildpath fromhere []


