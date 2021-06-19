module Soevnn.Utilities.PathwayMap

type PathNode<'l, 't when 'l : comparison> = { location : 'l; adjecent : 'l list; data : 't}

let InvertDirection<'l, 't when 'l : comparison> (directedgraph : Map<'l,PathNode<'l,'t>>) =
    directedgraph
    |> Map.map 
        (fun l n -> 
            {location = l ; adjecent = [for (fl,fn) in Map.toList directedgraph do if List.contains l fn.adjecent then yield! [fl]]; data = n.data}
        )
        


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


let PathMap<'l,'t when 'l : comparison> (directedgraph : Map<'l,PathNode<'l,'t>>) (origin : 'l) =
    let rec pathmap d n r i : 'l list * PathNode<'l,'t*('l*int)> list =
        List.fold 
            (fun (dis : 'l list, result : PathNode<'l,'t * ('l*int)> list) (l : 'l) -> 
                if not <| List.contains l dis then
                    pathmap (l :: dis) l ({location = l; adjecent = directedgraph.[l].adjecent; data = (directedgraph.[l].data,(n,i))} :: result) (i+1)
                else
                    (dis,result)
                ) 
            (d, r) 
            (directedgraph.[n].adjecent)
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
            pathmap [] origin [] 1
            |> snd
            |> List.map (fun (pn : PathNode<_,_>) -> (pn.location,snd pn.data))
            |> (fun head tail -> List.Cons(head,tail)) (origin,(origin,0))
        (
            pathlist
            |> List.map (fun (l, (n,i)) -> (l,n))
            |> Map.ofList,
            pathlist
            |> clump [] 0
            |> Map.ofList
        )

let PathExists<'l when 'l : comparison> (pathingmap : Map<'l,'l>) (tohere) = Map.containsKey tohere pathingmap

let BuildPath<'l when 'l : comparison> (pathingmap : Map<'l,'l>) (tohere) = 
    let rec buildpath l path =
        let nl = pathingmap.[l]
        if nl = l then path else buildpath nl (l::path)
    buildpath tohere []


