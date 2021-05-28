module Soevnn.Utilities.Partitions

type Key<'key when 'key : comparison> =
| First 
| Key of 'key 
| Last

type PartitionTreeNode<'key,'item when 'key : comparison> =
| Item of 'item
| Partition of 'key * PartitionTreeNode<'key,'item> * PartitionTreeNode<'key,'item>

let rec GetItem key node =
    match node with
    | Item i -> i 
    | Partition(k,lp,hp) -> GetItem key (if key < k then lp else hp)

let rec AddItemHigh key item node =
    match node with
    | Item i -> Partition(key, Item i, Item item) 
    | Partition(k,lp,hp) ->
        if key < k then
            Partition(k,AddItemHigh key item lp, hp)
        else
            Partition(k, lp, AddItemHigh key item hp)

let rec AddItemLow key item node =
    match node with
    | Item i -> Partition(key, Item item, Item i) 
    | Partition(k,lp,hp) -> 
        if key < k then
            Partition(k,AddItemLow key item lp, hp)
        else
            Partition(k, lp, AddItemLow key item hp)

let rec AddItemsHigh pairs node =
    match pairs with
    | [] -> node
    | (key,item) :: tail ->
        AddItemsHigh tail (AddItemHigh key item node)
  
let rec AddItemsLow pairs node =
    match pairs with
    | [] -> node
    | (key,item) :: tail ->
        AddItemsLow tail (AddItemLow key item node)      


let rec AddPartitionHigh size item node add offset =
    match node with
    | Item i -> Partition(add(offset, size), Item i, Item item) 
    | Partition(k,lp,hp) ->
            Partition(k, lp, AddPartitionHigh size item hp add k)

let rec AddPartitionLow size item node subtract offset =
    match node with
    | Item i -> Partition(subtract(offset,size), Item i, Item item) 
    | Partition(k,lp,hp) ->
            Partition(k, AddPartitionLow size item lp subtract k, hp)

let rec AddPartitionsHigh sizevaluepairs node add offset =
    match sizevaluepairs with
    | [] -> node
    | (size,item) :: tail ->
        AddPartitionsHigh tail (AddPartitionHigh size item node add offset) add offset
  
let rec AddPartitionsLow sizevaluepairs node subtract offset =
    match sizevaluepairs with
    | [] -> node
    | (size,item) :: tail ->
        AddPartitionsLow tail (AddPartitionLow size item node subtract offset) subtract offset

let rec GetMaxDepth node =
    match node with
    | Item _ -> 0
    | Partition(_,lp,hp) -> 1 + max (GetMaxDepth lp) (GetMaxDepth hp)

let rec GetMinDepth node =
    match node with
    | Item _ -> 0
    | Partition(_,lp,hp) -> 1 + min (GetMinDepth lp) (GetMinDepth hp)

let rec GetCount node =
    match node with
    | Item _ -> 1
    | Partition(_,lp,hp) -> GetCount lp + GetCount hp
    
let GetBalance node =
    match node with
    | Item(i) -> 0
    | Partition(_, Item _, Item _) -> 0
    | Partition(_, lp, hp) -> GetCount(hp) - GetCount(lp)

let rec GetIndex key node =
    match node with
    | Item _ -> 0
    | Partition(k,lp,hp) ->
        if key > k then
            GetCount lp + GetIndex key hp
        else
            GetIndex key lp

let LiftLeft node =
    match node with
    | Partition(k,Partition(lk,i1,i2),i3) ->
        Partition(lk,i1,Partition(k,i2,i3))
    | _ -> node
        
let LiftRight node =
    match node with
    | Partition(k,i1,Partition(hk,i2,i3)) ->
        Partition(hk,Partition(k,i1,i2),i3)
    | _ -> node
        

let rec LiftToRoot index node root =
    match node with
    | Item(_) as item -> item
    | Partition(k,lp,hp) -> 
        let thisindex = GetIndex k root
        if index < thisindex then
            LiftLeft (Partition(k,LiftToRoot index lp root,hp))
        else if index > thisindex then
            LiftRight (Partition(k,lp,LiftToRoot index hp root))
        else
            Partition(k,lp,hp)
        
let rec Balance node =
    match node with
    | Item _ -> node
    | _ ->
        match LiftToRoot ((GetCount node)/2) node node with
        | Partition(k,lp,hp) -> Partition(k,Balance lp,Balance hp)
        | Item _ as item -> item

let rec GetLowestKey node =
    match node with
    | Partition(k,Item _,_) -> Some k
    | Partition(k,lp,_) -> GetLowestKey lp
    | Item _ -> None
        
let rec GetHighestKey node =
    match node with
    | Partition(k,_,Item _) -> Some k
    | Partition(k,_,hp) -> GetHighestKey hp
    | Item _ -> None

let rec map<'T,'U> (func : 'T -> 'U) node =
    match node with
    | Item i -> i |> func |> Item
    | Partition(k,lp,hp) -> Partition(k,map<'T,'U> func lp,map<'T,'U> func hp)

let GetKeys node =
    let rec getkeys node result =
        match node with
        | Item i -> []
        | Partition(k,lp,hp) -> 
            getkeys lp (k :: (getkeys hp result))
    getkeys node []

let GetItems node =
    let rec getitems node result =
        match node with
        | Item i -> [i]
        | Partition(k,lp,hp) -> 
            getitems lp (getitems hp result)
    getitems node []


let Product<'k,'a,'b,'c when 'k : comparison> (add : 'k -> 'k -> 'k) (subtract : 'k -> 'k -> 'k ) (multiply : 'k -> 'k -> 'k ) (nodeleft : PartitionTreeNode<'k,'a>) (noderight : PartitionTreeNode<'k,'b>) (map : 'a -> 'b -> 'c) (zerokey : 'k) : PartitionTreeNode<'k,'c> =
    let leftkeys = GetKeys nodeleft
    let leftitems = GetItems nodeleft
    let rightkeys = GetKeys noderight
    let rightitems = GetItems noderight
    let newkeys = 
        List.map2
            multiply
            (List.scan subtract zerokey leftkeys)
            (List.scan subtract zerokey rightkeys)
        |> List.scan add zerokey
    let newitems =
        List.map2 map leftitems rightitems
    AddItemsHigh (List.zip newkeys newitems.Tail) (Item newitems.Head)
    |> Balance
        


    

type PartitionTree<'key,'item when 'key : comparison>(node : PartitionTreeNode<'key,'item>) = 
    //public new(item) = PartitionTree(Item(item))
    abstract Root : PartitionTreeNode<'key,'item>
    abstract Item : 'key -> 'item with get
    default this.Root = node
    default this.Item
        with get(key) = GetItem key node 
    member this.AddItemHigh(key,item) = PartitionTree<'key,'item>(AddItemHigh key item node)
    member this.AddItemLow(key,item) = PartitionTree<'key,'item>(AddItemLow key item node)
    member this.Balance = PartitionTree(Balance(node))
    member this.HighestKey = GetHighestKey this.Root
    member this.LowestKey = GetLowestKey this.Root
            

