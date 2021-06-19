module Soevnn.Utilities.Partitions

/// <summary> Adds "First" and "Last" keys to another key type. </summary>
type Key<'key when 'key : comparison> =
| First 
| Key of 'key 
| Last

/// <summary> A node in the partition tree. This can be an item or a subdivision. </summary>
type PartitionTreeNode<'key,'item when 'key : comparison> =
| Item of 'item
| Partition of 'key * PartitionTreeNode<'key,'item> * PartitionTreeNode<'key,'item>

/// <summary> Gets the item mapped to the partition the key is within. </summary>
let rec GetItem key node =
    match node with
    | Item i -> i 
    | Partition(k,lp,hp) -> GetItem key (if key < k then lp else hp)

/// <summary> Adds a partition/key pair using the lower bound of the partition. </summary>
let rec AddItemHigh key item node =
    match node with
    | Item i -> Partition(key, Item i, Item item) 
    | Partition(k,lp,hp) ->
        if key < k then
            Partition(k,AddItemHigh key item lp, hp)
        else
            Partition(k, lp, AddItemHigh key item hp)

/// <summary> Adds a partition/key pair using the upper bound of the partition. </summary>
let rec AddItemLow key item node =
    match node with
    | Item i -> Partition(key, Item item, Item i) 
    | Partition(k,lp,hp) -> 
        if key < k then
            Partition(k,AddItemLow key item lp, hp)
        else
            Partition(k, lp, AddItemLow key item hp)

/// <summary> Adds multiple partition/key pairs using the lower bound of the partitions. </summary>
let rec AddItemsHigh pairs node =
    match pairs with
    | [] -> node
    | (key,item) :: tail ->
        AddItemsHigh tail (AddItemHigh key item node)

/// <summary> Adds multiple partition/key pairs using the upper bound of the partitions. </summary>  
let rec AddItemsLow pairs node =
    match pairs with
    | [] -> node
    | (key,item) :: tail ->
        AddItemsLow tail (AddItemLow key item node)      

/// <summary> Adds one partition node to another as a lower bound. </summary>
let rec AddPartitionHigh size item node add offset =
    match node with
    | Item i -> Partition(add(offset, size), Item i, Item item) 
    | Partition(k,lp,hp) ->
            Partition(k, lp, AddPartitionHigh size item hp add k)

            
/// <summary> Adds one partition node to another as a upper bound. </summary>
let rec AddPartitionLow size item node subtract offset =
    match node with
    | Item i -> Partition(subtract(offset,size), Item i, Item item) 
    | Partition(k,lp,hp) ->
            Partition(k, AddPartitionLow size item lp subtract k, hp)


/// <summary> Adds multiple partitions node to another as a lower bound. </summary>
let rec AddPartitionsHigh sizevaluepairs node add offset =
    match sizevaluepairs with
    | [] -> node
    | (size,item) :: tail ->
        AddPartitionsHigh tail (AddPartitionHigh size item node add offset) add offset
 
 /// <summary> Adds multiple partitions node to another as a upper bound. </summary>
let rec AddPartitionsLow sizevaluepairs node subtract offset =
    match sizevaluepairs with
    | [] -> node
    | (size,item) :: tail ->
        AddPartitionsLow tail (AddPartitionLow size item node subtract offset) subtract offset

/// <summary> Gets the maximum depth of the leaves of a node. </summary>
let rec GetMaxDepth node =
    match node with
    | Item _ -> 0
    | Partition(_,lp,hp) -> 1 + max (GetMaxDepth lp) (GetMaxDepth hp)
    
/// <summary> Gets the minimum depth of the leaves of a node. </summary>
let rec GetMinDepth node =
    match node with
    | Item _ -> 0
    | Partition(_,lp,hp) -> 1 + min (GetMinDepth lp) (GetMinDepth hp)

/// <summary> Gets the number of items in the node. </summary>
let rec GetCount node =
    match node with
    | Item _ -> 1
    | Partition(_,lp,hp) -> GetCount lp + GetCount hp
    
/// <summary> Gets the balance of the two branches of a partition. </summary>
let GetBalance node =
    match node with
    | Item(i) -> 0
    | Partition(_, Item _, Item _) -> 0
    | Partition(_, lp, hp) -> GetCount(hp) - GetCount(lp)
    
/// <summary> Gets the index of a key within a node. The index is enumerated from lowest to highest keys. </summary>
let rec GetIndex key node =
    match node with
    | Item _ -> 0
    | Partition(k,lp,hp) ->
        if key > k then
            GetCount lp + GetIndex key hp
        else
            GetIndex key lp
            
/// <summary> Lifts the left sub-partition. </summary>
let LiftLeft node =
    match node with
    | Partition(k,Partition(lk,i1,i2),i3) ->
        Partition(lk,i1,Partition(k,i2,i3))
    | _ -> node
        
/// <summary> Lifts the right sub-partition. </summary>
let LiftRight node =
    match node with
    | Partition(k,i1,Partition(hk,i2,i3)) ->
        Partition(hk,Partition(k,i1,i2),i3)
    | _ -> node
        
        
/// <summary> Lifts a partition to root. If the node is an item, this does nothing. </summary>
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
        
/// <summary> Balances a partition tree. </summary>
let rec Balance node =
    match node with
    | Item _ -> node
    | _ ->
        match LiftToRoot ((GetCount node)/2) node node with
        | Partition(k,lp,hp) -> Partition(k,Balance lp,Balance hp)
        | Item _ as item -> item

/// <summary> Gets the lowest boundary key. </summary>
let rec GetLowestKey node =
    match node with
    | Partition(k,Item _,_) -> Some k
    | Partition(k,lp,_) -> GetLowestKey lp
    | Item _ -> None
        
/// <summary> Gets the highest boundary key. </summary>
let rec GetHighestKey node =
    match node with
    | Partition(k,_,Item _) -> Some k
    | Partition(k,_,hp) -> GetHighestKey hp
    | Item _ -> None

/// <summary> Maps the items in the partition tree. </summary>
let rec map<'T,'U> (func : 'T -> 'U) node =
    match node with
    | Item i -> i |> func |> Item
    | Partition(k,lp,hp) -> Partition(k,map<'T,'U> func lp,map<'T,'U> func hp)

/// <summary> Gets a list of all partition boundary keys. </summary>
let GetKeys node =
    let rec getkeys node result =
        match node with
        | Item i -> []
        | Partition(k,lp,hp) -> 
            getkeys lp (k :: (getkeys hp result))
    getkeys node []
    
/// <summary> Gets a list of all items in the partition tree. </summary>
let GetItems node =
    let rec getitems node result =
        match node with
        | Item i -> [i]
        | Partition(k,lp,hp) -> 
            getitems lp (getitems hp result)
    getitems node []

/// <summary> Multiplies the boundaries of two partition trees. </summary>
// ToDo: Add unit tests to verify this functions correctly.
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
        


    
    
/// <summary> A partition tree is a balanced tree with the keys acting as boundaries between partitions. The items are mapped to the partitions rather than the keys. The items can be accessed by any key within the boundaries of the item's associated partition. </summary>
// This provides a tidy way to interact with the partition trees.
// Partition trees are useful as probability maps that are accessed by randomly generated keys, giving each item a particular chance of being selected.
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
            

