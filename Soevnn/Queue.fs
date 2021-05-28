module Soevnn.Utilities.Queue

type 'a Queue = 'a list * 'a list
let Empty<'a> : 'a Queue = [],[]
let IsEmpty = function
    | [],_ -> true
    | _ -> false
let checkf = function
    | [],r -> List.rev r,[]
    | q -> q

let Enqueue ((f,r): _ Queue) x  = checkf (f,x :: r)
let Dequeue = function
    | ([],_) -> None
    | (x::f,r) -> Some (x,checkf (f,r))
let Peak = function
    | ([],_) -> None
    | (x::f,r) -> Some x

let Contains (value) = function
    | ([],_) -> false
    | (f,r) -> List.contains value f || List.contains value r

let head = Peak
let tail = function
    | ([],_) -> None
    | (x::f,r) -> checkf (f,r) |> Some