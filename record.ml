module type Table = sig
  type entry
  exception EmptyList
  type t = entry list 
  
  val is_equal_entry : entry -> entry -> bool
  val empty : t
  val add : entry -> t -> t
  val delete : entry -> t -> t
end

module RecordTable : Table = struct 
  type entry = {name : string; date : string; status : string}
  type t = entry list 
  exception EmptyList

  let is_equal_entry e1 e2 = e1.name = e2.name && e1.date = e2.date && e1.status = e2.status
  let empty = []
  let add entry lst = entry :: lst
  let rec delete entry lst =
    match lst with
    | [] -> raise EmptyList
    | h :: t -> if is_equal_entry entry h then t else h :: delete entry t 
end