exception Unimplemented

module type Formattable = sig
  type t
  val format : Format.formatter -> t -> unit
end

module type Comparable = sig
  type t
  val compare : t -> t -> [ `EQ | `GT | `LT ]
  include Formattable with type t := t
end

type ('k,'v) tree23 =
  | Leaf
  | Twonode of ('k,'v) twonode
  | Threenode of ('k,'v) threenode
and ('k,'v) twonode = {
  left2  : ('k,'v) tree23;
  value  : 'k * 'v;
  right2 : ('k,'v) tree23;
}
and ('k,'v) threenode = {
  left3   : ('k,'v) tree23;
  lvalue  : 'k * 'v;
  middle3 : ('k,'v) tree23;
  rvalue  : 'k * 'v;
  right3  : ('k,'v) tree23;
}


type ('k,'v) kicktree =
  |    Kicknode of ('k,'v) kicknode
and ('k,'v) kicknode = {
  left2  : ('k,'v) tree23;
  value  : 'k * 'v;
  right2 : ('k,'v) tree23;
}


type ('k,'v) trees=
   |Tree23 of ('k,'v) tree23
   |KickTree of ('k,'v) kicktree


module type Dictionary = sig
  module Key : Comparable
  module Value : Formattable
  type key = Key.t
  type value = Value.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : key -> value -> t -> t
  val member : key -> t -> bool
  val find : key -> t -> value option
  val remove : key -> t -> t
  val choose : t -> (key * value) option
  val fold : (key -> value -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> (key * value) list
  val expose_tree : t -> (key,value) tree23
  val format : Format.formatter -> t -> unit
end

module type DictionaryMaker =
  functor (K : Comparable) (V : Formattable)
    -> Dictionary with module Key = K and module Value = V

module MakeListDictionary (K : Comparable) (V : Formattable) = struct
  module Key = K
  module Value = V
  type key = K.t
  type value = V.t

  (* TODO: change type [t] from [unit] to something involving
   * association lists. *)
  (* AF: The list [(k1,v1),(k2,v2)...(kn,vn)] represents a list of n
   *     key value pairs ordered from least key to greatest key. [] is
   *     the empty list.
   * RI: TODO: The pairs are ordered from least key to greatest key and
               the list contains no duplicates
   *)
  type t = (key * value) list


  let rec dup_check d=
    match d with
    |[]->true
    |h::t-> if List.mem_assoc (fst h) t then failwith "There are duplicate keys in the list"
      else dup_check t

  let rec order_check d k=
    match d with
    |[]-> true
    |h::t -> if Key.compare k (fst h) = `GT then failwith "keys are not ordered correctly"
      else order_check t (fst h)

  let rec rep_ok d =
      match d with
    |[] ->d
    |h::t -> if dup_check d && order_check d (fst h) then d else failwith "suck it trebek, your dictionary blows"

  let empty = []

  let is_empty d =
    match rep_ok d with
    |[] -> true
    |h::t -> false

  let size d =
     List.length d


 let insert k v d =
   let rec insert_help k v d new_d=
     match d with
     |[]->List.rev_append new_d ((k,v)::[])
     |h::t -> if (Key.compare (fst h) k )= `LT then insert_help k v t (h::new_d)
       else if (Key.compare (fst h) k )= `EQ then List.rev_append ((k,v)::new_d) t

       else List.rev_append ((k,v)::new_d) d

  in insert_help k v d []


  let remove k d =

    let rec remove_help k d d_build orig_d=
      match d with
      |[]->orig_d
      |h::t-> if Key.compare k (fst h)= `LT then orig_d else if Key.compare (fst h) k=`EQ
      then List.rev_append d_build t else  remove_help k t (h::d_build) orig_d
   in let d_good=rep_ok d in
    remove_help k (d_good) [] d_good


  let find k d =
      (*
      let d_correct = rep_ok d in
      if List.mem_assoc k d_correct  then Some (List.assoc k d_correct ) else None
      *)
    let rec find_help k d=
      match d with
      |[]->None
      |h::t-> if Key.compare k (fst h)=`LT then None else if Key.compare k (fst h)=`EQ
        then Some (snd h) else find_help k t
   in find_help k (rep_ok d)

  let member k d =
    match find k d with
    |None-> false
    |Some v-> true

  let choose d =
    let d_correct = rep_ok d in
    match d_correct with
    |[] -> None
    |h::t -> Some h

  let to_list d =
    rep_ok d

  let rec fold f init d =
    if d=rep_ok d then
    match d with
    |[]->init
    |h::t->fold f (f (fst h) (snd h) init) t
    else failwith "buns"

  let expose_tree d =
    failwith "not a 2-3 tree"

  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)

end

module MakeTreeDictionary (K : Comparable) (V : Formattable) = struct
  module Key = K
  module Value = V
  type key = K.t
  type value = V.t

  type t = (key,value) tree23

(**[check_sym] returns 1 if d is either a 2 node or a 3 node, 0 if it is
  * neither
  *requries: d needs to be a valid tree23
  *)

  let rec check_sym d=
    match d with
  |Leaf -> 0
  |Twonode{left2;value;right2}-> 1
  |Threenode {left3;lvalue;middle3;rvalue;right3}->1

(**[check_bal] returns true if d and all its subtree's each have either
  * all nodes as children or all leafs as children
  *)
  let rec check_bal d=
    match d with
    |Leaf -> true
    |Twonode{left2;value;right2}-> if check_sym left2=check_sym right2
      then check_bal left2 && check_bal right2 else false
    |Threenode {left3;lvalue;middle3;rvalue;right3}->
      if check_sym left3=check_sym right3 &&check_sym left3=check_sym middle3
      then check_bal left3 &&check_bal right3 && check_bal middle3
      else false

(**[path_length] returns the length of the path to the longest left path
  * or the longest right path in a tree
  *requires: t needs to be an integer of either 0 or 2
  *)
  let rec path_length d t=
  match d with
  |Leaf->0
  |Twonode {left2;value;right2} when t=0->1+path_length left2 0
  |Twonode {left2;value;right2} when t=2->1+path_length right2 1
  |Threenode {left3;lvalue;middle3;rvalue;right3} when t=0-> 1+path_length left3 0
  |Threenode {left3;lvalue;middle3;rvalue;right3} when t=2-> 1+path_length right3 2
  |_->failwith "not right"

(**[check_path] true if the path length of the all left path is the same
  * as the length of the all right path
  *)
  let check_path d=
  match d with
  |Leaf -> true
  |Twonode {left2;value;right2} ->
    if path_length left2 0 =path_length right2 2
    then true else false
  |Threenode {left3;lvalue;middle3;rvalue;right3}->
    if path_length left3 0 =path_length right3 2
    then true else false

(**[get_val]returns the value of the subtree of d specified by t
  * requires: d needs to be a 2 node ora 3 node and t needs to be an int
  *of either 0 or 2
  *)
  let get_val d t=
    match d with
    |Leaf->failwith"once agian"
    |Twonode {left2;value;right2} -> value
    |Threenode {left3;lvalue;middle3;rvalue;right3} when t=0->lvalue
    |Threenode {left3;lvalue;middle3;rvalue;right3} when t=2->rvalue
    |_->failwith "not right"

(**[key look]returns true if the keys of the tree are properly placed with
  *lesser keys being to the right of greater keys
  *)
  let rec key_look d=
    match d with
    |Leaf -> true
    |Twonode {left2;value;right2} ->
      if Key.compare (fst value) (fst (get_val right2 0))=`LT
      && Key.compare (fst(get_val left2 2)) (fst value)=`LT
      then key_look left2 && key_look right2
      else false
    |Threenode {left3;lvalue;middle3;rvalue;right3}->
      if Key.compare (fst lvalue) (fst rvalue) =`LT
      &&  Key.compare (fst (get_val left3 2)) (fst lvalue) =`LT
      && Key.compare  (fst lvalue) (fst (get_val middle3 0))=`LT
      && Key.compare (fst(get_val middle3 2)) (fst rvalue) =`LT
      && Key.compare (fst rvalue) (fst(get_val right3 0))  =`LT
    then key_look left3 &&  key_look right3 && key_look middle3
    else false


  let rep_ok d =
    if check_bal d && check_path d && key_look d then d
    else failwith"incorrect rep invariant"

  let empty = Leaf

  let is_empty d =
    if d=Leaf then true else false

  let rec size d=
    match d with
    |Leaf -> 0
    |Twonode {left2;value;right2} ->
    1 + size left2 +size right2
    |Threenode {left3;lvalue;middle3;rvalue;right3}->2 + size left3 +size middle3 +size right3


(**[get_sub] returns the value of the subtree of a 2 node
  * [requires] partial: d can only be a 2 node
  *raises: failwith "incorrect use" if
  *)
  let get_val2 d=
    match d with
    |Leaf ->failwith "incorrect use"
    |Twonode{left2;value;right2}-> value
    |Threenode {left3;lvalue;middle3;rvalue;right3}->failwith "incorrect use"

(**[get_sub] returns the subtree of d specified by t, with 0 corresponding
  * to left, 1 to middle and 2 to right
  * [requires] partial: d ncan only be a 2 node or a 3 node and t can only be
  * an int of either 0 or 2 for 2 nodes or 0,1,2 for 3 nodes
  * raise's "wrong use" if a invalid d or t is called
  *)
  let get_sub d t=
    match d with
    |Leaf->failwith"wrong use"
    |Twonode {left2;value;right2} when t=0->left2
    |Twonode {left2;value;right2} when t=2->right2
    |Threenode {left3;lvalue;middle3;rvalue;right3} when t=0->left3
    |Threenode {left3;lvalue;middle3;rvalue;right3} when t=1->middle3
    |Threenode {left3;lvalue;middle3;rvalue;right3} when t=2->right3
    |_->failwith "wrong use"


(**[terminal_2] returns true if the tree23 [d] passed in is a a 2 node
  *with all subtree's being Leafs, else false
  * [requires] d needs to be a valid tree23
  *)
  let terminal_2 d=
    match d with
    |Leaf -> false
    |Twonode {left2;value;right2} ->if left2=Leaf && right2=Leaf
      then true else false
    |Threenode {left3;lvalue;middle3;rvalue;right3}->false

(**[terminal_3] returns true if the tree23 [d] passed in is a a 3 node
  *with all subtree's being Leafs, else false
  * [requires] d needs to be a valid tree23
  *)
  let terminal_3 d=
    match d with
    |Leaf -> false
    |Twonode {left2;value;right2} ->false
    |Threenode {left3;lvalue;middle3;rvalue;right3}->
      if left3=Leaf && middle3=Leaf && right3=Leaf then true else false


 let check_tree k d=
    match k with
    |KickTree Kicknode {left2=l;value=v;right2=r}->
      (match d with
      |Leaf -> failwith"nope"
      |Twonode {left2;value;right2} ->
        if Key.compare (fst v) (fst value)=`LT
        then Tree23(Threenode{left3=(get_sub l 0); lvalue=(get_val2 l);
                              middle3 = (get_sub l 2); rvalue = value;
                              right3=right2})
        else Tree23(Threenode{left3=left2; lvalue=value;
                              middle3=(get_sub r 0); rvalue=(get_val2 r);
                              right3=(get_sub r 2)})
      |Threenode {left3;lvalue;middle3;rvalue;right3}->
        if Key.compare (fst v) (fst lvalue)=`LT
        then KickTree(Kicknode{left2=Twonode{left2=l;value=v;right2=r};
                                 value= lvalue;
                                 right2= Twonode {left2=middle3; value=rvalue; right2= right3}})
        else if Key.compare (fst v) (fst lvalue)=`GT
             && Key.compare (fst v) (fst rvalue)=`LT
        then KickTree(Kicknode{left2= Twonode {left2=left3; value= lvalue; right2=l};
                                value=v;
                                right2= Twonode {left2=r;value= rvalue; right2=right3}})
        else  KickTree(Kicknode{left2=Twonode {left2=left3; value=lvalue; right2= middle3};
                                value= rvalue;
                                right2=Twonode{left2=l;value=v;right2=r}}))
    |Tree23 Leaf-> k
    |Tree23 Twonode{left2;value;right2}->k
    |Tree23 Threenode{left3;lvalue;middle3;rvalue;right3}->k


  let  insert k v d =
    let rec insert_help k v d=
      match d with
      |Leaf -> Tree23(Twonode {left2=Leaf;value=(k,v);right2=Leaf})
      |Twonode {left2=l;value=vals;right2=r}->
        if Key.compare k (fst vals)=`LT && (terminal_2 d)
        then Tree23(Threenode{left3=Leaf; lvalue= (k,v);
                              middle3=Leaf; rvalue=vals;
                              right3=Leaf})
        else if Key.compare k (fst vals)=`LT
        then  check_tree (insert_help k v l) d
        else if Key.compare k (fst vals)=`EQ
        then Tree23(Twonode {left2=l; value=(k,v); right2=r})
        else if Key.compare k (fst vals)=`GT && (terminal_2 d)
        then Tree23(Threenode {left3=Leaf;lvalue=vals;
                               middle3=Leaf;rvalue=(k,v);
                               right3=Leaf})
        else check_tree(insert_help k v r) d
      |Threenode {left3=l;lvalue=lv;middle3=m;rvalue=rv;right3=r}->
        if Key.compare k (fst lv)= `LT && (terminal_3 d)
        then KickTree(Kicknode {left2=Twonode {left2=Leaf;value=(k,v);right2=Leaf};
                                value=lv;
                                right2=r})
        else if Key.compare k (fst lv)= `LT
         then  check_tree(insert_help k v l) d
        else if Key.compare k (fst lv)= `EQ
        then Tree23(Threenode {left3=l;lvalue=(k,v);
                               middle3=m;rvalue=rv;
                               right3=r})
        else if Key.compare k (fst lv)= `GT && Key.compare k (fst rv)= `LT
             && (terminal_3 d)
        then KickTree(Kicknode {left2=Twonode {left2=Leaf;value=lv;right2=Leaf};
                                value =(k,v);
                                right2=Twonode {left2=Leaf;value=rv;right2=Leaf}})

        else if Key.compare k (fst lv)= `GT && Key.compare k (fst rv)= `LT
        then check_tree( insert_help k v m) d
        else if Key.compare k (fst rv)= `EQ
        then Tree23(Threenode {left3=l;lvalue=lv;
                               middle3=m;rvalue=(k,v);
                               right3=r})
        else if Key.compare k (fst rv)= `GT && (terminal_3 d)
        then KickTree(Kicknode {left2= Twonode{left2=Leaf;value=lv;right2=Leaf};
                                value=rv;
                                right2=Twonode {left2=Leaf;value=(k,v);right2=Leaf}})
        else check_tree(insert_help k v r) d

   in match insert_help k v d with
    |KickTree Kicknode {left2;value;right2}-> Twonode{left2;value;right2}
    |Tree23 Leaf-> Leaf
    |Tree23 Twonode{left2;value;right2}->Twonode{left2;value;right2}
    |Tree23 Threenode{left3;lvalue;middle3;rvalue;right3}->
    Threenode{left3;lvalue;middle3;rvalue;right3}

  let remove k d =
    raise Unimplemented

  let rec find k d =
    match d with
    |Leaf -> None
    |Twonode {left2;value;right2} ->if Key.compare k (fst value)=`LT
      then find k left2 else if Key.compare k (fst value)=`EQ then Some (snd value)
      else find k right2
    |Threenode {left3;lvalue;middle3;rvalue;right3}->if Key.compare k (fst lvalue)=`LT
      then find k left3 else if Key.compare k (fst lvalue)=`EQ then Some (snd lvalue)
      else if Key.compare k (fst rvalue)=`LT then find k middle3 else if Key.compare k (fst rvalue)=`EQ
      then Some (snd rvalue) else find k right3



  let member k d =
    if find k d = None then false else true

  let rec choose d =
    match d with
    |Leaf -> None
    |Twonode {left2;value;right2} ->if terminal_2 d then Some value
    else choose left2
    |Threenode {left3;lvalue;middle3;rvalue;right3}-> if terminal_3 d
    then Some lvalue else choose left3

  let to_list d =
    let rec to_list_help d accum=
      match d with
      |Leaf -> accum
      |Twonode {left2;value;right2} ->if terminal_2 d then value::accum

        else to_list_help left2(value::(to_list_help right2 accum))
      |Threenode {left3;lvalue;middle3;rvalue;right3}-> if terminal_3 d
        then lvalue::rvalue::accum
        else (to_list_help left3(lvalue::(to_list_help middle3 (rvalue::(to_list_help right3 accum)))))
  in to_list_help d []

  let expose_tree d =
    d

  let rec fold f init d =
      match d with
      |Leaf -> init
      |Twonode {left2;value;right2} ->if terminal_2 d then f (fst value)(snd value) init

        else fold f ((fold f (f (fst value)(snd value)(fold f init left2))) d)right2
      |Threenode {left3;lvalue;middle3;rvalue;right3}-> if terminal_3 d
      then f (fst rvalue) (snd rvalue) (f (fst lvalue)(snd lvalue) init)
        else fold f (f (fst rvalue)(snd rvalue)(fold f (f (fst lvalue)(snd lvalue)(fold f init left3)) middle3)) right3


  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)

end

module type Set = sig
  module Elt : Comparable
  type elt = Elt.t
  type t
  val rep_ok : t  -> t
  val empty : t
  val is_empty : t -> bool
  val size : t -> int
  val insert : elt -> t -> t
  val member : elt -> t -> bool
  val remove : elt -> t -> t
  val union : t -> t -> t
  val intersect : t -> t -> t
  val difference : t -> t -> t
  val choose : t -> elt option
  val fold : (elt -> 'acc -> 'acc) -> 'acc -> t -> 'acc
  val to_list : t -> elt list
  val format : Format.formatter -> t -> unit
end

module MakeSetOfDictionary (C : Comparable) (DM:DictionaryMaker) = struct
  module Elt = C
  type elt = Elt.t
  module F =struct
    type t = unit

    let format fmt d =
      Format.fprintf fmt "<abstr>"
     end
  module Dict= DM (C)(F)
      (*AF: The set {k1,k2,k2,..kn} contains the set of n keys ordered
             from least to greatest key with no duplicate keys.
        RI: All keys are orderd from least to greatest with no duplicate
            keys.
       *)

  type t = Dict.t

  let rep_ok s =
    Dict.rep_ok s

  let empty =
    Dict.empty

  let is_empty s =
     Dict.is_empty s
  let size s =
     Dict.size s

  let insert x s =
    Dict.insert x () s

  let member x s =
    Dict.member x s

  let remove x s =
    Dict.remove x s

  let choose s =
    match    Dict.choose s with
    |None-> None
    |Some (k,v)-> Some k

  let rec fold f init s =
    match choose s with
    |None-> init
    |Some k->fold f (f k init) (remove k s)


  let union s1 s2 =
   let rec union_helper s1 s2 =
    match choose s2 with
    |None->s1
    |Some h-> union_helper (insert h s1) (remove h s2)
    in union_helper s1 s2


  let intersect s1 s2 =
   let rec intersect_helper s1 s2 accum=
    match choose s2 with
    |None->accum
    |Some h-> if (member h s1) then intersect_helper s1 (remove h s2) (insert h  accum)
      else intersect_helper s1 (remove h s2) accum
   in intersect_helper s1 s2 empty


  let difference s1 s2 =

    let rec difference_helper s1 s2 =
    match choose s2 with
    |None->s1
    |Some h->if (member h s1) then difference_helper (remove h s1) (remove h s2)
      else difference_helper s1 (remove h s2)

    in difference_helper s1 s2

  let rec to_list s =
    List.map (fst)( Dict.to_list s )
  let format fmt d =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end
