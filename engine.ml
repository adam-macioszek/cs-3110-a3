exception Unimplemented
open Unix
module type Engine = sig
  type idx
  val index_of_dir : string -> idx
  val to_list : idx -> (string * string list) list
  val or_not  : idx -> string list -> string list -> string list
  val and_not : idx -> string list -> string list -> string list
  val format : Format.formatter -> idx -> unit
end

module MakeEngine
    (S:Data.Set with type Elt.t = string)
    (D:Data.Dictionary with type Key.t = string
                        and type Value.t =S.t)
  : Engine
=
struct
  type idx = D.t


   (**returns: the dictionary with all the strings in lst inserted into
     *diction
     *)
    let rec dic_builder lst path diction=
      match lst with
      |[]->diction
      |h::t-> match D.find h diction with
        |None->dic_builder t path (D.insert (String.lowercase_ascii h) (S.insert path S.empty) diction)
        |Some v-> if S.member path v then dic_builder t path diction else dic_builder t path ( D.insert (String.lowercase_ascii h) (S.insert path v) diction)

(**returns: the string of words taken out from the regexp match of an 
     * input line
     *)
    let rec words lst new_lst=
      match lst with
      |[]->new_lst
      |h::t-> match h with
        |Str.Text s-> words t new_lst
        |Str.Delim s-> words t (s::new_lst)

    let rec file_parse file path diction =
      let word = Str.regexp"[A-Za-z0-9]+[^  \t\n\r]*[A-Za-z0-9]*" in
      try file_parse file path (dic_builder (words( Str.full_split (word) (input_line file)) [] ) path diction)
      with End_of_file-> diction

    let rec entry_parse dir diction lst =
      match lst with
      |[]->diction
      |h::t-> if Str.string_match (Str.regexp ".+\\.\\txt$") h 0 then file_parse (open_in (dir^Filename.dir_sep^h)) (dir^Filename.dir_sep^h) diction
        else entry_parse dir diction t


    let rec  entry_list dir_hand lst=
      try  let entry= Unix.readdir dir_hand in
        entry_list dir_hand (entry::lst)

      with End_of_file -> Unix.closedir dir_hand; lst

    let index_of_dir d =
      let diction= D.empty in
      try entry_parse d diction (entry_list (Unix.opendir d) [])

      with e-> raise Not_found

  let to_list idx =
      D.fold (fun k v accum->(k,(S.to_list v))::accum) [] idx

  let or_not idx ors nots =

    let or_files=  D.fold (fun k v accum-> if List.mem k ors then S.union v accum else accum ) S.empty idx in
    let not_files= D.fold (fun k v accum-> if List.mem k nots then S.union v accum else accum ) S.empty idx in
    S.(difference or_files not_files |>to_list)

  let and_not idx ands nots =
    let and_files=  D.fold (fun k v accum-> if List.mem k ands && S.is_empty accum then S.union v accum
      else if  List.mem k ands then S.intersect v accum else accum ) S.empty idx in
     let not_files= D.fold (fun k v accum-> if List.mem k nots then S.union v accum else accum ) S.empty idx in
      S.(difference and_files not_files|>to_list)

  let format fmt idx =
    Format.fprintf fmt "<abstr>" (* TODO: improve if you wish *)
end



module Form:Data.Formattable =struct
  type t = string
  let format fmt d =
    Format.fprintf fmt "<abstr>"
end

module Comp:Data.Comparable with type t = string=struct
  type t= string
  let compare c1 c2= if compare c1 c2= ~-1 then `LT else if compare c1 c2=0 then `EQ else `GT
  let format fmt d =
    Format.fprintf fmt "<abstr>"
end

module ListDict = Data.MakeListDictionary (Comp) (Form)
module ListSet= Data.MakeSetOfDictionary (Comp) (Data.MakeListDictionary)

module TreeDict=Data.MakeTreeDictionary (Comp) (Form)
module TreeSet= Data.MakeSetOfDictionary (Comp) (Data.MakeTreeDictionary)

module ListEngine:Engine = MakeEngine (ListSet)  (Data.MakeListDictionary (Comp) (ListSet))
module TreeEngine = MakeEngine (TreeSet)  (Data.MakeTreeDictionary (Comp) (TreeSet))
