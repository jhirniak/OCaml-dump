let rec explode s =
  let l = String.length s in
  match l with
    | 0 -> []
    | _ -> (String.get s 0) :: (explode (String.sub s 1 (l - 1)))

let char_sort l = List.sort Char.compare l

let sort_str s = List.fold_right (List.map (char_sort (explode s)) Char.escaped) ~f:(^) ~init:("")

let bags = Hashtbl.create


