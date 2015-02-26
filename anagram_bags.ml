let rec explode s =
  let l = String.length s in
  match l with
    | 0 -> []
    | _ -> (String.get s 0) :: (explode (String.sub s 1 (l - 1)))

let char_sort l = List.sort Char.compare l

let sort_str s = List.fold_right (^) (List.map Char.escaped (char_sort (explode s))) ""

let rec get_anagrams l =
  let bags = Hashtbl.create (List.length l) in
  match l with
    | []     -> bags
    | (h::t) -> let ord_word = sort_str h in
                  Hashtbl.add bags ord_word h;
                  get_anagrams t

let print_bags bags = Hashtbl.iter
                        (fun k v -> Printf.printf "\n[ ";
                          List.iter (fun e -> Printf.printf "%s " e) (Hashtbl.find_all bags k);
                          Printf.printf " ]\n\n")
                        bags

let words = [ "alal"; "lala"; "kot"; "tok"; "cat"; "tac"; "bison"; "sonib"; "nosbi" ]

let () =
  let bags = get_anagrams words in
    Hashtbl.iter (fun k v -> List.iter (fun e -> print_string e) (Hashtbl.find_all bags k)) bags
