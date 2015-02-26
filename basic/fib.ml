
let fmem = Hashtbl.create 50;;
Hashtbl.add fmem 1 1;
Hashtbl.add fmem 2 1;;

let rec fib n = if Hashtbl.mem fmem n then
    Hashtbl.find fmem n
  else begin
    Hashtbl.add fmem n (fib (n-1) + fib (n-2));
    Hashtbl.find fmem n
  end;;

let rec act () = let n = read_int () in
  if n <= 0 then () else
  begin
    (* print_int n; *)
    print_int (fib n);
    print_newline ();
    act ()
  end;;

act();;
