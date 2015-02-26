type 'a tree = 
  | Empty
  | Node of 'a * 'a tree * 'a tree

let rec preorder f = function
  | Empty          -> ()
  | Node (v, l, r) -> f v;
                      preorder f l;
                      preorder f r

let rec inorder f = function
  | Empty          -> ()
  | Node (v, l, r) -> inorder f l;
                      f v;
                      inorder f r

let rec postorder f = function
  | Empty          -> ()
  | Node (v, l, r) -> postorder f l;
                      postorder f r;
                      f v

let rec levelorder f x =
  let queue = Queue.create() in
    Queue.add x queue;
    while not (Queue.is_empty queue) do
      match Queue.take queue with
        | Empty          -> ()
        | Node (v, l, r) -> f v;
                            Queue.add l queue;
                            Queue.add r queue
    done

let tree = Node (1,
                 Node (2,
                       Node (4,
                             Node (7, Empty, Empty),
                             Empty),
                       Node (5, Empty, Empty)),
                 Node (3,
                       Node (6,
                             Node (8, Empty, Empty),
                             Node (9, Empty, Empty)),
                       Empty)
                 )

let () =
  Printf.printf "Preorder: ";   preorder   (Printf.printf "%d ") tree; print_newline ();
  Printf.printf "Inorder: ";    inorder    (Printf.printf "%d ") tree; print_newline ();
  Printf.printf "Postorder: ";  postorder  (Printf.printf "%d ") tree; print_newline ();
  Printf.printf "Levelorder: "; levelorder (Printf.printf "%d ") tree; print_newline ()
  
