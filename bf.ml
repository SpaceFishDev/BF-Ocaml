let read_file filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let buffer = Bytes.create len in
  really_input ic buffer 0 len;
  close_in ic;
  buffer

let rec insert_at_end l i =
  match l with
  | [] -> [i]
  | h :: t -> h :: (insert_at_end t i)

let rec construct_list list number =
  if number > 0 then
    construct_list (insert_at_end list 0) (number - 1)
  else
    list

let ptr = ref 0

let rec update n memory operation x =
  match memory with
  | [] -> failwith "Index out of range"
  | hd :: tl ->
    if n = 0 then
      match operation with
      | "+" -> (hd + 1) :: tl
      | "-" -> (hd - 1) :: tl
      | "=" -> (x) :: tl
      | _ -> failwith "Invalid operation"
    else
      hd :: (update (n - 1) tl operation x)

let modify x op y =
  match op with
  | '+' -> x := !x + 1
  | '-' -> x := !x - 1
  | '=' -> x := y
  | _ -> ()

let stack = [0]

let rec remove_last lst =
  match lst with
  | [] -> failwith "Empty list"
  | [x] -> [], x
  | hd :: tl ->
    let new_tl, last = remove_last tl in
    hd :: new_tl, last


let rec execute str n memory stack =
  if n < String.length str then
    match str.[n] with
    | '+' -> execute str (n + 1) (update !ptr memory "+" 0) stack
    | '-' -> execute str (n + 1) (update !ptr memory "-" 0) stack
    | '>' ->
        modify ptr '+' 0;
        execute str (n + 1) memory stack
    | '<' ->
        modify ptr '-' 0;
        execute str (n + 1) memory stack
    | '[' ->
        execute str (n + 1) memory (insert_at_end stack n)
    | ']' ->
        if List.nth memory !ptr = 0 then
          execute str (n + 1) memory stack
        else
          let (new_stack, new_n) = remove_last stack in
          execute str (new_n) memory new_stack
    | '.' ->
        print_char (char_of_int (List.nth memory !ptr));
        execute str (n + 1) memory stack
    | ',' ->
        execute str (n + 1) (update !ptr memory "=" (int_of_char((read_line ()).[0]))) stack
    | _ ->
        execute str (n + 1) memory stack
  else
    memory

let rec print_list list =
  match list with
  | [] -> ()
  | head :: tail ->
      print_int head;
      print_char ' ';
      print_list tail

let () =
  let filename = "test.bf" in
  let lines = Bytes.to_string (read_file filename) in
  let _ = print_endline "--OUTPUT--\n" in
  let final_memory = execute lines 0 (construct_list [] (1024)) (stack) in
  print_endline "\n\n-- FINAL MEMORY --";
  print_list final_memory;
  print_char '\n'
