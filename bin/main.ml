let rec last list =
  match list with
  | [] -> None
  | [ x ] -> Some x
  | _ :: tl -> last tl
;;

let rec last_two list =
  match list with
  | [] | [ _ ] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tail -> last_two tail
;;

let rec nth list idx =
  match list with
  | [] -> raise (Failure "nth")
  | hd :: _ when idx = 0 -> hd
  | _ :: tail -> nth tail (idx - 1)
;;

let length list =
  let rec aux x acc =
    match x with
    | [] -> acc
    | _ :: tail -> aux tail (acc + 1)
  in
  aux list 0
;;

let rev list =
  let rec aux l out =
    match l with
    | [] -> out
    | hd :: tl -> aux tl (hd :: out)
  in
  aux list []
;;

let is_palindrome list = rev list = list

type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
  let rec aux acc = function
    | [] -> acc
    | One x :: tail -> aux (x :: acc) tail
    | Many x :: tail -> aux (aux acc x) tail
  in
  rev (aux [] list)
;;

let compress list =
  let rec aux acc = function
    | [] -> acc
    | [ x ] -> x :: acc
    | x :: y :: t when x = y -> aux acc (y :: t)
    | x :: y :: t -> aux (x :: acc) (y :: t)
  in
  rev (aux [] list)
;;

let () =
  print_endline "";
  let list = [ "x"; "a"; "m"; "a"; "x" ] in
  let _ =
    match last list with
    | Some x -> Printf.printf "Last: %s\n" x
    | None -> print_endline "Last: None"
  in
  let _ =
    match last_two list with
    | Some (x, y) -> Printf.printf "Last two: (%s, %s)\n" x y
    | None -> print_endline "Last two: None"
  in
  let second = nth list 2 in
  Printf.printf "Nth (2): %s\n" second;
  let len = length list in
  Printf.printf "Length: %d\n" len;
  Printf.printf "Reversed: ";
  let reversed = rev list in
  List.iter (Printf.printf "%s ") reversed;
  print_endline "";
  let palindrome = is_palindrome list in
  Printf.printf "Is palindrome: %b\n" palindrome;
  let flat = flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ] in
  Printf.printf "Flatten: ";
  List.iter (Printf.printf "%s ") flat;
  print_endline "";
  let duplicates =
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
  in
  let compressed = compress duplicates in
  Printf.printf "Compress: ";
  List.iter (Printf.printf "%s ") compressed;
  print_endline ""
;;
