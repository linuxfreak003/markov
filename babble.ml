open Core.Std;;

let table = String.Table.create ();;

let usage () =
  printf "Usage: %s <filename> <length (default 3)> <length of sentence(default 15)>\n" Sys.argv.(0);
  exit 0
;;

let getFile filename () =
  match Sys.is_file filename with
    | `Yes -> In_channel.read_all filename
    | _ -> printf "\"%s\" not a file!\n" filename; usage ()
;;

let addSpace a b = a ^ " " ^ b;;

let isAlNum c = Char.is_alphanum c || Char.is_whitespace c;;

let rec next_n n lst = match (lst, n) with
  | ([],_) | (_,0) -> []
  | (x::xs, n) -> x :: (next_n (n-1) xs)
;;

let rec parseFile file len () = match file with
    | [] -> ()
    | x::xs ->
      let nextStuff = next_n len xs in
      let stuff = List.fold_left ~init:"" ~f:addSpace nextStuff in
      let lower = String.lowercase stuff in
      let str = String.filter ~f:isAlNum lower in
      Hashtbl.add_multi table x str;
      parseFile xs len ()
;;

let is_whitespace = function
  | "" | "\n" | " " | "\r" | "\t" -> true
  | _ -> false
;;

let rec babble word = function
  | 0 -> ()
  | n -> 
      let words = match Hashtbl.find table word with
        | None | Some [] -> exit 0
        | Some xs -> List.nth_exn xs (Random.int (List.length xs))
      in
      let new_word = List.hd_exn (List.rev (String.split ~on:' ' words)) in
      printf "%s" words;
      babble new_word (n-1)
;;

let () =
  at_exit (fun () -> printf "\n");
  Random.self_init ();
  let length = Array.length Sys.argv in
  let filename = if length >= 2 then Sys.argv.(1) else usage (); in
  let file = getFile filename () in
  let lst = String.split_on_chars ~on:[' ';'\t';'\n';'\r'] file in
  let len = if length >= 3 then int_of_string Sys.argv.(2) else 3 in
  let sentence = if length >= 4 then int_of_string Sys.argv.(3) else 15 in
  parseFile (List.filter ~f:(fun x -> not (is_whitespace x) ) lst) len ();
  printf "The";
  babble "the" sentence;
  printf "\n";
;;
