open Core

type draw_order = int list

type bingo_table = int list list

(* Parsing subsystem *)

let parse_line line_text =
  let split = String.split ~on:' ' line_text in
  let filtered = List.filter split ~f:(fun x -> String.(<>) x "") in
  List.map filtered ~f:Int.of_string

let rec parse_board lines board boards call_order =
  match lines with
  | _ when (List.length board) = 5 -> parse_boards lines ((List.rev board) :: boards) call_order
  | head :: rest -> (
      let line = parse_line head in
      parse_board rest (line :: board) boards call_order
  )
  | _ -> failwith "Ran out of input"

and parse_boards lines boards call_order =
  match lines with
  | [] -> boards, call_order
  | "" :: rest -> parse_board rest [] boards call_order
  | x :: _ -> failwith (sprintf "Unexpected input %s" x)

let parse_order lines =
  match lines with
  | line :: rest -> 
    let split = String.split line ~on:',' in
    let call_order = List.map split ~f:Int.of_string in
    parse_boards rest [] call_order
  | _ -> failwith "Ran out of input"

let parse lines =
  parse_order lines

(* main code *)
let row_win board called =
  List.exists board ~f:(fun row ->
    let row_set = Set.of_list (module Int) row in
    let called_set = Set.of_list (module Int) called in
    Set.is_subset row_set ~of_:called_set
  )

let column_win board called =
  let rotated = match board with
  | a :: b :: c :: d :: e :: [] ->
    List.map (List.range 0 5) ~f:(fun idx ->
        [
          List.nth_exn a idx;
          List.nth_exn b idx;
          List.nth_exn c idx;
          List.nth_exn d idx;
          List.nth_exn e idx
        ]
    )
  | _ -> failwith "Board does not have the right number of rows"
  in
  row_win rotated called

let is_winner board called =
  (row_win board called) || (column_win board called)

let find_unmarked board called =
  let concat = List.concat board in
  let board_set = Set.of_list (module Int) concat in
  let called_set = Set.of_list (module Int) called in
  Set.diff board_set called_set

let rec find_winner boards called remaining =
  let winner = List.find boards ~f:(fun board ->
      is_winner board called
  ) in
  match winner, remaining with
  | Some w, _ ->
    let unmarked = find_unmarked w called in
    let sum_unmarked = Set.fold unmarked ~f:(+) ~init:0 in
    w, (List.hd_exn called), sum_unmarked
  | None, [] -> failwith "No winner"
  | None, next :: new_remaining -> (
      find_winner boards (next :: called) new_remaining
  )

let challenge_1 data =
  let boards, call_order = parse data in
  let _winner, win_call, sum_unmarked = find_winner boards [] call_order in
  win_call * sum_unmarked

let () =
  let data = In_channel.read_lines "day4.txt" in
  let sample = ["7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1";
"";
"    22 13 17 11  0";
"     8  2 23  4 24";
"    21  9 14 16  7";
"     6 10  3 18  5";
"     1 12 20 15 19";
"";
"     3 15  0  2 22";
"     9 18 13 17  5";
"    19  8  7 25 23";
"    20 11 10 24  4";
"    14 21 16 12  6";
"";
"    14 21 17 24  4";
"    10 16 15  9 19";
"    18  8 23 26 20";
"    22 11 13  6  5";
"     2  0 12  3  7";
  ] in
  let test_result = challenge_1 sample in
  assert (test_result = 4512);
  printf "Challenge 1: %d\n" (challenge_1 data)
