open Core

type draw_order = int list

module Board = struct
  let board_rows board =
    List.groupi board ~break:(fun idx _ _ -> idx % 5 = 0)

  let board_columns board =
    let rows = board_rows board in
    List.transpose_exn rows

  let find_unmarked board called =
    let board_set = Set.of_list (module Int) board in
    let called_set = Set.of_list (module Int) called in
    Set.diff board_set called_set

  let find_win rows called =
    List.exists rows ~f:(fun row ->
      let row_set = Set.of_list (module Int) row in
      let called_set = Set.of_list (module Int) called in
      Set.is_subset row_set ~of_:called_set
    )

  let row_win board called =
    let rows = board_rows board in
    find_win rows called

  let column_win board called =
    let columns = board_columns board in
    find_win columns called

  let is_winner board called =
    (row_win board called) || (column_win board called)
end

module Print = struct
  let print_board board =
    let rows = Board.board_rows board in
    List.iter rows ~f:(fun row ->
      let printable = String.concat ~sep:" " (List.map row ~f:(fun x -> sprintf "%d" x)) in
      printf "%s\n" printable
    );
    printf "\n"
end


module Parse = struct
  let parse_line line_text =
    let split = String.split ~on:' ' line_text in
    let filtered = List.filter split ~f:(fun x -> String.(<>) x "") in
    List.map filtered ~f:Int.of_string

  let rec parse_board lines board boards call_order =
    match (List.length board), lines with
    | 25, _ -> parse_boards lines (board :: boards) call_order
    | _, hd :: tl ->
        parse_board tl (board @ (parse_line hd)) boards call_order
    | _, _ -> failwith "Ran out of input for boards"

  and parse_boards lines boards call_order =
    match lines with
    | [] -> (List.rev boards), call_order
    | _ -> parse_board lines [] boards call_order

  let parse_order lines =
    match lines with
    | line :: rest -> 
      let split = String.split line ~on:',' in
      let call_order = List.map split ~f:Int.of_string in
      parse_boards rest [] call_order
    | _ -> failwith "Ran out of input"

  let parse lines =
    parse_order lines
end

(* main code *)

let find_winners boards called =
  List.partition_tf boards ~f:(fun board -> Board.is_winner board called)

let rec order_winners_inner winning_boards remaining_boards called_numbers remaining_numbers =
  match remaining_numbers, remaining_boards with
  | [], _ | _, [] -> (List.rev winning_boards)
  | hd :: tl, _ -> 
    let next_winners, next_remaining = find_winners remaining_boards called_numbers in
    let winners_with_called = List.map next_winners ~f:(fun winner -> winner, called_numbers) in
    order_winners_inner (winners_with_called @ winning_boards) next_remaining (hd :: called_numbers) tl

let order_winners boards call_order =
  order_winners_inner [] boards [] call_order

let winner_hash board call_list =
  match call_list with
  | winning_call :: _ ->
    let unmarked = Board.find_unmarked board call_list in
    let sum_unmarked = Set.fold unmarked ~f:(+) ~init:0 in
    winning_call * sum_unmarked
  | _ -> failwith "Impossible win"
  
 
let challenge_1 data =
  let boards, call_order = Parse.parse data in
  let win_order = order_winners boards call_order in
  let winner, calls = List.hd_exn win_order in
  printf "WINNER IS\n";
  Print.print_board winner;
  winner_hash winner calls

let challenge_2 data =
  let boards, call_order = Parse.parse data in
  let win_order = order_winners boards call_order in
  let winner, calls = List.hd_exn (List.rev win_order) in
  printf "LOSER IS\n";
  Print.print_board winner;
  winner_hash winner calls

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
  printf "Challenge 1: %d\n" (challenge_1 data);
  let test_result_2 = challenge_2 sample in
  assert (test_result_2 = 1924);
  printf "Challenge 2: %d\n" (challenge_2 data);
