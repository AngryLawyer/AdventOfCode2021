open Core

module Point = struct
  let string_of (x, y) =
    sprintf "%d,%d" x y

  let of_string str =
    let split = String.split ~on:',' str in
    match split with
    | first :: second :: [] -> (Int.of_string first), (Int.of_string second)
    | _ -> failwith "Bad input coord"
end

module Line = struct
  let string_of (p1, p2) =
    sprintf "%s -> %s" (Point.string_of p1) (Point.string_of p2)

  let of_string line =
    let split = Str.split (Str.regexp " -> ") line in
    match split with
    | first :: second :: [] -> (Point.of_string first), (Point.of_string second)
    | _ -> failwith "Bad input line"

  let is_straight ((x1, y1), (x2, y2)) =
    (x1 = x2) || (y1 = y2)
    
end

module Parser = struct
  let parse input =
    List.map input ~f:Line.of_string
end

let challenge_1 input =
  let parsed = Parser.parse input in
  let straight = List.filter parsed ~f:Line.is_straight in
  List.iter straight ~f:(fun line -> printf "%s\n" (Line.string_of line));
  0

let () =
  let data = In_channel.read_lines "day5.txt" in
  let sample = [
    "0,9 -> 5,9";
    "8,0 -> 0,8";
    "9,4 -> 3,4";
    "2,2 -> 2,1";
    "7,0 -> 7,4";
    "6,4 -> 2,0";
    "0,9 -> 2,9";
    "3,4 -> 1,4";
    "0,0 -> 8,8";
    "5,5 -> 8,2"
  ] in
  let test_result = challenge_1 sample in
  assert (test_result = 5);
  printf "Challenge 1: %d\n" (challenge_1 data)
