open Core

module Point = struct

  module T = struct
    type t = int * int

    let string_of (x, y) =
      sprintf "%d,%d" x y

    let of_string str =
      let split = String.split ~on:',' str in
      match split with
      | first :: second :: [] -> (Int.of_string first), (Int.of_string second)
      | _ -> failwith "Bad input coord"

    let equal p1 p2 =
      match p1, p2 with
      | (x1, y1), (x2, y2) when x1 = x2 && y1 = y2 -> true
      | _ -> false


    let compare (x1, y1) (x2, y2) =
      let x = Int.compare x1 x2 in
      let y = Int.compare y1 y2 in
      match x, y with
      | 0, 0 -> 0
      | 0, y -> y
      | x, _ -> x

    let sexp_of_t (x1, y1) : Sexp.t =
      List [ Atom (sprintf "%d" x1); Atom (sprintf "%d" y1) ]

  end
  include T
  include Comparator.Make(T)
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

  let magnitude ((x1, y1), (x2, y2)) =
    let hyp_squared = ((Float.of_int (x1 - x2)) ** 2.0) +. ((Float.of_int (y1 - y2)) ** 2.0) in
    Float.sqrt hyp_squared

  let rec points_crossed_inner x1 y1 x2 y2 dx dy sx sy err out =
    let new_out = (x1, y1) :: out in
    if Point.equal (x1, y1) (x2, y2) then
      out
    else
      let e2 = 2 * err in
      let err, x1 = if e2 >= dy then 
          (err + dy), (x1 + sx)
        else
          err, x1 in
      let err, y1 = if e2 <= dx then
          (err + dx), (y1 + sy)
        else
          err, y1
      in
        points_crossed_inner x1 y1 x2 y2 dx dy sx sy err new_out
    

  let points_crossed ((x1, y1), (x2, y2)) =
    (* Bresenham's Line Algorithm *)
    let dx = Int.abs (x2 - x1) in
    let sx = if x1 < x2 then 1 else -1 in
    let dy = -(Int.abs (y2 - y1)) in
    let sy = if y1 < y2 then 1 else -1 in
    let err = dx + dy in
    (x2, y2) :: (points_crossed_inner x1 y1 x2 y2 dx dy sx sy err [])
end

module Parser = struct
  let parse input =
    List.map input ~f:Line.of_string
end

let rec find_collisions lines_as_sets so_far =
  match lines_as_sets with
  | line :: rest ->
    let collisions = List.fold rest ~f:(fun acc other_line ->
      Set.union acc (
        Set.inter line other_line
      )
    ) ~init:so_far in
    find_collisions rest collisions
  | _ -> so_far

let challenge_1 input =
  let parsed = Parser.parse input in
  let straight = List.filter parsed ~f:Line.is_straight in
  let lines_as_sets = List.map straight ~f:(fun l -> Set.of_list (module Point) (Line.points_crossed l)) in
  let collisions = find_collisions lines_as_sets (Set.empty (module Point)) in
  Set.length collisions

let challenge_2 input =
  let parsed = Parser.parse input in
  let lines_as_sets = List.map parsed ~f:(fun l -> Set.of_list (module Point) (Line.points_crossed l)) in
  let collisions = find_collisions lines_as_sets (Set.empty (module Point)) in
  Set.length collisions

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
  printf "Challenge 1: %d\n" (challenge_1 data);
  let test_result_2 = challenge_2 sample in
  assert (test_result_2 = 12);
  printf "Challenge 2: %d\n" (challenge_2 data)
