open Core

type command_name =
  | Forward
  | Down
  | Up

type command = command_name * int
type vector = int * int

let parse_command cmd =
  let command, distance = match String.split cmd ~on:' ' with

    | command :: distance :: [] -> command, distance
    | _ -> failwith "Too long a string"
  in
  let distance_int = Int.of_string distance in
  (
    match command with
    | "forward" -> Forward
    | "down" -> Down
    | "up" -> Up
    | _ -> failwith "Unrecognised command"
  ), distance_int

let command_to_vector command =
  let command_name, magnitude = command in
  match command_name with
  | Forward -> magnitude, 0
  | Down -> 0, magnitude
  | Up -> 0, -magnitude

let vector_add (x1, y1) (x2, y2) = (x1 + x2), (y1 + y2)

let challenge_1 data =
  let commands = List.map data ~f:parse_command in
  let vectors = List.map commands ~f:command_to_vector in
  let x, y = List.fold vectors ~f:vector_add ~init:(0,0) in
  x * y


let apply_aimed_command acc command =
  let command_name, magnitude = command in
  let x, y, aim = acc in
  match command_name with
  | Forward -> x + magnitude , y + (magnitude * aim), aim
  | Down -> x, y, aim + magnitude
  | Up -> x, y, aim - magnitude

let challenge_2 data =
  let commands = List.map data ~f:parse_command in
  let x, y, _ = List.fold commands ~f:apply_aimed_command ~init:(0,0,0) in
  x * y

let () =
  let data = In_channel.read_lines "day2.txt" in
  let sample = [
    "forward 5";
    "down 5";
    "forward 8";
    "up 3";
    "down 8";
    "forward 2"
  ] in
  let test_result = challenge_1 sample in
  assert (test_result = 150);
  printf "Challenge 1: %d\n" (challenge_1 data);
  let test_result_2 = challenge_2 sample in
  assert (test_result_2 = 900);
  printf "Challenge 2: %d\n" (challenge_2 data)
