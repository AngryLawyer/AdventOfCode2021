open Core

type mode =
  | Gamma
  | Epsilon

let make_masks data =
  let head = List.hd_exn data in
  let length = String.length head in
  let sequence = List.range 0 length in
  List.map sequence ~f:(fun x -> Int.pow 2 x)


let data_to_numbers data =
  List.map data ~f:(fun row -> int_of_string ("0b" ^ row))


let apply_mask numbers mask mode =
  let masked = List.map numbers ~f:(fun num -> Int.bit_and num mask) in
  let total = List.length (List.filter masked ~f:(fun x -> x > 0)) in
  let max = (List.length numbers) in
  let half = max / 2 in
  match mode with
  | Gamma -> if total > half then mask else 0
  | Epsilon -> if total <= half then mask else 0
    

let apply_masks numbers masks mode =
  let applied = List.map masks ~f:(fun mask -> apply_mask numbers mask mode) in
  List.reduce_exn applied ~f:(+)

let challenge_1 data =
  let as_numbers = data_to_numbers data in
  let masks = make_masks data in
  let gamma = apply_masks as_numbers masks Gamma in
  let epsilon = apply_masks as_numbers masks Epsilon in
  gamma * epsilon


let split_into_boxes numbers mask =
  List.fold numbers ~f:(fun (left, right) number ->
      if (Int.bit_and number mask) <> 0 then (number :: left), right
      else left, number :: right
  ) ~init:([], [])


type system =
  | O2
  | Scrubber

let rec find_value numbers masks system =
  match List.length numbers with
  | 1 -> List.hd_exn numbers
  | _ -> (
      let mask, rest = match masks with
      | [] -> failwith "Ran out of masks"
      | mask :: rest -> mask, rest in
      let yes, no = split_into_boxes numbers mask in
      match system, (List.length yes), (List.length no) with
      | O2, yes_len, no_len when yes_len >= no_len -> find_value yes rest system
      | O2, yes_len, no_len when yes_len < no_len -> find_value no rest system
      | Scrubber, yes_len, no_len when yes_len >= no_len -> find_value no rest system
      | Scrubber, yes_len, no_len when yes_len < no_len -> find_value yes rest system
      | _ -> failwith "ARGH"
  )


let challenge_2 data = 
  let as_numbers = data_to_numbers data in
  let masks = List.rev(make_masks data) in
  let o2 = find_value as_numbers masks O2 in
  let scrubber = find_value as_numbers masks Scrubber in
  o2 * scrubber

let () =
  let data = In_channel.read_lines "day3.txt" in
  let sample = [
    "00100";
    "11110";
    "10110";
    "10111";
    "10101";
    "01111";
    "00111";
    "11100";
    "10000";
    "11001";
    "00010";
    "01010"
  ] in
  let test_result = challenge_1 sample in
  assert (test_result = 198);
  printf "Challenge 1: %d\n" (challenge_1 data);
  let test_result_2 = challenge_2 sample in
  assert (test_result_2 = 230);
  printf "Challenge 2: %d\n" (challenge_2 data)
