open Core

let list_to_string l =
  String.concat (List.map l ~f:(fun x -> sprintf "%d" x)) ~sep:","

let buckets_to_string l =
  let idxes = List.range 0 9 in
  let zipped = List.zip_exn idxes l in
  String.concat (List.map zipped ~f:(fun (idx, count) -> sprintf "([%d]: %d)" idx count)) ~sep:","

let total l =
  List.reduce_exn l ~f:(+)

let generate count =
  List.map (List.range 0 count) ~f:(fun _ -> 8)

let cycle buckets =
  match buckets with
  | dropped :: rest -> 
    assert (List.length rest = 8);
    let front_6, rear_3 = List.split_n rest 6 in
    assert ((List.length front_6) = 6);
    (
      match rear_3 with
      | sixes :: sevens :: [] ->
        front_6 @ ((sixes + dropped) :: sevens :: dropped :: [])
      | _ -> failwith "Bucket logic wrong"
    )
  | _ -> failwith "Bad buckets"

let rec cycle_times items count =
  match count with
  | 0 -> 
    items
  | x -> cycle_times (cycle items) (x - 1)


let bucket line =
  List.map (List.range 0 9) ~f:(fun idx ->
    List.length (List.filter line ~f:(fun item -> item = idx))
  )


let parse lines =
  let line = List.hd_exn lines in
  let parsed = List.map (String.split line ~on:',') ~f:Int.of_string in
  bucket parsed


let challenge_1 lines =
  let bucketed = parse lines in
  total (cycle_times bucketed 80)

let challenge_2 lines =
  let bucketed = parse lines in
  total (cycle_times bucketed 256)

let () =
  let data = In_channel.read_lines "day6.txt" in
  let sample = [
    "3,4,3,1,2"
  ] in
  let test_result = challenge_1 sample in
  assert (test_result = 5934);
  printf "Challenge 1: %d\n" (challenge_1 data);
  let test_result = challenge_2 sample in
  assert (test_result = 26984457539);
  printf "Challenge 2: %d\n" (challenge_2 data)

