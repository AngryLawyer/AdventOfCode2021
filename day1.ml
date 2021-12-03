open Core

type change =
  | Increase
  | Decrease
  | NA

let number_of_increases data_as_numbers =
  let yesterdays = None :: data_as_numbers in
  let todays = List.append data_as_numbers [None] in
  let zipped = List.zip_exn todays yesterdays in
  let result = List.map zipped ~f:(fun (today, yesterday) ->
    match today, yesterday with
    | Some today, Some yesterday when today < yesterday -> Decrease
    | Some today, Some yesterday when today > yesterday -> Increase
    | _ -> NA
  ) in
  List.fold (List.map result ~f:(function
      | Increase -> 1
      | _ -> 0
  )) ~f:(+) ~init:0


let challenge_1 data =
  let data_as_numbers = List.map ~f:(fun d -> Some (Int.of_string d)) data in
  number_of_increases data_as_numbers

let create_windows data_as_numbers =
  let todays = List.append data_as_numbers [None; None] in
  let yesterdays = None :: (List.append data_as_numbers [None]) in
  let days_before = None :: None :: data_as_numbers in
  let zipped = List.zip_exn (List.zip_exn todays yesterdays) days_before in
  List.map zipped ~f:(fun ((today, yesterday), day_before) ->
    match today, yesterday, day_before with
      | Some a, Some b, Some c -> Some (a + b + c)
      | _ -> None
  )

let challenge_2 data = 
  let data_as_numbers = List.map ~f:(fun d -> Some (Int.of_string d)) data in
  let windows = create_windows data_as_numbers in
  number_of_increases windows

let () =
  let data = In_channel.read_lines "day1.txt" in
  let sample = [
    "199";
    "200";
    "208";
    "210";
    "200";
    "207";
    "240";
    "269";
    "260";
    "263"
  ] in
  let test_result = challenge_1 sample in
  assert (test_result = 7);
  printf "Challenge 1: %d\n" (challenge_1 data);
  let test_result_2 = challenge_2 sample in
  assert (test_result_2 = 5);
  printf "Challenge 2: %d\n" (challenge_2 data)
