let year = 2023
let day = 1
open Core

let char_pair_to_int char_pair = 
  match char_pair with
  | (a, b) -> Int.of_string( String.of_char a ^ String.of_char b )

(* takes input multiline string, and returns a list of (list of characters)  *)
let input_to_linewise_characters input = input
  |> Stdlib.String.split_on_char '\n'
  |> List.map ~f:String.to_list

let get_linewise_calibration_value = (fun parent_list -> 
  let without_alphabet_chars = List.map ~f:(List.filter ~f:(Char.is_digit)) parent_list in

  let calibration_value_per_line = List.map ~f:(fun child_list -> (
    match List.length child_list with
  | 1 -> (List.hd_exn child_list, List.hd_exn child_list) |> char_pair_to_int
  | _ -> (List.hd_exn child_list, List.last_exn child_list) |> char_pair_to_int
  )) without_alphabet_chars in calibration_value_per_line
)

module Part_1 = struct
  let run (input : string) : (string, string) result =
  input
    |> input_to_linewise_characters
    |> get_linewise_calibration_value
    (* addition of all items in the list *)
    |> List.fold ~init:0 ~f:(fun acc curr_val -> acc +  curr_val)
    |> Stdlib.string_of_int
    |> Stdlib.Result.ok 

end

module Part_2 = struct
  let run (input : string) : (string, string) result = Ok input
end
