(**
  This module contains utils for safe integer conversions.
*)

open Printf

exception Conversion_Error of string
(**
  Conversion Error exception. Raised by 
   {! cst_of_string}, {! adr_of_string} and {! reg_of_string}
*)


(**
  [valid_cst i] ensures that integer [i] belongs to the range of Eva consants.
  Return [`Cst i] if the test succeed, else raise {! Conversion_Error}.
*)
let valid_cst i =
  if i >= 0 && float_of_int i < 2. ** 16. then `Cst i
    else raise (Conversion_Error (sprintf "Invalid const #%d" i))


(**
  [valid_adr i] ensures that integer [i] belongs to the range of Eva adresses.
  Return [`Adr i] if the test succeed, else raise {! Conversion_Error}.
*)
let valid_adr i =
  if i >= 0 && float_of_int i < 2. ** 16. then `Adr i
  else raise (Conversion_Error (sprintf "Invalid const adr #%d" i))


(**
  [valid_reg r] ensures that integer [r] belongs to the range of Eva registers.
  Return [`Reg r] if the test succeed, else raise {! Conversion_Error}.
*)
let valid_reg r =
  if r >= 0 && r < 16 then `Reg r
  else raise (Conversion_Error (sprintf "Invalid register r%d" r))


let cst_of_string s =
  try
    String.sub s 1 ((String.length s)-1) 
    |> int_of_string |> valid_cst
  with
    | Failure(_) ->
      Printf.fprintf stderr "Unable to convert const %s, this value outpasses memory limitations" s;
      exit 1
    | Conversion_Error (e) ->
      Printf.fprintf stderr "%s" e;
      exit 1


let reg_of_string s =
  try
    String.sub s 1 ((String.length s)-1)
    |> int_of_string |> valid_reg
  with
  | Failure(_) -> 
    Printf.fprintf stderr "Unable to convert reg %s, this value outpasses memory limitations" s;
    exit 1
  | Conversion_Error (e) ->
    Printf.fprintf stderr "%s" e;
    exit 1


let adr_of_string s =
  try
    String.sub s 1 ((String.length s)-1)
    |> int_of_string |> valid_adr
  with
  | Failure(_) ->
    Printf.fprintf stderr "Unable to convert adr %s, this value outpasses memory limitations" s;
    exit 1
  | Conversion_Error (e) ->
    Printf.fprintf stderr "%s" e;
    exit 1