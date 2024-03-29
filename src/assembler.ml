(**
  Main module of the assembler
*)

open Instructions
open Instructions_processing
open Sys
open Filename


(**
  [write_to_file output_channel opcode] writes [opcode] in the [output_channel].
*)
let write_to_file oc i =
  output_char oc (char_of_int ((i lsr 24) land 0xFF));
  output_char oc (char_of_int ((i lsr 16) land 0xFF));
  output_char oc (char_of_int ((i lsr 8)  land 0xFF));
  output_char oc (char_of_int (i land 0xFF))


(**
  [get_adresses instr_list] builds an assocation table (label -> adresse)
*)
let get_adresses instr_list =
  let rec step l curr_adr adr_list =
    match l with
    | [] -> adr_list
    | LABEL (`Label s) :: tail -> step tail curr_adr ((s, curr_adr)::adr_list)
    | _ :: tail -> step tail (curr_adr+1) adr_list
  in
  step instr_list 0 []


(**
  [set_adresses instr_list] replaces labels in a sequence of instructions by 
  their exact adresses.
*)
let set_adresses instr_list =
  let adr_list = get_adresses instr_list in
  let replace instr =
    match instr with
    | LDR_R_AL  (`Reg r1, `Label s) -> LDR_R_AC (`Reg r1, `Adr (List.assoc s adr_list))
    | STR_R_AL  (`Reg r1, `Label s) -> STR_R_AC (`Reg r1, `Adr (List.assoc s adr_list))
    | MOV_R_L   (`Reg r1, `Label s) -> MOV_R_C  (`Reg r1, `Cst (List.assoc s adr_list))
    | _ -> instr
  in
  List.map replace instr_list
  |> List.filter (function LABEL _ -> false | _ -> true)


(**
  [do_assemble in_chan out_chan] reads assembly code from [in_channel] assemble 
  it and outputs the binary in [out_chan].
*)
let do_assemble in_chan out_chan =
    let instructions = ref [] in
    let lexbuf = Lexing.from_channel in_chan in
    while true do
      try
        instructions := !instructions @ [Parser.instruction lexbuf]
      with
      | Parser.Eof ->
        let to_output = set_adresses !instructions in
        let process i = to_bin i |> write_to_file out_chan in
        List.iter process to_output;
        exit 0
      | Parser.Error e -> failwith e
    done


exception IO_error of string

(**
  Get an automatic name for outputs files.
*)
let get_output_name f =
  if check_suffix f "evasm" then (
    concat (getcwd ())  ((chop_suffix (basename f) "evasm") ^ "evo")
  ) else (
    raise (IO_error "Input files should have extension .evasm")
  )


(**
  Check wether an output name is valid or not. Raise an {!IO_error} if the output name is'nt valid.
*)
let check_output_name f =
  if check_suffix f "evo" then f
  else raise (IO_error "Output files should have extension .evo")


(**
  Run the full assembler.
  @param in_f The input file
  @param out_f The output file (option)
*)
let run in_f out_f =
  match in_f, out_f with
  | f, None ->
    do_assemble (open_in f) (open_out (get_output_name f))
  | f1, Some(f2) ->
    do_assemble (open_in f1) (open_out (check_output_name f2))