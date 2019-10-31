open Eva_parsing
open Instructions

let write_to_file oc i =
  output_char oc (char_of_int ((i lsr 24) land 0xFF));
  output_char oc (char_of_int ((i lsr 16) land 0xFF));
  output_char oc (char_of_int ((i lsr 8)  land 0xFF));
  output_char oc (char_of_int (i land 0xFF))


let get_adresses instr_list =
  let rec step l i adr_list =
    match l with
    | [] -> adr_list
    | LABEL (`Label s) :: tail -> step tail i ((s, i)::adr_list)
    | _ :: tail -> step tail (i+1) adr_list
  in
  step instr_list 0 []


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


let _ =
  let l = ref [] in
  let oc = open_out "test.evo" in
  (* let ic = open_in "test.evasm" in *)
  let lexbuf = Lexing.from_channel stdin in

  while true do
    try
      l := !l @ [ Parser.instruction  lexbuf ]
    with
    | Parser.Eof ->
      let open Instructions_processing in
      let instr = set_adresses !l in
      let process i = to_bin i |> write_to_file oc in
      List.iter process instr;
      List.iter pprint !l;
      exit 0
    | Parser.Error e -> failwith e
  done