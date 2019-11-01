open Eva_parsing

let parse_string s =
  let lexbuf = Lexing.from_string s in
  Parser.instruction lexbuf


let%test _ =
  let instr = "ADD R0, R1\n" in
  parse_string instr = ADD_R_R (`Reg 0, `Reg 1)


let%test _ =
  let instr = "ADD R0, #1\n" in
  parse_string instr = ADD_R_C (`Reg 0, `Cst 1)


let%test _ =
  let instr = "ADDC R0, R1\n" in
  parse_string instr = ADDC_R_R (`Reg 0, `Reg 1)


let%test _ =
  let instr = "ADDC R0, #1\n" in
  parse_string instr = ADDC_R_C (`Reg 0, `Cst 1)


let%test _ =
  let instr = "SUB R0, R1\n" in
  parse_string instr = SUB_R_R (`Reg 0, `Reg 1)


let%test _ =
  let instr = "SUB R0, #1\n" in
  parse_string instr = SUB_R_C (`Reg 0, `Cst 1)


let%test _ =
  let instr = "SUBC R0, R1\n" in
  parse_string instr = SUBC_R_R (`Reg 0, `Reg 1)


let%test _ =
  let instr = "SUBC R0, #1\n" in
  parse_string instr = SUBC_R_C (`Reg 0, `Cst 1)


let%test _ =
  let instr = "MOV R0, R1\n" in
  parse_string instr = MOV_R_R (`Reg 0, `Reg 1)


let%test _ =
  let instr = "MOV R0, #1\n" in
  parse_string instr = MOV_R_C (`Reg 0, `Cst 1)


let%test _ =
  let instr = "MOV R0, tag\n" in
  parse_string instr = MOV_R_L (`Reg 0, `Label "tag")


let%test _ =
  let instr = "LDR R0, [R1]\n" in
  parse_string instr = LDR_R_AR (`Reg 0, `Reg 1)


let%test _ =
  let instr = "LDR R0, tag\n" in
  parse_string instr = LDR_R_AL (`Reg 0, `Label "tag")


let%test _ =
  let instr = "LDR R0, #1\n" in
  parse_string instr = LDR_R_AC (`Reg 0, `Adr 1)


let%test _ =
  let instr = "STR R0, [R1]\n" in
  parse_string instr = STR_R_AR (`Reg 0, `Reg 1)


let%test _ =
  let instr = "STR R0, tag\n" in
  parse_string instr = STR_R_AL (`Reg 0, `Label "tag")


let%test _ =
  let instr = "STR R0, #1\n" in
  parse_string instr = STR_R_AC (`Reg 0, `Adr 1)


let%test _ =
  let instr = "LDR R0, [R1]\n" in
  parse_string instr = LDR_R_AR (`Reg 0, `Reg 1)


let%test _ =
  let instr = "PUSH R0\n" in
  parse_string instr = PUSH_R (`Reg 0)


let%test _ =
  let instr = "POP R0\n" in
  parse_string instr = POP_R (`Reg 0)


let%test _ =
  let instr = "LDR R0, [R1]\n" in
  parse_string instr = LDR_R_AR (`Reg 0, `Reg 1)


let%test _ =
  let instr = "LDR R0, tag\n" in
  parse_string instr = LDR_R_AL (`Reg 0, `Label "tag")


let%test _ =
  let instr = "CMP R0, R1\n" in
  parse_string instr = CMP_R_R (`Reg 0, `Reg 1)


let%test _ =
  let instr = "BEQ R0\n" in
  parse_string instr = BEQ_R (`Reg 0)


let%test _ =
  let instr = "BNEQ R0\n" in
  parse_string instr = BNEQ_R (`Reg 0)


let%test _ =
  let instr = "BLT R0\n" in
  parse_string instr = BLT_R (`Reg 0)


let%test _ =
  let instr = "BLE R0\n" in
  parse_string instr = BLE_R (`Reg 0)



