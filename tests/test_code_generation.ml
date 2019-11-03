open Eva_lib
open Instructions
open Instructions_processing

let%test _ =
  let instr = ADD_R_R (`Reg 0, `Reg 1) in
  to_bin instr = 0x00001000

let%test _ =
  let instr = ADD_R_C (`Reg 0, `Cst 1) in
  to_bin instr = 0x10000001

let%test _ =
  let instr = ADDC_R_R (`Reg 0, `Reg 1) in
  to_bin instr = 0x04001000

let%test _ =
  let instr = ADDC_R_C (`Reg 0, `Cst 1) in
  to_bin instr = 0x14000001