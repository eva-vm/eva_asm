(**
 * Module for Instructions types
 *)

type oper = [
  | `Reg    of int
  | `Cst    of int
  | `Label  of string
  | `Adr    of int
]
(** Type for operands. This type is just a guideline *)

type t =
  | LABEL     of [ `Label of string ]
  
  (* ----------------------------------------- *)
  (* -         Arithmetic operations          -*)
  (* ----------------------------------------- *)
  
  | ADD_R_R   of [ `Reg of int ] * [ `Reg of int ]
  | ADD_R_C   of [ `Reg of int ] * [ `Cst of int ]
  | ADDC_R_R  of [ `Reg of int ] * [ `Reg of int ]
  | ADDC_R_C  of [ `Reg of int ] * [ `Cst of int ]
  
  | SUB_R_R   of [ `Reg of int ] * [ `Reg of int ]
  | SUB_R_C   of [ `Reg of int ] * [ `Cst of int ]
  | SUBC_R_R  of [ `Reg of int ] * [ `Reg of int ]
  | SUBC_R_C  of [ `Reg of int ] * [ `Cst of int ]
  
  (* ----------------------------------------- *)
  (* -         Register Manipulation          -*)
  (* ----------------------------------------- *)

  | MOV_R_R   of [ `Reg of int ] * [ `Reg of int ]
  | MOV_R_C   of [ `Reg of int ] * [ `Cst of int ]
  | MOV_R_L   of [ `Reg of int ] * [ `Label of string ]

  (* ----------------------------------------- *)
  (* -            Load and Store              -*)
  (* ----------------------------------------- *)

  | LDR_R_AR  of [ `Reg of int ] * [ `Reg of int ]
  | LDR_R_AC  of [ `Reg of int ] * [ `Adr of int ]
  | LDR_R_AL  of [ `Reg of int ] * [ `Label of string ]
  | STR_R_AR  of [ `Reg of int ] * [ `Reg of int ]
  | STR_R_AC  of [ `Reg of int ] * [ `Adr of int ]
  | STR_R_AL  of [ `Reg of int ] * [ `Label of string ]
  
  (* ----------------------------------------- *)
  (* -            Conditionnals               -*)
  (* ----------------------------------------- *)
  
  | CMP_R_R   of [ `Reg of int ] * [ `Reg of int ]
  | CMP_R_C   of [ `Reg of int ] * [ `Cst of int ]
  | BEQ_R     of [ `Reg of int ]
  | BNEQ_R    of [ `Reg of int ]
  | BLT_R     of [ `Reg of int ]
  | BLE_R     of [ `Reg of int ]

  (* ----------------------------------------- *)
  (* -              PUSH - POP                -*)
  (* ----------------------------------------- *)
  
  | PUSH_R    of [ `Reg of int ]
  | POP_R     of [ `Reg of int ]
(** Type for instructions *)


let pprint instr =
  match instr with
  | ADD_R_R   _ -> print_endline "ADD"
  | ADD_R_C   _ -> print_endline "ADD"
  | ADDC_R_R  _ -> print_endline "ADDC"
  | ADDC_R_C  _ -> print_endline "ADDC"
  | SUB_R_R   _ -> print_endline "SUB"
  | SUB_R_C   _ -> print_endline "SUB"
  | SUBC_R_R  _ -> print_endline "SUBC"
  | SUBC_R_C  _ -> print_endline "SUBC"
  | MOV_R_R   _ -> print_endline "MOV"
  | MOV_R_C   _ -> print_endline "MOV"
  | MOV_R_L   _ -> print_endline "MOV"
  | LDR_R_AR  _ -> print_endline "LDR"
  | LDR_R_AL  _ -> print_endline "LDR"
  | LDR_R_AC  _ -> print_endline "LDR"
  | STR_R_AR  _ -> print_endline "STR"
  | STR_R_AL  _ -> print_endline "STR"
  | STR_R_AC  _ -> print_endline "STR"
  | CMP_R_C   _ -> print_endline "CMP"
  | CMP_R_R   _ -> print_endline "CMP"
  | BEQ_R     _ -> print_endline "BEQ"
  | BNEQ_R    _ -> print_endline "BNEQ"
  | BLT_R     _ -> print_endline "BLT"
  | BLE_R     _ -> print_endline "BLE"
  | PUSH_R    _ -> print_endline "PUSH"
  | POP_R     _ -> print_endline "POP"
  | LABEL     _ -> print_endline "LABEL"