
{
  exception Eof
  exception Error of string
  open Instructions
  open Conversions
  open Printf
  
  let incr_linenum lexbuf = 
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum  = pos.pos_lnum + 1;
      pos_bol   = pos.pos_cnum;
    }

  let print_position lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    sprintf "%s:%d:%d" pos.pos_fname
      pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  
  (* let reg_of_string s =
    String.sub s 1 ((String.length s)-1) |> int_of_string 
    |> fun x ->
      if x > 0 && x < 16 then `Reg x
      else raise (Error (sprintf "Invalid register R%d" x))
  
  let cst_of_string s =
    String.sub s 1 ((String.length s)-1) |> int_of_string 
    |> fun x ->
      if x > 0 && float_of_int x < 2. ** 16. then `Cst x 
      else raise (Error (sprintf "Invalid const #%d" x))

  let adr_of_string s =
    String.sub s 1 ((String.length s)-1) |> int_of_string
    |> fun x ->
      if x > 0 && float_of_int x < 2. ** 16. then `Adr x
      else raise (Error (sprintf "Invalid adr #%d" x)) *)

}


let reg   = ('R' | 'r') ['0'-'9']+
let blank = (('\t')+ | (' ')+)
let cst   = '#' ['0'-'9']+
let label = ['a'-'z' 'A'-'Z' '_']+ ['0'-'9']* ['a'-'z' 'A'-'Z' '_']*


rule instruction = parse
  | '\n'
    { incr_linenum lexbuf; instruction lexbuf }

  | blank
    { instruction lexbuf }

  | ';' [^ '\n']+ '\n'
    { incr_linenum lexbuf; instruction lexbuf }

  | (label as l) blank* ':'
    { LABEL (`Label l) }
  
  | "ADD" blank (reg as r1) ',' blank (reg as r2)
    { 
      ADD_R_R (reg_of_string r1, reg_of_string r2)
    }
  

  | "ADD" blank (reg as r1) ',' blank (cst as c)
    {
      ADD_R_C (reg_of_string r1, cst_of_string c)
    }


  | "ADDC" blank (reg as r1) ',' blank (reg as r2)
    {
      ADDC_R_R (reg_of_string r1, reg_of_string r2)
    }


  | "ADDC" blank (reg as r1) ',' blank (cst as c)
    { 
      ADDC_R_C (reg_of_string r1, cst_of_string c)
    }
  
  | "SUB" blank (reg as r1) ',' blank (reg as r2)
    { 
      SUB_R_R (reg_of_string r1, reg_of_string r2)
    }
  

  | "SUB" blank (reg as r1) ',' blank (cst as c)
    {
      SUB_R_C (reg_of_string r1, cst_of_string c)
    }


  | "SUBC" blank (reg as r1) ',' blank (reg as r2)
    {
      SUBC_R_R (reg_of_string r1, reg_of_string r2)
    }


  | "SUBC" blank (reg as r1) ',' blank (cst as c)
    { 
      SUBC_R_C (reg_of_string r1, cst_of_string c)
    }


  | "MOV" blank (reg as r1) ',' blank (reg as r2)
    { 
      MOV_R_R (reg_of_string r1, reg_of_string r2)
    }
  

  | "MOV" blank (reg as r1) ',' blank (cst as c)
    { 
      MOV_R_C (reg_of_string r1, cst_of_string c)
    }

  | "MOV" blank (reg as r1) ',' blank (label as l)
    { 
      MOV_R_L (reg_of_string r1, `Label l)
    }
    

  | "LDR" blank (reg as r1) ',' blank '[' (reg as r2) ']'
    { 
      LDR_R_AR (reg_of_string r1, reg_of_string r2)
    }

  
  | "LDR" blank (reg as r1) ',' blank (label as l)
    { 
      LDR_R_AL (reg_of_string r1, `Label l)
    }

  
  | "LDR" blank (reg as r1) ',' blank (cst as c)
    { 
      LDR_R_AC (reg_of_string r1, adr_of_string c)
    }

  
  | "STR" blank (reg as r1) ',' blank '[' (reg as r2) ']'
    {
      STR_R_AR (reg_of_string r1, reg_of_string r2)
    }

  
  | "STR" blank (reg as r1) ',' blank (label as l)
    { 
      STR_R_AL (reg_of_string r1, `Label l)
    }

  
  | "STR" blank (reg as r1) ',' blank (cst as c)
    { 
      STR_R_AC (reg_of_string r1, adr_of_string c)
    }

  
  | "PUSH" blank (reg as r1)
    {
      PUSH_R (reg_of_string r1)
    }
  
  
  | "POP"  blank (reg as r1)
    {
      POP_R (reg_of_string r1)
    }


  | "CMP"  blank (reg as r1) ',' blank (reg as r2)
    {
      CMP_R_R (reg_of_string r1, reg_of_string r2)
    }


  | "CMP"  blank (reg as r1) ',' blank (cst as c)
    {
      CMP_R_C (reg_of_string r1, cst_of_string c)
    }

  | "BEQ"  blank (reg as r1)
    {
      BEQ_R (reg_of_string r1)
    }

  | "BNEQ"  blank (reg as r1)
    {
      BNEQ_R (reg_of_string r1)
    }

  | "BLT"  blank (reg as r1)
    {
      BLT_R (reg_of_string r1)
    }

  | "BLE"  blank (reg as r1)
    {
      BLE_R (reg_of_string r1)
    }
  
  | "IN"  blank (reg as r1)
    {
      IN_R (reg_of_string r1)
    }
  
  | "OUT"  blank (reg as r1)
    {
      OUT_R (reg_of_string r1)
    }

  | eof
    { raise Eof }
  | _ as t
    { raise ( Error ("Invalid token " ^ (String.make 1 t) ^ " " ^ (print_position lexbuf)) ) }