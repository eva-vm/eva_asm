open Cmdliner
open Assembler

let input_file =
  let doc = "The input assembly file" in
  let docv = "file.evasm" in
  Arg.(required & pos 0 (some file) None & info [] ~doc ~docv)


let output_file =
  let doc = "The output file" in
  let docv = "file.evo" in
  Arg.(value & opt (some string) None & info ["o"; "output"] ~doc ~docv)


let cmd =
  let doc = "Assembler for Eva assembly language" in
  let man = [
    `S Manpage.s_authors;
    `P "Arthur Correnson <arthur.correnson@univ-tlse3.fr>";
    `S Manpage.s_bugs;
    `P "If you find a bug, please send an email to <arthur.correnson@univ-tlse3.fr>";
  ] in
  Term.(const run $ input_file $ output_file),
  Term.info "evasm" ~version:"v0.1" ~exits:Term.default_exits ~doc ~man

let () = Term.(exit @@ eval cmd)
