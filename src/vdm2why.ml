open Libparser
open Libpprinter

open Parser
open Printer

open Printf

let main () = 
  let stream = 
    if Array.length Sys.argv < 2 then
      line_stream_of_channel stdin
    else 
      let chan = open_in Sys.argv.(1) in
      line_stream_of_channel chan
  in
  let pb = build_parserbuffer stream in
  try 
    let decls = (many parse_type_decl) pb in
    let _ = whitespaces pb in
    let _ = eos pb in
    let _ = List.map (fun decl ->
      let token = vdmtypedecl2token decl in
      let box = token2box token 200 2 in
      printf "%s\n" (box2string box)
    ) decls in
    ()
  with
    | NoMatch -> 
      printf "parsing error: %s\n" (errors2string pb); flush Pervasives.stdout;
      raise Pervasives.Exit
;;


main ()

