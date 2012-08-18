open Parser
open Libparser

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
    let _ = (many parse_type_decl) pb in
    let _ = whitespaces pb in
    let _ = eos pb in
    ()
  with
    | NoMatch -> 
      printf "parsing error: %s\n" (errors2string pb); flush Pervasives.stdout;
      raise Pervasives.Exit
;;


main ()

