open Libparser
open Libpprinter

open Parser
open Printer

open Printf

open Calculus_kernel

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
    let decls = (many parse_module_decl) pb in
    let m = List.fold_right (fun (hd1, hd2, hd3, hd4, hd5) (tys, tes, vals, states, ops) ->
      (hd1 @ tys), (hd2 @ tes), (hd3 @ vals), (hd4 @ states), (hd5 @ ops)     
    ) decls ([], [], [], [], []) in
    let _ = whitespaces pb in
    let _ = eos pb in
    let token = vdmmoduledecl2token m in
    let box = token2box token 200 2 in
    printf "%s\n" (box2string box)
  with
    | NoMatch -> 
      printf "parsing error:\n%s\n" (markerror pb); flush Pervasives.stdout;
      raise Pervasives.Exit
;;


main ()

