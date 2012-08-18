open Def
open Libpprinter
open Libparser
open Extlist

let rec withParen (t: token) : token =
  Box [Verbatim "("; t; Verbatim ")"]

let rec withBracket (t: token) : token =
  Box [Verbatim "{"; t; Verbatim "}"]

(* a data structure to mark the place where the term/pattern is *)
type place = InNotation of associativity * priority * int (* in the sndth place of the application to the notation with assoc and priority *)
	     | InApp (* in the head of application *)
	     | InArg  (* as an argument (Explicit) *)
	     | Alone (* standalone *)

let verbatims (l: name list) = Verbatim (String.concat "" l)

let rec vdmtype2token (ty: vdmtype) : token =
  match ty with
    | TyBool -> Verbatim "bool"
    | TyNat1 -> Verbatim "nat1"
    | TyNat -> Verbatim "nat"
    | TyInt -> Verbatim "int"
    | TyChar -> Verbatim "char"
    | TyQuote s -> verbatims ["<"; s; ">"]
    | TyVar i -> verbatims ["?"; string_of_int i]
    | TyName (n, _) -> Verbatim n
    | TySet ty -> Box [Verbatim "set of"; Space 1; vdmtype2token ty]
    | TySeq ty -> Box [Verbatim "seq"; Space 1; vdmtype2token ty]
    | TySeq1 ty -> Box [Verbatim "seq1"; Space 1; vdmtype2token ty]
    | TyMap (ty1, ty2) -> Box [Verbatim "map"; Space 1; vdmtype2token ty1; Space 1; Verbatim "to"; Space 1; vdmtype2token ty2]
    | TyInjMap (ty1, ty2) -> Box [Verbatim "inmap"; Space 1; vdmtype2token ty1; Space 1; Verbatim "to"; Space 1; vdmtype2token ty2]
    | TyProd tys -> Box (intercalates [Space 1; Verbatim "*"; Space 1] (List.map (fun ty -> vdmtype2token ty) tys))
    | TyComp (n, fields) -> Box ([Verbatim "n"; Space 1; Verbatim "::"; Space 1] @
				 [Box (intercalate Newline (List.map ( fun field ->
				   Box (
				     (match fst field with
				       | None -> []
				       | Some (n, true) -> [Verbatim n; Space 1; Verbatim ":"; Space 1]
				       | Some (n, false) -> [Verbatim n; Space 1; Verbatim ":-"; Space 1]
				     ) @
				       [vdmtype2token (snd field); Newline]
				   )
				 ) fields))])
				

let rec vdmtypedecl2token (decl: vdmtypedecl) : token =
  match decl with
    | TypeDecl (n, ty, _) -> Box [Verbatim n; Space 1; Verbatim "="; Space 1; vdmtype2token ty]


