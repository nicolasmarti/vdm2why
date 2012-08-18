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
    | TyOption ty -> Box [Verbatim "["; vdmtype2token ty; Verbatim "]"]		
    | TyUnion tys -> Box (intercalates [Space 1; Verbatim "|"; Space 1] (List.map vdmtype2token tys))
    | TyFct (tys, b, ty) -> 
      Box ( [Verbatim "("] @
	      (intercalates [Space 1; Verbatim ","; Space 1] 
		 (List.map vdmtype2token tys)) @
	      [Verbatim ")"; Space 1; Verbatim (if b then "+>" else "->"); Space 1; vdmtype2token ty])

let rec vdmterm2token (te: vdmterm) (p: place) : token =
  match te.ast with
    | TeBool b -> Verbatim (if b then "true" else "false")
    | TeBottom -> Verbatim "_|_"
    | TeNat1 i -> Verbatim (string_of_int i)
    | TeNat i -> Verbatim (string_of_int i)
    | TeInt i -> Verbatim (string_of_int i)
    | TeReal r -> Verbatim (string_of_float r)
    | Abs te -> 
      (match p with
	| InArg -> withParen
	| _ -> fun x -> x
      )
	(Box [Verbatim "abs"; Space 1; vdmterm2token te InArg])
    | Floor te -> 
      (match p with
	| InArg -> withParen
	| _ -> fun x -> x
      )
	(Box [Verbatim "abs"; Space 1; vdmterm2token te InArg])
    | Minus te -> 
      (match p with
	| InArg -> withParen
	| _ -> fun x -> x
      )
	(Box [Verbatim "abs"; Space 1; vdmterm2token te InArg])
    | TeChar c -> verbatims ["'"; String.make 1 c; "'"]
    | TeApp (f, args) -> 
      Box ([verbatims [f; "("]] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun te -> vdmterm2token te InArg) args
	       )
	      ) @
	      [verbatims [")"]]
      )
    | TeName n -> Verbatim n
    | TeToken te -> Box [Verbatim "mk_token("; vdmterm2token te Alone; Verbatim ")"]
    | TeFieldAccess (te, n) -> Box [vdmterm2token te InArg; verbatims ["."; n]]
	   


let rec vdmtypedecl2token (decl: vdmtypedecl) : token =
  match decl with
    | TypeDecl (n, ty, _, None) -> Box [Verbatim n; Space 1; Verbatim "="; Space 1; vdmtype2token ty]
    | TypeDecl (n, ty, pos, Some (p, def, _)) -> 
      Box [Verbatim n; Space 1; Verbatim "="; Space 1; vdmtype2token ty; Newline;
	   verbatims ["inv_"; n]; Space 1; Verbatim ":"; Space 1; vdmtype2token (TyFct ([TyName (n, pos)], true, TyBool)); Space 1; Verbatim "=="; Space 1; vdmterm2token def Alone
	  ]

