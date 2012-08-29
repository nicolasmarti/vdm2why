open Def
open Libpprinter
open Libparser
open Extlist
open Parser

let rec withParen (t: token) : token =
  Box [Verbatim "("; t; Verbatim ")"]

let rec withBracket (t: token) : token =
  Box [Verbatim "{"; t; Verbatim "}"]

(* a data structure to mark the place where the term/pattern is *)
type place = InNotationPrefix of priority * int (* in the sndth place of the application to the notation with assoc and priority *)
	     | InNotationPostfix of priority * int (* in the sndth place of the application to the notation with assoc and priority *)
	     | InNotationInfix of associativity * priority * int (* in the sndth place of the application to the notation with assoc and priority *)
	     | InArg  (* as an argument (Explicit) *)
	     | Alone (* standalone *)

let verbatims (l: name list) = Verbatim (String.concat "" l)



let rec vdmtype2token (ty: vdmtype) : token =
  match ty with
    | TyBool -> Verbatim "bool"
    | TyNat1 -> Verbatim "nat1"
    | TyNat -> Verbatim "nat"
    | TyInt -> Verbatim "int"
    | TyReal -> Verbatim "real"
    | TyChar -> Verbatim "char"
    | TyToken -> Verbatim "token"
    | TyQuote s -> verbatims ["<"; s; ">"]
    | TyVar i -> verbatims ["?"; string_of_int i]
    | TyName (n, _) -> Verbatim n
    | TySet ty -> Box [Verbatim "set of"; Space 1; vdmtype2token ty]
    | TySeq ty -> Box [Verbatim "seq of"; Space 1; vdmtype2token ty]
    | TySeq1 ty -> Box [Verbatim "seq1 of"; Space 1; vdmtype2token ty]
    | TyMap (ty1, ty2) -> Box [Verbatim "map"; Space 1; vdmtype2token ty1; Space 1; Verbatim "to"; Space 1; vdmtype2token ty2]
    | TyInjMap (ty1, ty2) -> Box [Verbatim "inmap"; Space 1; vdmtype2token ty1; Space 1; Verbatim "to"; Space 1; vdmtype2token ty2]
    | TyProd tys -> Box (intercalates [Space 1; Verbatim "*"; Space 1] (List.map (fun ty -> vdmtype2token ty) tys))
    | TyComp (n, fields) -> IBox ([Verbatim n; Space 1; Verbatim "::"; Space 1] @
				 [IBox (intercalate Newline (List.map ( fun field ->
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
    | TyFct (ty1, b, ty2) -> 
      Box [vdmtype2token ty1; Space 1; Verbatim (if b then "+>" else "->"); Space 1; vdmtype2token ty2]

let vdmsymb_infix_assoc (i: string) : associativity =
  let (_, a, _) = Hashtbl.find infixes i in a

let vdmsymb_infix_prio (i: string) : priority =
  let (p, _, _) = Hashtbl.find infixes i in p

let vdmsymb_prefix_prio (i: string) : priority =
  let (p, _) = Hashtbl.find prefixes i in p

let vdmsymb_postfix_prio (i: string) : priority =
  let (p, _) = Hashtbl.find postfixes i in p


let rec vdmterm2token (te: vdmterm) (p: place) : token =
  match te.ast with
    | TeBool b -> Verbatim (if b then "true" else "false")
    | TeBottom -> Verbatim "_|_"
    | TeNat1 i -> Verbatim (string_of_int i)
    | TeNat i -> Verbatim (string_of_int i)
    | TeInt i -> Verbatim (string_of_int i)
    | TeReal r -> Verbatim (string_of_float r)
    | TeChar c -> verbatims ["'"; String.make 1 c; "'"]
    | TeApp (f, args) -> 
      Box ([verbatims [f; "("]] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun te -> vdmterm2token te InArg) args
	       )
	      ) @
	      [verbatims [")"]]
      )
    | TeJoker -> Verbatim "-"
    | TeDotDotDot -> Verbatim "..."
    | TeName n -> Verbatim n
    | TeToken te -> Box [Verbatim "mk_token("; vdmterm2token te Alone; Verbatim ")"]
    | TeFieldAccess (te, n) -> Box [vdmterm2token te InArg; verbatims ["."; n]]
    | TeSetEnum tes -> 
      Box ([Verbatim "{"] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun te -> vdmterm2token te InArg) tes
	       )
	      ) @
	      [verbatims ["}"]]
      )

    | TeSetComprehension (te, tes, te') -> 
      Box ([Verbatim "{"; Space 1; vdmterm2token te Alone; Space 1; Verbatim "|"; Space 1] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun te -> vdmterm2token te InArg) tes
	       )
	      ) @
	      (match te' with
		| None -> []
		| Some te' -> [Space 1; Verbatim "&"; Space 1; vdmterm2token te' Alone; Space 1]
	      ) @ [Verbatim "}"]
      )
    | TeSeqComprehension (te, tes, te') -> 
      Box ([Verbatim "["; Space 1; vdmterm2token te Alone; Space 1; Verbatim "|"; Space 1] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun te -> vdmterm2token te InArg) tes
	       )
	      ) @
	      (match te' with
		| None -> []
		| Some te' -> [Space 1; Verbatim "&"; Space 1; vdmterm2token te' Alone; Space 1]
	      ) @ [Verbatim "]"]
      )
    | TeSeqEnum tes -> 
      Box ([Verbatim "["] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun te -> vdmterm2token te InArg) tes
	       )
	      ) @
	      [verbatims ["]"]]
      )
    | TeMapEnum [] -> Verbatim "{|->}"
    | TeMapEnum tes -> 
      Box ([Verbatim "{"] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun (te1, te2) -> Box [vdmterm2token te1 InArg; Space 1; Verbatim "|->"; Space 1; vdmterm2token te2 InArg]) tes
	       )
	      ) @
	      [verbatims ["}"]]
      )
    | TeInSet (te1, te2) ->
      Box [vdmterm2token te1 Alone; Space 1; Verbatim "in set"; Space 1; vdmterm2token te2 Alone]
    | TeInSetDom (te1, te2) ->
      Box [vdmterm2token te1 Alone; Space 1; Verbatim "in set dom"; Space 1; vdmterm2token te2 Alone]
    | TeInSetRng (te1, te2) ->
      Box [vdmterm2token te1 Alone; Space 1; Verbatim "in set rng"; Space 1; vdmterm2token te2 Alone]
    | TeIfte (te1, te2, te3) -> 
      Box [Verbatim "if"; Space 1; vdmterm2token te1 Alone; Space 1; Verbatim "then"; Space 1; vdmterm2token te2 Alone; Space 1; Verbatim "else"; Space 1; vdmterm2token te3 Alone]
    | TeLetIn (cases, te3) -> 
      Box [Verbatim "let"; Space 1;
	   Box (intercalate Newline (
	     List.map (fun (te1, te2) -> Box [vdmterm2token te1 Alone; Space 1; Verbatim "="; Space 1; vdmterm2token te2 Alone]) cases
	   )
	   ); Space 1; Verbatim "in"; Space 1;
	   vdmterm2token te3 Alone
	  ]
    | TeLetInSt (te1, te2, te3) -> 
      Box [Verbatim "let"; Space 1; vdmterm2token te1 Alone; Space 1; Verbatim "be st"; 
	   Space 1; vdmterm2token te2 Alone; Space 1; Verbatim "in"; Space 1; vdmterm2token te3 Alone
	  ]
    | TeIota (te1, te2) ->
      Box [Verbatim "iota"; Space 1; vdmterm2token te1 Alone; Space 1; Verbatim "&"; 
	   Space 1; vdmterm2token te2 Alone]
    | TeForall (te1, te2) ->
      Box ([Verbatim "forall"; Space 1] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun te1 -> vdmterm2token te1 Alone) te1
	       )
	      ) @ [Space 1 ; Verbatim "&"; Space 1; vdmterm2token te2 Alone]
      )
    | TeExists (te1, te2) ->
      Box ([Verbatim "forall"; Space 1] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun te1 -> vdmterm2token te1 Alone) te1
	       )
	      ) @ [Space 1 ; Verbatim "&"; Space 1; vdmterm2token te2 Alone]
      )
    | TeExistsUniq (te1, te2) ->
      Box ([Verbatim "forall"; Space 1] @
	      (intercalates [Verbatim ","; Space 1] (
		List.map (fun te1 -> vdmterm2token te1 Alone) te1
	       )
	      ) @ [Space 1 ; Verbatim "&"; Space 1; vdmterm2token te2 Alone]
      )
    | TeCase (te1, te2) ->
      Box ([Verbatim "cases"; Space 1; vdmterm2token te1 Alone; Space 1; Verbatim ":"; Newline] @
	      (List.map (fun (patterns, body) ->
		Box [Verbatim "|"; Space 1;
		     Box (intercalates [Verbatim ","; Space 1] (List.map (fun te -> vdmterm2token te Alone) patterns)); Verbatim "->"; Space 1; vdmterm2token body Alone
		]
	       ) te2
	      )
      )

    | TePrefix (pre, te) -> 
      let myprio = vdmsymb_prefix_prio pre in
      (match p with
	(* if we are an argument *)
	| InArg -> withParen
	(* if we are in a notation such that *)
	(* a prefix or postfix binding more  than us *)
	| InNotationPrefix (i, _) when i > myprio -> withParen
	| InNotationPostfix (i, _) when i > myprio -> withParen
	(* or another infix with higher priority *)
	| InNotationInfix (_, i, _) when i > myprio -> withParen
	(* or another infix with same priority depending on the associativity and position *)
	(* I am the second argument and its left associative *)
	| InNotationInfix (LeftAssoc, i, 2) when i = myprio -> withParen
	(* I am the first argument and its right associative *)
	| InNotationInfix (RightAssoc, i, 1) when i = myprio -> withParen

	(* else we do not need parenthesis *)
	| _ -> fun x -> x
      ) (
	Box [Verbatim pre; vdmterm2token te (InNotationPrefix (myprio, 1))]
       )

    | TePostfix (post, te) -> 
      let myprio = vdmsymb_postfix_prio post in
      (match p with
	| InArg -> withParen
	| InNotationPrefix (i, _) when i > myprio -> withParen
	| _ -> fun x -> x
      ) (
	Box [vdmterm2token te (InNotationPostfix (myprio, 1)); Verbatim post]
       )
    | TeInfix (inf, te1, te2) -> 
      let myassoc = vdmsymb_infix_assoc inf in
      let myprio = vdmsymb_infix_prio inf in
      (match p with
	(* if we are an argument *)
	| InArg -> withParen
	(* if we are in a notation such that *)
	(* a prefix or postfix binding more  than us *)
	| InNotationPrefix (i, _) when i > myprio -> withParen
	| InNotationPostfix (i, _) when i > myprio -> withParen
	(* or another infix with higher priority *)
	| InNotationInfix (_, i, _) when i > myprio -> withParen
	(* or another infix with same priority depending on the associativity and position *)
	(* I am the first argument and its left associative *)
	| InNotationInfix (LeftAssoc, i, 1) when i = myprio -> withParen
	(* I am the second argument and its right associative *)
	| InNotationInfix (RightAssoc, i, 2) when i = myprio -> withParen

	(* else we do not need parenthesis *)
	| _ -> fun x -> x
      ) (
	Box [vdmterm2token te1 (InNotationInfix (myassoc, myprio, 1)); Space 1; Verbatim inf; Space 1; vdmterm2token te2 (InNotationInfix (myassoc, myprio, 2))]
       )


let rec vdmtypedecl2token (decl: vdmtypedecl) : token =
  match decl with
    | TypeDecl (n, TyComp (n', fields), _, inv) when n = n' -> Box ([Verbatim n; Space 1; Verbatim "::"; Space 1; 
								      IBox (intercalate Newline (List.map ( fun field ->
									Box (
									  (match fst field with
									    | None -> []
									    | Some (n, true) -> [Verbatim n; Space 1; Verbatim ":"; Space 1]
									    | Some (n, false) -> [Verbatim n; Space 1; Verbatim ":-"; Space 1]
									  ) @
									    [vdmtype2token (snd field); Newline]
									)
								      ) fields))
								     ] @ (match inv with
								       | None -> []
								       | Some (p, def, _) ->
									 [verbatims ["inv_"; n; "("]; vdmterm2token p Alone; Verbatim ")"; Space 1; Verbatim "=="; Space 1; vdmterm2token def Alone]
    )
    )
    | TypeDecl (n, ty, _, None) -> Box [Verbatim n; Space 1; Verbatim "="; Space 1; vdmtype2token ty]
    | TypeDecl (n, ty, pos, Some (p, def, _)) -> 
      Box [Verbatim n; Space 1; Verbatim "="; Space 1; vdmtype2token ty; Newline;
	   verbatims ["inv_"; n]; Space 1; Verbatim ":"; Space 1; vdmtype2token (TyFct (TyName (n, pos), true, TyBool)); Newline; 
	   verbatims ["inv_"; n; "("]; vdmterm2token p Alone; Verbatim ")"; Space 1; Verbatim "=="; Space 1; vdmterm2token def Alone
	  ]

let rec vdmtermdecl2token (decl: vdmtermdecl) : token =
  match decl with
    | TeSignature (n, tys, ty) ->
      Box ([Verbatim n] @
	      ( match tys with
		| [] -> []
		| _ -> [Box ([Verbatim "["] @
				(intercalates [Verbatim ","; Space 1] (
				  List.map (fun n -> Verbatim n) tys
				 )
				) @
				[verbatims ["]"]]
	      
		)]) @
	      [Space 1; Verbatim ":"; Space 1; vdmtype2token ty]
      )
    | TeDef (n, args, body, pre, post, measure)->
      Box ([Verbatim n] @
	      ( match args with
		| [] -> []
		| _ -> [Box ([Verbatim "("] @
				(intercalates [Verbatim ","; Space 1] (
				  List.map (fun te -> vdmterm2token te Alone) args
				 )
				) @
				[verbatims [")"]]
	      
		)]) @
	      [Space 1; Verbatim "=="; Space 1; vdmterm2token body Alone] @
	      (match pre with
		| None -> []
		| Some pre -> [Verbatim "pre"; Space 1; Verbatim "=="; Space 1; vdmterm2token pre Alone ]
	      ) @
	      (match post with
		| None -> []
		| Some post -> [Verbatim "post"; Space 1; Verbatim "=="; Space 1; vdmterm2token post Alone]
	      ) @
	      (match measure with
		| None -> []
		| Some n -> [Verbatim "measure"; Space 1; Verbatim "=="; Space 1; Verbatim n]
	      )
      )



let rec vdmmoduledecl2token (m: vdmmoduledecl) : token =
  let tys, tes, _, _ = m in
  Box [
    Verbatim "types"; Newline; Newline;
    IBox (intercalate Newline (List.map vdmtypedecl2token tys)); Newline; Newline;
    Verbatim "functions"; Newline; Newline;
    IBox (intercalate Newline (List.map vdmtermdecl2token tes)); Newline;
  ]
