open Def
open Libparser

open Str
open Printf

let at_start_pos (startp: (int * int)) (p: 'a parsingrule) : 'a parsingrule =
  fun pb ->
    let curp = cur_pos pb in
    if (snd startp > snd curp) then (
      (*printf "%d > %d\n" (snd startp) (snd curp);*)
      raise NoMatch
    );
    p pb

let after_start_pos (startp: (int * int)) (p: 'a parsingrule) : 'a parsingrule =
  fun pb ->
    let curp = cur_pos pb in
    if (snd startp >= snd curp) then (
      (*printf "%d >= %d\n" (snd startp) (snd curp);*)
      raise NoMatch
    );
    p pb

let with_pos (p: 'a parsingrule) : ('a * pos) parsingrule =
  fun pb ->
    let startp = cur_pos pb in
    let res = p pb in
    let endp = cur_pos pb in
    (res, (startp, endp))

(* some basic parsers *)
let parse_real : float parsingrule =
  applylexingrule (regexp "[+-]?[0-9]+.[0-9]*", 
		   fun (s:string) -> 
		     try
		       float_of_string s
		     with
		       | _ -> printf "cannot make a float of %s." s; raise NoMatch
  )
;;

let parse_nat1 = applylexingrule (regexp "[1-9][0-9]*", fun (s:string) -> int_of_string s)
let parse_nat = applylexingrule (regexp "[0-9]+", fun (s:string) -> int_of_string s)
let parse_int = applylexingrule (regexp "[+-]?[0-9]+", fun (s:string) -> int_of_string s)

let keywords = ["true"; "false"; "not"; "and"; "or"; 
		"abs"; "floor"; "div"; "rem"; "mod";
		"inv"; "set"; "of"; "seq"; "seq1"; "map"; 
		"to"; "inmap"; "compose"; "token"; "mk_token"]

let parse_keywords : unit parsingrule =
  foldp (List.map (fun x -> keyword x ()) keywords)

let parse_name : name parsingrule = applylexingrule (regexp "[a-zA-Z][a-zA-Z0-9_]*", 
						      fun (s:string) -> 
							if List.mem s keywords then raise NoMatch else s
)

let parse_char : char parsingrule = applylexingrule (regexp ".", 
						      fun (s:string) -> 
							String.get s 0
)


(* the prefix/infix/postfix parsers *)
let prefixes : (string, (priority * (pos -> vdmterm -> vdmterm))) Hashtbl.t = Hashtbl.create 100
let infixes : (string, (priority * associativity * (pos -> vdmterm -> vdmterm -> vdmterm))) Hashtbl.t = Hashtbl.create 100
let postfixes : (string, (priority * (pos -> vdmterm -> vdmterm))) Hashtbl.t = Hashtbl.create 100;;

Hashtbl.add prefixes "not" (100, fun pos x -> build_term ~pos:pos (Not x));;
Hashtbl.add prefixes "-" (300, fun pos x -> build_term ~pos:pos (Minus x));;

Hashtbl.add infixes "and" (90, RightAssoc, fun pos x y -> build_term ~pos:pos (And (x, y)));;
Hashtbl.add infixes "or" (80, RightAssoc, fun pos x y -> build_term ~pos:pos (Or (x, y)));;
Hashtbl.add infixes "=>" (70, RightAssoc, fun pos x y -> build_term ~pos:pos (Impl (x, y)));;
Hashtbl.add infixes "<=>" (70, RightAssoc, fun pos x y -> build_term ~pos:pos (Iff (x, y)));;

Hashtbl.add infixes "-" (300, RightAssoc, fun pos x y -> build_term ~pos:pos (Sub (x, y)));;
Hashtbl.add infixes "+" (300, RightAssoc, fun pos x y -> build_term ~pos:pos (Add (x, y)));;
Hashtbl.add infixes "*" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (Mult (x, y)));;
Hashtbl.add infixes "/" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (Divide (x, y)));;
Hashtbl.add infixes "div" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (Div (x, y)));;
Hashtbl.add infixes "rem" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (Rem (x, y)));;
Hashtbl.add infixes "mod" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (Rem (x, y)));;
Hashtbl.add infixes "**" (320, RightAssoc, fun pos x y -> build_term ~pos:pos (Power (x, y)));;

Hashtbl.add infixes ">=" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (GE (x, y)));;
Hashtbl.add infixes "<=" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (LE (x, y)));;
Hashtbl.add infixes "<" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (LT (x, y)));;
Hashtbl.add infixes ">" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (GT (x, y)));;

Hashtbl.add infixes "=" (200, NoAssoc, fun pos x y -> build_term ~pos:pos (Eq (x, y)));;
Hashtbl.add infixes "<>" (200, NoAssoc, fun pos x y -> build_term ~pos:pos (Neq (x, y)));;



(* the type parser *)
let rec parse_type ?(leftmost: int * int = -1, -1) (pb: parserbuffer) : vdmtype = begin 
  error (
  (* product *)
  tryrule (fun pb ->
    let () = whitespaces pb in
    let tys = separatedBy (parse_type_lvl0 ~leftmost:leftmost) 
      (fun pb ->
	let () = whitespaces pb in
	let () = after_start_pos leftmost (word "*") pb in
	let () = whitespaces pb in
	()
      ) pb in
    let () = whitespaces pb in
    match tys with
      | [] -> raise NoMatch
      | hd::[] -> raise NoMatch
      | _ ->  TyProd (tys)
  )
  (* product *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let tys = separatedBy (parse_type_lvl0 ~leftmost:leftmost) 
      (fun pb ->
	let () = whitespaces pb in
	let () = after_start_pos leftmost (word "|") pb in
	let () = whitespaces pb in
	()
      ) pb in
    let () = whitespaces pb in
    match tys with
      | [] -> raise NoMatch
      | hd::[] -> hd
      | _ ->  TyUnion (tys)
  )
  (* normal *)
  <|> tryrule (parse_type_lvl0 ~leftmost:leftmost)
  ) "not a valid type"
end pb

and parse_type_lvl0 ?(leftmost: int * int = -1, -1) (pb: parserbuffer) : vdmtype = begin 
  error (
  (* bool *)
  tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "bool") pb in
    let () = whitespaces pb in
    TyBool
  )
  (* numbers *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "nat1") pb in
    let () = whitespaces pb in
    TyNat1
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "nat") pb in
    let () = whitespaces pb in
    TyNat
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "int") pb in
    let () = whitespaces pb in
    TyInt
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "real") pb in
    let () = whitespaces pb in
    TyReal
  )
  (* char *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "char") pb in
    let () = whitespaces pb in
    TyChar
  )
  (* token *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "token") pb in
    let () = whitespaces pb in
    TyChar
  )
  (* quote *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "<") pb in
    let q = parse_name pb in
    let () = word ">" pb in
    let () = whitespaces pb in
    TyQuote q
  )
  (* set of *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "set") pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "of") pb in
    let () = whitespaces pb in
    let ty = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    TySet ty
  )
  (* seq *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "seq") pb in
    let () = whitespaces pb in
    let ty = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    TySeq ty
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "seq1") pb in
    let () = whitespaces pb in
    let ty = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    TySeq1 ty
  )
  (* map *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "map") pb in
    let () = whitespaces pb in
    let ty1 = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "to") pb in
    let () = whitespaces pb in
    let ty2 = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    TyMap (ty1, ty2)
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "inmap") pb in
    let () = whitespaces pb in
    let ty1 = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "to") pb in
    let () = whitespaces pb in
    let ty2 = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    TyInjMap (ty1, ty2)
  )
  (* composite types *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let n = after_start_pos leftmost parse_name pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "::") pb in
    let () = whitespaces pb in
    let fields = 
      many1 (fun pb ->
	let name = error (
	  mayberule (fun pb ->
	    let () = whitespaces pb in
	    let n' = after_start_pos leftmost parse_name pb in
	    let () = whitespaces pb in
	    let b = (tryrule (fun pb -> after_start_pos leftmost (word ":-") pb; false) <|> tryrule (fun pb -> after_start_pos leftmost (word ":") pb; true)) pb in
	    let () = whitespaces pb in
	    n', b
	  )
	) "not a valid field" pb in 
	let ty = parse_type ~leftmost:leftmost pb in
	name, ty
      ) pb in
    let () = whitespaces pb in
    TyComp (n, fields)
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "compose") pb in
    let () = whitespaces pb in
    let n = after_start_pos leftmost parse_name pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "of") pb in
    let () = whitespaces pb in
    let fields = 
      many1 (fun pb ->
	let name = error (
	  mayberule (fun pb ->
	    let () = whitespaces pb in
	    let n' = after_start_pos leftmost parse_name pb in
	    let () = whitespaces pb in
	    let b = (tryrule (fun pb -> after_start_pos leftmost (word ":-") pb; false) <|> tryrule (fun pb -> after_start_pos leftmost (word ":") pb; true)) pb in
	    let () = whitespaces pb in
	    n', b
	  )
	) "not a valid field" pb in 
	let ty = parse_type ~leftmost:leftmost pb in
	name, ty
      ) pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "end") pb in
    let () = whitespaces pb in
    TyComp (n, fields)
  )
  (* option *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "[") pb in
    let () = whitespaces pb in
    let ty = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "]") pb in
    let () = whitespaces pb in
    TyOption ty
  )
  (* function *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "(") pb in
    let () = whitespaces pb in
    let tys = separatedBy
      (parse_type ~leftmost:leftmost) 
      (fun pb ->
	let () = whitespaces pb in
	let () = after_start_pos leftmost (word ",") pb in
	let () = whitespaces pb in
	()
      )
      pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word ")") pb in
    let () = whitespaces pb in
    let b = (tryrule (fun pb -> after_start_pos leftmost (word "->") pb; false) <|> tryrule (fun pb -> after_start_pos leftmost (word "+>") pb; true)) pb in
    let () = whitespaces pb in
    let ty = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    TyFct (tys, b, ty)
  )
  (* a name *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let n, pos = with_pos (after_start_pos leftmost parse_name) pb in
    let () = whitespaces pb in
    TyName (n, pos)
  )
  (* with paren *)
  <|> tryrule (paren (parse_type ~leftmost:leftmost))

  ) "not a valid type"
end pb

(* the term parser *)
let rec parse_term ?(leftmost: int * int = -1, -1) (pb: parserbuffer) : vdmterm = begin 
  error (
  (* field access *)
  tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let te = parse_basic_term ~leftmost:leftmost pb in
    let _ = after_start_pos leftmost (word ".") pb in
    let n = after_start_pos leftmost parse_name pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (TeFieldAccess (te, n))
  )
  <|>  tryrule (parse_basic_term ~leftmost:leftmost)
  ) "not a valid term"
end pb

and parse_basic_term ?(leftmost: int * int = -1, -1) (pb: parserbuffer) : vdmterm = begin 
  (* true, false *)
  tryrule (fun pb ->
    let () = whitespaces pb in
    let (), pos = with_pos (after_start_pos leftmost (word "true")) pb in
    let () = whitespaces pb in
    build_term ~pos:pos (TeBool true)
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let (), pos = with_pos (after_start_pos leftmost (word "true")) pb in
    let () = whitespaces pb in
    build_term ~pos:pos (TeBool true)
  )
  (* numbers *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let n, pos = with_pos (after_start_pos leftmost parse_nat1) pb in
    let () = whitespaces pb in
    build_term ~pos:pos (TeNat1 n)
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let n, pos = with_pos (after_start_pos leftmost parse_nat) pb in
    let () = whitespaces pb in
    build_term ~pos:pos (TeNat n)
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let n, pos = with_pos (after_start_pos leftmost parse_int) pb in
    let () = whitespaces pb in
    build_term ~pos:pos (TeInt n)
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let n, pos = with_pos (after_start_pos leftmost parse_real) pb in
    let () = whitespaces pb in
    build_term ~pos:pos (TeReal n)
  )
  (* math *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let () = after_start_pos leftmost (word "abs") pb in
    let () = whitespaces pb in
    let te = after_start_pos leftmost parse_term pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (Abs te)
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let () = after_start_pos leftmost (word "floor") pb in
    let () = whitespaces pb in
    let te = after_start_pos leftmost parse_term pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (Floor te)
  )
  (* char *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let () = after_start_pos leftmost (word "'") pb in
    let c = parse_char pb in
    let () = word "'" pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (TeChar c)
  )
  (* quote *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let () = after_start_pos leftmost (word "<") pb in
    let q = parse_name pb in
    let () = word ">" pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (TeQuote q)
  )
  (* mk_token*)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let _ = after_start_pos leftmost (word "mk_token(") pb in
    let te = parse_term ~leftmost:leftmost pb in
    let _ = after_start_pos leftmost (word ")") pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (TeToken te)
  )
  (* name, function call *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let q, pos = with_pos (after_start_pos leftmost parse_name) pb in
    let args = mayberule (fun pb ->
      let () = after_start_pos leftmost (word "(") pb in
      let args = separatedBy (parse_term ~leftmost:leftmost) (word ",") pb in
      let () = after_start_pos leftmost (error (word ")") "missing closing parenthesis")pb in
      args
    ) pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    match args with
      | None -> build_term ~pos:pos (TeName q)
      | Some l -> build_term ~pos:(startpos, endpos) (TeApp (q, l))
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let te = paren (parse_term ~leftmost:leftmost) pb in
    let () = whitespaces pb in
    te
  )

end pb

and parse_vdmop : vdmterm opparser = {
  primary = parse_basic_term;
  prefixes = prefixes;
  infixes = infixes;
  postfixes = postfixes;
  reserved = (fun pb -> ());
}

and parse_op_term (pb: parserbuffer) : vdmterm = begin
  opparse parse_vdmop
end pb


let parse_type_decl (pb: parserbuffer) : vdmtypedecl = begin
  error (
  tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let name = parse_name pb in
    let () = whitespaces pb in
    let () = word "=" pb in
    let () = whitespaces pb in
    let ty= parse_type ~leftmost:startpos pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    let _ = mayberule (word ";") in
    let () = whitespaces pb in

    let inv = mayberule (fun pb ->
      let startpos = cur_pos pb in
      let () = word "inv" pb in
      let () = whitespaces pb in
      let pattern = parse_term ~leftmost:startpos pb in
      let () = whitespaces pb in
      let () = error (word "==") "inv should be defined with == " pb in
      let () = whitespaces pb in
      let def = parse_term ~leftmost:startpos pb in
      let endpos = cur_pos pb in
      let () = whitespaces pb in
      let _ = mayberule (word ";") in
      let () = whitespaces pb in
      (pattern, def, (startpos, endpos))
    ) pb in
    TypeDecl (name, ty, (startpos, endpos), inv)
  ) 
  ) "not a valide type declaration"
end pb
