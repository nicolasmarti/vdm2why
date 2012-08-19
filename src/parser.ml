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
		"to"; "inmap"; "compose"; "token"; "mk_token"; "len";
		"types"; "functions";
		"if"; "then"; "else"; "in"; "let"
	       ]

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

let parse_string : string parsingrule =
  any_except ["\""]
;;

let parse_comment : unit parsingrule =
  tryrule (
    fun pb ->
      let () = whitespaces pb in
      let () = word "--" pb in
      let _ = any_except_nl [] pb in
      let () = whitespaces pb in
      ()
  )
;;

let whitespaces = orrule parse_comment Libparser.whitespaces;;

(* the prefix/infix/postfix parsers *)
let prefixes : (string, (priority * (pos -> vdmterm -> vdmterm))) Hashtbl.t = Hashtbl.create 100
let infixes : (string, (priority * associativity * (pos -> vdmterm -> vdmterm -> vdmterm))) Hashtbl.t = Hashtbl.create 100
let postfixes : (string, (priority * (pos -> vdmterm -> vdmterm))) Hashtbl.t = Hashtbl.create 100;;

Hashtbl.add prefixes "not" (100, fun pos x -> build_term ~pos:pos (TePrefix ("not", x)));;
Hashtbl.add prefixes "-" (400, fun pos x -> build_term ~pos:pos (TePrefix ("-", x)));;
Hashtbl.add prefixes "abs" (310, fun pos x -> build_term ~pos:pos (TePrefix ("abs", x)));;
Hashtbl.add prefixes "floor" (310, fun pos x -> build_term ~pos:pos (TePrefix ("floor", x)));;
Hashtbl.add prefixes "len" (310, fun pos x -> build_term ~pos:pos (TePrefix ("len", x)));;

Hashtbl.add infixes "and" (90, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("and", x, y)));;
Hashtbl.add infixes "or" (80, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("or", x, y)));;
Hashtbl.add infixes "=>" (70, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("=>", x, y)));;
Hashtbl.add infixes "<=>" (70, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("<=>", x, y)));;

Hashtbl.add infixes "-" (300, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("-", x, y)));;
Hashtbl.add infixes "+" (300, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("+", x, y)));;
Hashtbl.add infixes "*" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("*", x, y)));;
Hashtbl.add infixes "/" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("/", x, y)));;
Hashtbl.add infixes "div" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("div", x, y)));;
Hashtbl.add infixes "rem" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("rem", x, y)));;
Hashtbl.add infixes "mod" (310, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("mod", x, y)));;
Hashtbl.add infixes "**" (320, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("**", x, y)));;

Hashtbl.add infixes ">=" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix (">=", x, y)));;
Hashtbl.add infixes "<=" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("<=", x, y)));;
Hashtbl.add infixes "<" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("<", x, y)));;
Hashtbl.add infixes ">" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix (">", x, y)));;

(* this is a bit dangerous *)
Hashtbl.add infixes "in set" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("in set", x, y)));;
Hashtbl.add infixes "not in set" (200, RightAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("not in set", x, y)));;

Hashtbl.add infixes "=" (200, NoAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("=", x, y)));;
Hashtbl.add infixes "<>" (200, NoAssoc, fun pos x y -> build_term ~pos:pos (TeInfix ("<>", x, y)));;



(* the type parser *)
let rec parse_type ?(leftmost: int * int = -1, -1) (pb: parserbuffer) : vdmtype = begin 
  (* function *)
  tryrule (fun pb ->
    let () = whitespaces pb in
    let ty1 = (parse_type_lvl1 ~leftmost:leftmost) pb in
    let () = whitespaces pb in
    let b = (tryrule (fun pb -> after_start_pos leftmost (word "->") pb; false) <|> tryrule (fun pb -> after_start_pos leftmost (word "+>") pb; true)) pb in
    let () = whitespaces pb in
    let ty2 = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    TyFct (ty1, b, ty2)
  )
  <|> tryrule (parse_type_lvl1 ~leftmost:leftmost)
end pb

and parse_type_lvl1 ?(leftmost: int * int = -1, -1) (pb: parserbuffer) : vdmtype = begin 
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
    TyToken
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
    let () = after_start_pos leftmost (word "of") pb in
    let () = whitespaces pb in
    let ty = parse_type ~leftmost:leftmost pb in
    let () = whitespaces pb in
    TySeq ty
  )
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "seq1") pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "of") pb in
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
    tryrule parse_op_term
    <|> tryrule (parse_term_lvl1 ~leftmost:leftmost)
  ) "not a valid term"
end pb

and parse_term_lvl1 ?(leftmost: int * int = -1, -1) (pb: parserbuffer) : vdmterm = begin 
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
  <|> tryrule (parse_basic_term ~leftmost:leftmost)
  ) "not a valid term"
end pb

and parse_basic_term ?(leftmost: int * int = -1, -1) (pb: parserbuffer) : vdmterm = begin 
  error (
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
  (* enumeration set *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let _ = after_start_pos leftmost (word "{") pb in
    let () = whitespaces pb in
    let tes = separatedBy (parse_term ~leftmost:leftmost) 
      (fun pb ->
	let () = whitespaces pb in
	let () = after_start_pos leftmost (word ",") pb in
	let () = whitespaces pb in
	()
      ) pb in
    let () = whitespaces pb in
    let _ = after_start_pos leftmost (word "}") pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (TeSetEnum tes)
  )
  (* comprehension set *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let _ = after_start_pos leftmost (word "{") pb in
    let () = whitespaces pb in
    let pat = parse_term ~leftmost:leftmost pb in
    let () = whitespaces pb in
    let _ = after_start_pos leftmost (word "|") pb in
    let () = whitespaces pb in
    let preds = separatedBy (parse_term ~leftmost:leftmost) 
      (fun pb ->
	let () = whitespaces pb in
	let () = after_start_pos leftmost (word ",") pb in
	let () = whitespaces pb in
	()
      ) pb in
    let () = whitespaces pb in
    let _ = after_start_pos leftmost (word "}") pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (TeSetComprehension (pat, preds))
  )
  (* enumeration list *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let _ = after_start_pos leftmost (word "[") pb in
    let () = whitespaces pb in
    let tes = separatedBy (parse_term ~leftmost:leftmost) 
      (fun pb ->
	let () = whitespaces pb in
	let () = after_start_pos leftmost (word ",") pb in
	let () = whitespaces pb in
	()
      ) pb in
    let () = whitespaces pb in
    let _ = after_start_pos leftmost (word "]") pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (TeSeqEnum tes)
  )
  (* joker *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let q, pos = with_pos (after_start_pos leftmost (word "-")) pb in
    let _ = notp (parse_term ~leftmost:leftmost) pb in
    let () = whitespaces pb in
    build_term ~pos:pos TeJoker
  )
  (* ifte *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let () = after_start_pos leftmost (word "if") pb in
    let () = whitespaces pb in
    let te1 = parse_term ~leftmost:leftmost pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "then") pb in
    let () = whitespaces pb in
    let te2 = parse_term ~leftmost:leftmost pb in
    let () = whitespaces pb in
    let () = after_start_pos leftmost (word "else") pb in
    let () = whitespaces pb in
    let te3 = parse_term ~leftmost:leftmost pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (TeIfte (te1, te2, te3))
  )
  (* let in *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let () = after_start_pos leftmost (word "let") pb in
    let tes = many1 (fun pb ->
      let () = whitespaces pb in
      let te1 = parse_term ~leftmost:leftmost pb in
      let () = whitespaces pb in
      let () = after_start_pos leftmost (word "=") pb in
      let () = whitespaces pb in
      let te2 = parse_term ~leftmost:leftmost pb in
      let () = whitespaces pb in
      te1, te2
    ) pb in
    let () = after_start_pos leftmost (word "in") pb in
    let () = whitespaces pb in
    let te3 = parse_term ~leftmost:leftmost pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    build_term ~pos:(startpos, endpos) (TeLetIn (tes, te3))
  )
  (* string *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let () = after_start_pos leftmost (word "\"") pb in
    let s = parse_string pb in
    let () = after_start_pos leftmost (word "\"") pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    let acc = ref [] in
    String.iter (fun c -> acc := (build_term (TeChar c))::!acc) s;
    build_term ~pos:(startpos, endpos) (TeSeqEnum (List.rev !acc))
  )
  (* name, function call *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let startpos = cur_pos pb in
    let q, pos = with_pos (after_start_pos leftmost parse_name) pb in
    let () = whitespaces pb in
    let args = mayberule (fun pb ->
      let () = after_start_pos leftmost (word "(") pb in
      let () = whitespaces pb in
      let args = separatedBy (parse_term ~leftmost:leftmost) 
	(fun pb ->
	  let () = whitespaces pb in
	  let () = after_start_pos leftmost (word ",") pb in
	  let () = whitespaces pb in
	  ()
	) pb in
      let () = whitespaces pb in
      let () = after_start_pos leftmost (error (word ")") "missing closing parenthesis")pb in
      args
    ) pb in
    let endpos = cur_pos pb in
    let () = whitespaces pb in
    match args with
      | None -> build_term ~pos:pos (TeName q)
      | Some l -> build_term ~pos:(startpos, endpos) (TeApp (q, l))
  )
  (* paren *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let te = paren (parse_term ~leftmost:leftmost) pb in
    let () = whitespaces pb in
    te
  )
  ) "not a valid term"
end pb

and parse_vdmop: vdmterm opparser = {
  primary = parse_term_lvl1;
  prefixes = prefixes;
  infixes = infixes;
  postfixes = postfixes;
  reserved = (fun pb -> raise NoMatch);
}

and parse_op_term (pb: parserbuffer) : vdmterm = begin
  opparse parse_vdmop
end pb


let parse_type_decl (pb: parserbuffer) : vdmtypedecl = begin
  error (
  tryrule (fun pb ->
    let name, ty, pos = (
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
	let _ = mayberule (word ";") pb in
	let () = whitespaces pb in
	name, ty, (startpos, endpos)
      )
      (* composite types *)
      <|> tryrule (fun pb ->
	let () = whitespaces pb in
	let startpos = cur_pos pb in
	let n = parse_name pb in
	let () = whitespaces pb in
	let () = after_start_pos startpos (word "::") pb in
	let () = whitespaces pb in
	let fields = 
	  many1 (fun pb ->
	    let name = error (
	      mayberule (fun pb ->
		let () = whitespaces pb in
		let n' = after_start_pos startpos parse_name pb in
		let () = whitespaces pb in
		let b = (tryrule (fun pb -> after_start_pos startpos (word ":-") pb; false) <|> tryrule (fun pb -> after_start_pos startpos (word ":") pb; true)) pb in
		let () = whitespaces pb in
		n', b
	      )
	    ) "not a valid field" pb in 
	    let ty = parse_type ~leftmost:startpos pb in
	    name, ty
	  ) pb in
	let endpos = cur_pos pb in
	let () = whitespaces pb in
	let _ = mayberule (word ";") pb in
	let () = whitespaces pb in
	n, TyComp (n, fields), (startpos, endpos)
      )) pb in


    let inv = mayberule (error (fun pb ->
      let startpos = cur_pos pb in
      let () = word "inv" pb in
      let () = whitespaces pb in
      let pattern = parse_term ~leftmost:startpos pb in
      let () = whitespaces pb in
      let () = error (word "==") "inv should be defined with == " pb in
      let () = whitespaces pb in
      let def = (parse_term ~leftmost:startpos) pb in
      let endpos = cur_pos pb in
      let () = whitespaces pb in
      let _ = mayberule (word ";") pb in
      let () = whitespaces pb in
      (pattern, def, (startpos, endpos))
    ) "not an invariant") pb in
    let () = whitespaces pb in
    let () = whitespaces pb in
    TypeDecl (name, ty, pos, inv)
  ) 
  ) "not a valide type declaration"
end pb

let parse_term_decl (pb: parserbuffer) : vdmtermdecl = begin
  error (
  (* Signature *)
  tryrule (fun pb ->
    let startpos = cur_pos pb in
    let n = parse_name pb in
    let tyvars = mayberule (fun pb ->

      let _ = after_start_pos startpos (word "[") pb in
      let () = whitespaces pb in
      let tes = separatedBy parse_name 
	(fun pb ->
	  let () = whitespaces pb in
	  let () = after_start_pos startpos (word ",") pb in
	  let () = whitespaces pb in
	  ()
	) pb in
      let () = whitespaces pb in
      let _ = after_start_pos startpos (word "]") pb in
      tes

    ) pb in

    let () = whitespaces pb in
    let _ = after_start_pos startpos (word ":") pb in
    let () = whitespaces pb in
    let ty = parse_type ~leftmost:startpos pb in
    let () = whitespaces pb in
    let _ = mayberule (word ";") pb in
    let () = whitespaces pb in

    TeSignature (n,
		 (match tyvars with | None -> [] | Some l -> l),
		 ty
    )
  )
  (* Definition *)
  <|> tryrule (fun pb ->
    let startpos = cur_pos pb in
    let n = parse_name pb in
    let () = whitespaces pb in
    let _ = after_start_pos startpos (word "(") pb in
    let () = whitespaces pb in
    let args = separatedBy (parse_term ~leftmost:startpos)
      (fun pb ->
	let () = whitespaces pb in
	let () = after_start_pos startpos (word ",") pb in
	let () = whitespaces pb in
	()
      ) pb in
    let () = whitespaces pb in
    let _ = after_start_pos startpos (word ")") pb in
    let () = whitespaces pb in
    let _ = after_start_pos startpos (word "==") pb in
    let () = whitespaces pb in
    let body = parse_term ~leftmost:startpos pb in
    let () = whitespaces pb in
    let _ = mayberule (word ";") pb in
    let () = whitespaces pb in
    
    TeDef (n,
	   args,
	   body,
	   None,
	   None,
	   None
    )
  )
  ) "not a valide term declaration"
end pb

let parse_module_decl (pb: parserbuffer) : vdmmoduledecl = begin
  error (
  (* parse types *)
  tryrule (fun pb ->
    let () = whitespaces pb in
    let () = word "types" pb in
    let () = whitespaces pb in
    let tys = many parse_type_decl pb in
    (tys, [])
  )
  (* parse functions *)
  <|> tryrule (fun pb ->
    let () = whitespaces pb in
    let () = word "functions" pb in
    let () = whitespaces pb in
    let tes = many parse_term_decl pb in
    ([], tes)
  )
  ) "not a valide module declaration"
end pb
