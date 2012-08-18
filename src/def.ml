open Libparser

type name = string

type vdmtype = TyBool
	       | TyNat1
	       | TyNat
	       | TyInt
	       | TyReal
	       | TyChar
	       | TyQuote of string

	       | TyVar of int
	       | TyName of string * pos

	       | TyToken

	       | TySet of vdmtype
	       | TySeq of vdmtype
	       | TySeq1 of vdmtype
	       | TyMap of vdmtype * vdmtype
	       | TyInjMap of vdmtype * vdmtype
	       | TyProd of vdmtype list
	       | TyComp of name * ((name * bool) option * vdmtype) list
	       | TyOption of vdmtype
	       | TyUnion of vdmtype list
	       | TyFct of vdmtype * bool * vdmtype

type vdmsymb_prefix = Not | Abs | Floor | Minus
type vdmsymb_postfix
type vdmsymb_infix = And | Or | Impl | Iff | Eq | Neq | LT | LE | GT | GE | Div | Rem | Mod | Power | Plus | Sub | Mult | Divide

let vdmsymb_infix_assoc (i: vdmsymb_infix) : associativity =
  match i with
    | Neq -> NoAssoc

let vdmsymb_infix_prio (i: vdmsymb_infix) : priority =
  match i with
    | Neq -> 200

let vdmsymb_prefix_prio (i: vdmsymb_prefix) : priority =
  0

type vdmterm_ast = TeBool of bool
		   | TeBottom

		   | TeInfix of vdmsymb_infix * vdmterm * vdmterm
		   | TePrefix of vdmsymb_prefix * vdmterm
		       
		   | TeNat1 of int
		   | TeNat of int
		   | TeInt of int
		   | TeReal of float
	       
		   | TeChar of char
		       
		   | TeQuote of string
		       
		   | TeName of string
		   | TeApp of name * vdmterm list

		   | TeToken of vdmterm

		   | TeFieldAccess of vdmterm * name

		   | TeSetEnum of vdmterm list

and vdmterm = {
  ast: vdmterm_ast;
  type_: vdmtype option;
  pos: pos;
}



let build_term ?(pos: pos = nopos) (a: vdmterm_ast) : vdmterm =
  { ast = a; type_ = None; pos = pos }

type vdmtypedecl = TypeDecl of name * vdmtype * pos * (vdmterm * vdmterm * pos) option
