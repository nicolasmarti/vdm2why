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
	       | TyFct of vdmtype list * bool * vdmtype

type vdmterm_ast = TeBool of bool
		   | TeBottom
		   | Not of vdmterm
		   | And of vdmterm * vdmterm
		   | Or of vdmterm * vdmterm
		   | Impl of vdmterm * vdmterm
		   | Iff of vdmterm * vdmterm
		       
		   | Eq of vdmterm * vdmterm
		   | Neq of vdmterm * vdmterm
		   | LT of vdmterm * vdmterm
		   | LE of vdmterm * vdmterm
		   | GT of vdmterm * vdmterm
		   | GE of vdmterm * vdmterm
		       
		   | TeNat1 of int
		   | TeNat of int
		   | TeInt of int
		   | TeReal of float
		       
		   | Abs of vdmterm
		   | Floor of vdmterm
		   | Minus of vdmterm
		       
		   | Add of vdmterm * vdmterm
		   | Sub of vdmterm * vdmterm
		   | Mult of vdmterm * vdmterm
		   | Divide of vdmterm * vdmterm
		       
		   | Div of vdmterm * vdmterm
		   | Rem of vdmterm * vdmterm
		   | Mod of vdmterm * vdmterm
		       
		   | Power of vdmterm * vdmterm
		       
		   | TeChar of char
		       
		   | TeQuote of string
		       
		   | TeName of string
		   | TeApp of name * vdmterm list

		   | TeToken of vdmterm

		   | TeFieldAccess of vdmterm * name

and vdmterm = {
  ast: vdmterm_ast;
  type_: vdmtype option;
  pos: pos;
}

let build_term ?(pos: pos = nopos) (a: vdmterm_ast) : vdmterm =
  { ast = a; type_ = None; pos = pos }

type vdmtypedecl = TypeDecl of name * vdmtype * pos * (vdmterm * vdmterm * pos) option
