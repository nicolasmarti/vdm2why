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


type vdmterm_ast = TeBool of bool
		   | TeBottom

		   | TeInfix of string * vdmterm * vdmterm
		   | TePrefix of string * vdmterm
		   | TePostfix of string * vdmterm
		       
		   | TeNat1 of int
		   | TeNat of int
		   | TeInt of int
		   | TeReal of float
	       
		   | TeChar of char
		       
		   | TeQuote of string
		       
		   | TeName of string

		   | TeDotDotDot
		   | TeJoker

		   | TeApp of name * vdmterm list

		   | TeToken of vdmterm

		   | TeFieldAccess of vdmterm * name

		   | TeSetEnum of vdmterm list
		   | TeSetComprehension of vdmterm * vdmterm list * vdmterm
		   | TeInSet of vdmterm * vdmterm
		   | TeInSetDom of vdmterm * vdmterm
		   | TeInSetRng of vdmterm * vdmterm

		   | TeMapEnum of (vdmterm * vdmterm) list

		   | TeSeqEnum of vdmterm list
		   | TeSeqComprehension of vdmterm * vdmterm list * vdmterm
		   
		   | TeIfte of vdmterm * vdmterm * vdmterm
		   | TeLetIn of (vdmterm * vdmterm) list * vdmterm
		   | TeLetInSt of vdmterm * vdmterm * vdmterm

		   | TeIota of vdmterm * vdmterm
		   | TeForall of vdmterm list * vdmterm
		   | TeExists of vdmterm list * vdmterm
		   | TeExistsUniq of vdmterm list * vdmterm

		   | TeCase of vdmterm * (vdmterm list * vdmterm) list

and vdmterm = {
  ast: vdmterm_ast;
  type_: vdmtype option;
  pos: pos;
}

let build_term ?(pos: pos = nopos) (a: vdmterm_ast) : vdmterm =
  { ast = a; type_ = None; pos = pos }


type vdmtypedecl = TypeDecl of name * vdmtype * pos * (vdmterm * vdmterm * pos) option

type vdmtermdecl = TeSignature of name * name list * vdmtype
		   | TeDef of name * vdmterm list * vdmterm * vdmterm option * vdmterm option * name option

type vdmvaluedecl = Value of vdmterm * vdmtype option * vdmterm

type vdmmoduledecl = vdmtypedecl list * vdmtermdecl list * vdmvaluedecl list
		      
