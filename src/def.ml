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
	       | TyOp of vdmtype * vdmtype

	       | TyUnit


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
		   | TePat of string

		   | TeDotDotDot
		   | TeJoker

		   | TeApp of name * vdmterm list

		   | TeToken of vdmterm

		   | TeFieldAccess of vdmterm * name

		   | TeSetEnum of vdmterm list
		   | TeSetComprehension of vdmterm * vdmterm list * vdmterm option
		   | TeInSet of vdmterm * vdmterm
		   | TeInSetDom of vdmterm * vdmterm
		   | TeInSetRng of vdmterm * vdmterm

		   | TeMapEnum of (vdmterm * vdmterm) list
		   | TeMapComprehension of vdmterm * vdmterm * vdmterm list * vdmterm option

		   | TeSeqEnum of vdmterm list
		   | TeSeqComprehension of vdmterm * vdmterm list * vdmterm option
		   
		   | TeIfte of vdmterm * vdmterm * vdmterm
		   | TeLetIn of vdmterm list * vdmterm
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

type vdmstmt = {
  sast: vdmstmt_ast;
  stype_: vdmtype option;
  spos: pos;
}
and vdmstmt_ast = StmtSeq of vdmstmt * vdmstmt
		  | StmtAssign of vdmterm * vdmterm
		  | StmtLetIn of (vdmterm * vdmterm) list * vdmstmt
		  | StmtLetInSt of vdmterm * vdmterm * vdmstmt
		  | StmtApp of name * vdmterm list
		  | StmtDef of (vdmterm * vdmterm) list * vdmstmt
		  | StmtReturn of vdmterm
		  | StmtIfte of (vdmterm * vdmstmt) list * vdmstmt
		  | StmtDcl of (vdmterm * vdmterm) list * vdmstmt
		  | StmtCase of vdmterm * (vdmterm list * vdmstmt) list
		  | StmtForall of vdmterm * vdmstmt

let build_stmt ?(pos: pos = nopos) (a: vdmstmt_ast) : vdmstmt =
  { sast = a; stype_ = None; spos = pos }

type vdmtypedecl = TypeDecl of name * vdmtype * pos * (vdmterm * vdmterm * pos) option

type vdmtermdecl = TeSignature of name * name list * vdmtype
		   | TeDef of name * vdmterm list * vdmterm * vdmterm option * vdmterm option * name option

type vdmvaluedecl = Value of vdmterm * vdmtype option * vdmterm

type vdmstatedecl = State of name * ((name * bool) option * vdmtype) list * (vdmterm * vdmterm * pos) option * (vdmterm * vdmterm * pos) option

type vdmoperationdecl = Operation of name * vdmterm list * ((name * vdmtype) list) option * vdmstmt option * (bool * name list * vdmtype option) option * vdmterm option * vdmterm option * unit
			| OpSig of name * vdmtype

type vdmmoduledecl = vdmtypedecl list * vdmtermdecl list * vdmvaluedecl list * vdmstatedecl list * vdmoperationdecl list
		      
