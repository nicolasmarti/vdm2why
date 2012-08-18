
type name = string

type vdmtype = TyBool
	       | TyNat1
	       | TyNat
	       | TyInt
	       | TyReal
	       | TyChar
	       | TyQuote of string

type vdmterm = TeBool of bool
	       | Bottom
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

type vdmtypedecl = TypeDecl of name * vdmtype
		   | InvDecl of vdmterm * vdmterm
