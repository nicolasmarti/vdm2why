open def




(*
 workflow of type checking
 - traverse the typedeclaration
   * add the name of type to a global type list
   * check and add the types for the constructor to a global map (name |-> type)
 - traverse the vdmtermdecl
   * check and add the signature to a global map (name |-> type)
 - traverse the typedeclaration
   * check and add the invariant to a global map (name |-> type)
 - traverse the vdmtermdecl
   * check and add the definitions to a global map (name |-> type)
 - traverse the value, the operation
   * check and add the definitions to a global map (name |-> type)
*)
