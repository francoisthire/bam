val derive_type_declarations :
     Ppxlib.Asttypes.rec_flag
  -> Ppxlib.type_declaration list
  -> Ppxlib.structure_item list

val derive_module_type_declaration :
  ctxt:'a -> Ppxlib.module_type_declaration -> Ppxlib.structure_item list
