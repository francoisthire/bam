let deriving_str_type_declaration ~ctxt:_ (rec_flag, type_declarations) =
  Deriver.derive_type_declarations rec_flag type_declarations

let str_type_decl =
  Ppxlib.Deriving.Generator.V2.make_noarg deriving_str_type_declaration

let deriving_str_module_type_decl = Deriver.derive_module_type_declaration

let str_module_type_decl =
  Ppxlib.Deriving.Generator.V2.make_noarg deriving_str_module_type_decl

let deriver = Ppxlib.Deriving.add "gen" ~str_type_decl ~str_module_type_decl
