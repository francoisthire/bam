open Ppxlib

val is_identifier_expr : expression -> bool

val is_type_var_used : string -> type_declaration -> bool
