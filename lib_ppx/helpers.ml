open Ppxlib

let is_identifier_expr (expr : expression) : bool =
  match expr.pexp_desc with Pexp_ident _ -> true | _ -> false

let rec is_type_var_used_in_core_type var_name (ct : core_type) : bool =
  match ct.ptyp_desc with
  | Ptyp_var s ->
      s = var_name (* Check if it matches the variable name *)
  | Ptyp_arrow (_, ct1, ct2) ->
      is_type_var_used_in_core_type var_name ct1
      || is_type_var_used_in_core_type var_name ct2
  | Ptyp_tuple cts ->
      List.exists (is_type_var_used_in_core_type var_name) cts
  | Ptyp_constr (_, args) ->
      List.exists (is_type_var_used_in_core_type var_name) args
  | Ptyp_alias (ct, s) ->
      s.txt = var_name || is_type_var_used_in_core_type var_name ct
  | Ptyp_poly (_, ct) ->
      is_type_var_used_in_core_type var_name ct
  | _ ->
      false

(* Function to check if a type variable is used in a type_declaration *)
let is_type_var_used var_name (td : Parsetree.type_declaration) : bool =
  let is_used_in_manifest =
    match td.Parsetree.ptype_manifest with
    | Some ct ->
        is_type_var_used_in_core_type var_name ct
    | None ->
        false
  in
  let is_used_in_kind =
    match td.Parsetree.ptype_kind with
    | Parsetree.Ptype_abstract ->
        is_used_in_manifest
    | Parsetree.Ptype_variant constrs ->
        constrs
        |> List.map (fun c -> c.pcd_args)
        |> List.exists (function
             | Pcstr_tuple tuple ->
                 List.exists (is_type_var_used_in_core_type var_name) tuple
             | Pcstr_record record ->
                 record
                 |> List.map (fun label -> label.pld_type)
                 |> List.exists (is_type_var_used_in_core_type var_name) )
    | Parsetree.Ptype_record lbls ->
        List.exists
          (fun l -> is_type_var_used_in_core_type var_name l.Parsetree.pld_type)
          lbls
    | Parsetree.Ptype_open ->
        is_used_in_manifest
  in
  is_used_in_manifest || is_used_in_kind
