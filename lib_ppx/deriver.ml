open Ppxlib

type env = {recursive_types: string list; runtime: Runtime.t}

let loc = !Ast_helper.default_loc

let gen_name ident = match ident with "t" -> "gen" | _ -> "gen_" ^ ident

let derive_type_variable ident = Ast_builder.Default.evar ~loc (gen_name ident)

let rec derive_core_type env core_type =
  (* We want to ensure that if the user specified a shrinker, it will be used once and not used recursively. *)
  let env = {env with runtime= {env.runtime with shrinker= None}} in
  let core_type, runtime = Attributes.Core_type.update core_type env.runtime in
  let env = {env with runtime} in
  match core_type with
  | [%type: unit] ->
      Runtime.get runtime Unit
  | [%type: bool] | [%type: Bool.t] ->
      Runtime.get runtime Bool
  | [%type: char] | [%type: Char.t] ->
      Runtime.get runtime Char
  | [%type: int] | [%type: Int.t] ->
      Runtime.get runtime (Ranged Int)
  | [%type: int32] | [%type: Int32.t] ->
      Runtime.get runtime (Ranged Int32)
  | [%type: int64] | [%type: Int64.t] ->
      Runtime.get runtime (Ranged Int64)
  | [%type: string] | [%type: String.t] ->
      Runtime.get runtime (Sized String)
  | [%type: bytes] | [%type: Bytes.t] ->
      Runtime.get runtime (Sized Bytes)
  | [%type: [%t? ty] option] | [%type: [%t? ty] Option.t] ->
      let env = {env with runtime= {env.runtime with gen= None}} in
      let gen_ty = derive_core_type env ty in
      Runtime.get runtime Option gen_ty
  | [%type: [%t? ty] list] | [%type: [%t? ty] List.t] ->
      let env = {env with runtime= {env.runtime with gen= None}} in
      let gen_ty = derive_core_type env ty in
      Runtime.get runtime (Sized List) gen_ty
  | [%type: [%t? ty] array] | [%type: [%t? ty] Array.t] ->
      let env = {env with runtime= {env.runtime with gen= None}} in
      let gen_ty = derive_core_type env ty in
      Runtime.get runtime (Sized Array) gen_ty
  | [%type: [%t? ty] Seq.t] ->
      let env = {env with runtime= {env.runtime with gen= None}} in
      let gen_ty = derive_core_type env ty in
      Runtime.get runtime (Sized Seq) gen_ty
  | {ptyp_desc= Ptyp_tuple tuple; _} ->
      derive_tuple env tuple `Core_type
  | {ptyp_desc= Ptyp_constr ({txt= Lident ident; _}, type_args); _} ->
      derive_type_constr env ident type_args
  | {ptyp_desc= Ptyp_var ident; _} ->
      derive_type_variable ident
  | _ ->
      Runtime.get runtime Any

(* post_process is used when the tuple is declared within a variant. In that
   case, the constructor identifier must prefix the tuple. *)
and derive_tuple env tuple from =
  let vars =
    tuple |> List.mapi (fun i _label_declaration -> "arg_" ^ string_of_int i)
  in
  let gens = List.map (derive_core_type env) tuple in
  env.runtime.use_monadic_syntax := true ;
  let base =
    vars
    |> List.map (fun name -> Ast_builder.Default.evar ~loc name)
    |> Ast_builder.Default.pexp_tuple ~loc
    |> fun tuple_expr ->
    match from with
    | `Constructor constructor_name ->
        let expr = match tuple with [] -> None | _ -> Some tuple_expr in
        [%expr
          return
            [%e Ast_builder.Default.pexp_construct ~loc constructor_name expr]]
    | `Core_type ->
        [%expr return [%e tuple_expr]]
  in
  List.fold_left2
    (fun expr var gen ->
      let name = Ast_builder.Default.pvar ~loc var in
      [%expr
        let* [%p name] = [%e gen] in
        [%e expr]] )
    base (List.rev vars) (List.rev gens)

and derive_type_constr env ident args =
  let args =
    List.map
      (fun arg ->
        let env = {env with runtime= {env.runtime with gen= None}} in
        derive_core_type env arg )
      args
  in
  let f =
    match env.runtime.gen with
    | None ->
        Ast_builder.Default.evar ~loc (gen_name ident)
    | Some gen ->
        gen
  in
  if List.mem ident env.recursive_types then
    (* If the recursive type does not take any type parameter, the generators
       code might not be a value which is an issue in case of recursive values. We
       wrap them with a unit argument. *)
    Ast_builder.Default.eapply ~loc f (Ast_builder.Default.eunit ~loc :: args)
  else Ast_builder.Default.eapply ~loc f args

let derive_manifest env = function
  | None ->
      Runtime.get env.runtime Any
  | Some core_type ->
      derive_core_type env core_type

let derive_label_declaration env label_declaration =
  let label_declaration, runtime =
    Attributes.Label_declaration.update label_declaration env.runtime
  in
  let env = {env with runtime} in
  match env.runtime.gen with
  | None ->
      derive_core_type env label_declaration.pld_type
  | Some gen ->
      gen

let derive_record env record from =
  env.runtime.use_monadic_syntax := true ;
  let base =
    record
    |> List.map (fun label_declaration ->
           let location =
             Astlib.Location.
               {txt= Astlib.Longident.parse label_declaration.pld_name.txt; loc}
           in
           let expression =
             Ast_builder.Default.evar ~loc label_declaration.pld_name.txt
           in
           (location, expression) )
    |> fun fields ->
    Ast_builder.Default.pexp_record ~loc fields None
    |> fun record_expr ->
    match from with
    | `Constructor constructor_name ->
        let expr = match record with [] -> None | _ -> Some record_expr in
        [%expr
          return
            [%e Ast_builder.Default.pexp_construct ~loc constructor_name expr]]
    | `Type_declaration ->
        [%expr return [%e record_expr]]
  in
  List.fold_left
    (fun expr label_declaration ->
      let body = derive_label_declaration env label_declaration in
      let name = Ast_builder.Default.pvar ~loc label_declaration.pld_name.txt in
      [%expr
        let* [%p name] = [%e body] in
        [%e expr]] )
    base (List.rev record)

let derive_constructor_declaration env constructor_declaration =
  let constructor_declaration, runtime =
    Attributes.Constructor_declaration.update constructor_declaration
      env.runtime
  in
  let env = {env with runtime} in
  let weight = Option.value ~default:1 env.runtime.weight in
  match env.runtime.gen with
  | Some gen ->
      (weight, gen)
  | None -> (
      let constructor_name =
        {txt= Astlib.Longident.parse constructor_declaration.pcd_name.txt; loc}
      in
      let from = `Constructor constructor_name in
      match constructor_declaration.pcd_args with
      | Pcstr_tuple tuple ->
          (weight, derive_tuple env tuple from)
      | Pcstr_record record ->
          (weight, derive_record env record from) )

let derive_variant env constructors =
  match constructors with
  | [x] ->
      derive_constructor_declaration env x |> snd
  | _ ->
      let constructors = List.rev constructors in
      let names =
        constructors |> List.map (fun {pcd_name; _} -> gen_name pcd_name.txt)
      in
      let weights, constructors_expr =
        constructors
        |> List.map (derive_constructor_declaration env)
        |> List.split
      in
      let base =
        let names = List.map (Ast_builder.Default.evar ~loc) (List.rev names) in
        let weights =
          List.map (Ast_builder.Default.eint ~loc) (List.rev weights)
        in
        let weighted_list =
          List.map2
            (fun weight name ->
              Ast_builder.Default.pexp_tuple ~loc [weight; name] )
            weights names
        in
        let expr = Ast_builder.Default.elist ~loc weighted_list in
        match env.runtime.shrinker with
        | None ->
            [%expr Bam.Std.oneof [%e expr]]
        | Some shrinker ->
            [%expr Bam.Std.oneof ~shrinker:[%e shrinker] [%e expr]]
      in
      List.fold_left2
        (fun expr name body ->
          let name = Ast_builder.Default.pvar ~loc name in
          [%expr
            let [%p name] = [%e body] in
            [%e expr]] )
        base names constructors_expr

let derive_type_declaration env type_declaration =
  env.runtime.use_monadic_syntax := false ;
  let type_declaration, runtime =
    Attributes.Type_declaration.update type_declaration env.runtime
  in
  let env = {env with runtime} in
  match env.runtime.gen with
  | Some gen ->
      gen
  | None ->
      let ct = core_type_of_type_declaration type_declaration in
      let expected_type = [%type: [%t ct] Bam.Gen.t] in
      let body =
        match type_declaration.ptype_kind with
        | Ptype_abstract ->
            derive_manifest env type_declaration.ptype_manifest
        | Ptype_record record ->
            derive_record env record `Type_declaration
        | Ptype_variant variant ->
            derive_variant env variant
        | Ptype_open ->
            Runtime.get env.runtime Any
      in
      let td = name_type_params_in_td type_declaration in
      let params =
        td.ptype_params
        |> List.filter_map (fun (param, _variance) ->
               match param with
               | {ptyp_desc= Ptyp_var ident; _} ->
                   Some ident
               | _ ->
                   None )
        |> List.filter (Fun.flip Helpers.is_type_var_used td)
        |> List.map (fun ident ->
               Ast_builder.Default.pvar ~loc (gen_name ident) )
      in
      (* This can be used for debugging when modifying the deriver to ensure its correctness. *)
      let show_expected_type = false in
      let base =
        if show_expected_type then [%expr ([%e body] : [%t expected_type])]
        else body
      in
      let gen_expr =
        List.fold_left
          (fun expr ident -> [%expr fun [%p ident] -> [%e expr]])
          base (List.rev params)
      in
      if !(env.runtime.use_monadic_syntax) then
        [%expr
          let open Bam.Std.Syntax in
          [%e gen_expr]]
      else gen_expr

let derive_type_declarations rec_flag type_declarations :
    Ast.structure_item list =
  let loc = !Ast_helper.default_loc in
  let is_recursive = really_recursive rec_flag type_declarations in
  let env =
    { recursive_types=
        type_declarations |> List.map (fun td -> td.ptype_name.txt)
    ; runtime= Runtime.default }
  in
  let bindings =
    type_declarations
    |> List.map (fun td ->
           let name = td.ptype_name.txt in
           let generator_name =
             let raw = gen_name name in
             Ast_builder.Default.pvar ~loc raw
           in
           let gen_expr = derive_type_declaration env td in
           let gen_expr =
             if is_recursive = Recursive then
               Ast_builder.Default.pexp_fun ~loc Nolabel None
                 [%pat? ()]
                 gen_expr
             else gen_expr
           in
           { pvb_pat= generator_name
           ; pvb_expr= gen_expr
           ; pvb_attributes= []
           ; pvb_loc= loc } )
  in
  Ast_builder.Default.pstr_value_list ~loc is_recursive bindings

let derive_str_signature_item = function
  | Psig_type (rec_flag, type_declarations) ->
      derive_type_declarations rec_flag type_declarations
  | _ ->
      []

let derive_str_signature signature =
  signature
  |> List.map (fun signature_item ->
         derive_str_signature_item signature_item.psig_desc )
  |> List.concat

let derive_str_module_type = function
  | Pmty_ident _ ->
      []
  | Pmty_signature signature ->
      derive_str_signature signature
  | Pmty_functor _ ->
      []
  | Pmty_with _ ->
      []
  | Pmty_typeof _ ->
      []
  | Pmty_extension _ ->
      []
  | Pmty_alias _ ->
      []

let derive_module_type_declaration ~ctxt:_ module_type_declaration =
  match module_type_declaration.pmtd_type with
  | None ->
      []
  | Some module_type ->
      let loc = !Ast_helper.default_loc in
      let module_binding =
        { pmb_name= {loc; txt= Some module_type_declaration.pmtd_name.txt}
        ; pmb_expr=
            Ast_builder.Default.pmod_structure ~loc
              (derive_str_module_type module_type.pmty_desc)
        ; pmb_attributes= []
        ; pmb_loc= loc }
      in
      [Ast_builder.Default.pstr_module ~loc module_binding]
