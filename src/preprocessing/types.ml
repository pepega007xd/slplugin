open Cil
open Cil_types
open Astral
open Constants
open Common

type field_type = Next | Prev | Top | Other of string | Data
type struct_type = Sll | Dll | Nl | Struct

let is_struct_ptr (typ : typ) : bool =
  match unrollTypeDeep typ with TPtr (TComp (_, _), _) -> true | _ -> false

let is_struct_ptr_var (var : varinfo) = is_struct_ptr var.vtype

let get_struct_pointer_fields (structure : compinfo) : fieldinfo list =
  structure.cfields |> Option.get
  |> List.filter (fun field -> is_struct_ptr field.ftype)

let rec get_self_and_sll_fields (structure : compinfo) :
    fieldinfo list * fieldinfo list =
  let self_pointers, other_pointers =
    structure |> get_struct_pointer_fields
    |> List.partition (fun field ->
           match field.ftype with
           | TPtr (TComp (target_struct, _), _) ->
               target_struct.ckey = structure.ckey
           | _ -> false)
  in
  let sll_pointers =
    List.filter
      (fun field ->
        match field.ftype with
        | TPtr (TComp (structure, _), _) -> get_struct_type structure = Sll
        | _ -> false)
      other_pointers
  in
  (self_pointers, sll_pointers)

and get_struct_type (structure : compinfo) : struct_type =
  let self_pointers, sll_pointers = get_self_and_sll_fields structure in

  match (self_pointers, sll_pointers) with
  | [ _ ], [] -> Sll
  | [ _ ], [ _ ] -> Nl
  | [ _; _ ], [] -> Dll
  | _ -> Struct

let get_field_type (field : fieldinfo) : field_type =
  let self_pointers, sll_pointers = get_self_and_sll_fields field.fcomp in

  match (self_pointers, sll_pointers) with
  (* SLL *)
  | [ next ], [] when field.forder = next.forder -> Next
  (* DLL *)
  | [ next; _ ], [] when field.forder = next.forder -> Next
  | [ _; prev ], [] when field.forder = prev.forder -> Prev
  (* NL *)
  | [ top ], [ _ ] when field.forder = top.forder -> Top
  | [ _ ], [ next ] when field.forder = next.forder -> Next
  | _ -> if is_struct_ptr field.ftype then Other field.fname else Data

let struct_info : (compinfo, Sort.t * MemoryModel.StructDef.t) Hashtbl.t =
  Hashtbl.create 113

let get_structure (typ : typ) : compinfo =
  match unrollTypeDeep typ with
  | TPtr (TComp (compinfo, _), _) -> compinfo
  | _ -> fail "unsupported type: %a" Printer.pp_typ typ

let rec get_type_info (structure : compinfo) : Sort.t * MemoryModel.StructDef.t
    =
  Hashtbl.find_opt struct_info structure |> function
  | Some result -> result
  | None -> (
      let dummy_struct_def = MemoryModel.StructDef.mk "dummy_struct_def" [] in
      match get_struct_type structure with
      | Sll -> (SL_builtins.loc_ls, dummy_struct_def)
      | Dll -> (SL_builtins.loc_dls, dummy_struct_def)
      | Nl -> (SL_builtins.loc_nls, dummy_struct_def)
      | Struct ->
          let name = structure.cname in
          let sort = Sort.mk_loc name in
          let fields =
            structure |> get_struct_pointer_fields
            |> List.map (fun field ->
                   let sort =
                     field.ftype |> get_structure |> get_type_info |> fst
                   in
                   MemoryModel0.Field.mk field.fname sort)
          in
          let struct_def = MemoryModel.StructDef.mk name fields in
          let result = (sort, struct_def) in
          Hashtbl.add struct_info structure result;
          result)

let get_struct_def (sort : Sort.t) : MemoryModel.StructDef.t =
  Hashtbl.to_seq_values struct_info
  |> Seq.find (fun (s, _) -> sort = s)
  |> Option.get |> snd

let varinfo_to_var (varinfo : Cil_types.varinfo) : SL.Variable.t =
  match varinfo.vname with
  | name when name = null_var_name -> SL.Variable.nil
  | name when name = const_var_name -> fail "_const in varinfo_to_var"
  | _ when not @@ is_struct_ptr varinfo.vtype ->
      fail "invalid type in varinfo_to_var: %a" Printer.pp_varinfo varinfo
  | _ ->
      let sort = varinfo.vtype |> get_structure |> get_type_info |> fst in
      SL.Variable.mk varinfo.vname sort

let is_list_struct (typ : typ) =
  match typ |> get_structure |> get_struct_type with
  | Sll | Dll | Nl -> true
  | _ -> false

let process_types =
  object
    inherit Visitor.frama_c_inplace

    method! vtype (typ : typ) =
      match unrollTypeDeep typ with
      | TComp (structure, _) ->
          ignore @@ get_type_info structure;
          SkipChildren
      | _ -> DoChildren
  end

let process_types (file : file) =
  Visitor.visitFramacFileFunctions process_types file;

  let heap_sort =
    Hashtbl.to_seq_values struct_info |> List.of_seq |> HeapSort.of_list
  in
  Common.solver := Solver.set_heap_sort heap_sort !Common.solver
