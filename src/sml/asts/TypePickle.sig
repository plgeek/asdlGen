(* Machine generated. Edit at your own risk 
   Reproduce with the following
  --base_signature=STD_PKL
  --base_structure=StdPkl
  --line_width=74
  --no_action=false
  --output_directory=asts
  --view=SML
  --xml_pickler=false
  *)
signature TypePickle_SIG = 
  sig
    include STD_PKL
    
    datatype prim = String
        | Identifier
        | Int
    and field = Id of {type_map_key:StdPrims.int, label:StdPrims.identifier}
        | Option of {type_map_key:StdPrims.int, label:StdPrims.identifier}
        | Sequence of {type_map_key:StdPrims.int, label:StdPrims.identifier}
    and type_map_value = Defined of {pkl_tag:StdPrims.int, name:qid, fields:field list, cnstr_map_keys:StdPrims.int list}
        | Prim of {pkl_tag:StdPrims.int, p:prim}
    withtype qid = {qualifier:StdPrims.identifier list, base:StdPrims.identifier}
    and cnstr_map_value = {pkl_tag:StdPrims.int, type_map_key:StdPrims.int, name:qid, fields:field list}
    and cnstr_map_entry = {key:StdPrims.int, v:cnstr_map_value}
    and cnstr_map = {max_key:StdPrims.int, entries:cnstr_map_entry list}
    and type_map_entry = {key:StdPrims.int, v:type_map_value}
    and type_map = {max_key:StdPrims.int, entries:type_map_entry list}
    and module_map_value = {name:qid, file:StdPrims.string}
    and module_map_entry = {key:StdPrims.int, v:module_map_value}
    and module_map = {max_key:StdPrims.int, entries:module_map_entry list}
    and type_env = {version:StdPrims.int, magic:StdPrims.int, mmap:module_map, tmap:type_map, cmap:cnstr_map}
    
    
    
  end
