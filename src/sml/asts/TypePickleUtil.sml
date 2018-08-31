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
structure TypePickleUtil : TypePickleUtil_SIG =
  struct
    open StdPkl
    
    
    
    fun attrbs_field x = 
        (case (x) of 
              (TypePickle.Id{type_map_key, label}) => {type_map_key=type_map_key, label=label}
            | (TypePickle.Option{type_map_key, label}) => {type_map_key=type_map_key, label=label}
            | (TypePickle.Sequence{type_map_key, label}) => {type_map_key=type_map_key, label=label})
    and attrbs_type_map_value x = 
        (case (x) of 
              (TypePickle.Defined{pkl_tag, name, fields, cnstr_map_keys}) => {pkl_tag=pkl_tag}
            | (TypePickle.Prim{pkl_tag, p}) => {pkl_tag=pkl_tag})
    and read_qid s = 
        let val qualifier =  (read_list StdPrimsUtil.read_identifier s)
          val base =  (StdPrimsUtil.read_identifier s) in {qualifier=qualifier, base=base}
        end
    and read_prim s = 
        (case ((read_tag s)) of 
              1 => TypePickle.String
            | 2 => TypePickle.Identifier
            | 3 => TypePickle.Int
            | _ => (die ()))
    and read_field s = 
        (case ((read_tag s)) of 
              1 => let val type_map_key =  (StdPrimsUtil.read_int s)
                val label =  (StdPrimsUtil.read_identifier s) in TypePickle.Id{type_map_key=type_map_key, label=label}
              end
            | 2 => let val type_map_key =  (StdPrimsUtil.read_int s)
                val label =  (StdPrimsUtil.read_identifier s) in TypePickle.Option{type_map_key=type_map_key, label=label}
              end
            | 3 => let val type_map_key =  (StdPrimsUtil.read_int s)
                val label =  (StdPrimsUtil.read_identifier s) in TypePickle.Sequence{type_map_key=type_map_key, label=label}
              end
            | _ => (die ()))
    and read_cnstr_map_value s = 
        let val pkl_tag =  (StdPrimsUtil.read_int s)
          val type_map_key =  (StdPrimsUtil.read_int s)
          val name =  (read_qid s)
          val fields =  (read_list read_field s) in {pkl_tag=pkl_tag, type_map_key=type_map_key, name=name, fields=fields}
        end
    and read_cnstr_map_entry s = 
        let val key =  (StdPrimsUtil.read_int s)
          val v =  (read_cnstr_map_value s) in {key=key, v=v}
        end
    and read_cnstr_map s = 
        let val max_key =  (StdPrimsUtil.read_int s)
          val entries =  (read_list read_cnstr_map_entry s) in {max_key=max_key, entries=entries}
        end
    and read_type_map_value s = 
        (case ((read_tag s)) of 
              1 => let val pkl_tag =  (StdPrimsUtil.read_int s)
                val name =  (read_qid s)
                val fields =  (read_list read_field s)
                val cnstr_map_keys =  (read_list StdPrimsUtil.read_int s) in TypePickle.Defined{pkl_tag=pkl_tag, name=name, fields=fields, cnstr_map_keys=cnstr_map_keys}
              end
            | 2 => let val pkl_tag =  (StdPrimsUtil.read_int s)
                val p =  (read_prim s) in TypePickle.Prim{pkl_tag=pkl_tag, p=p}
              end
            | _ => (die ()))
    and read_type_map_entry s = 
        let val key =  (StdPrimsUtil.read_int s)
          val v =  (read_type_map_value s) in {key=key, v=v}
        end
    and read_type_map s = 
        let val max_key =  (StdPrimsUtil.read_int s)
          val entries =  (read_list read_type_map_entry s) in {max_key=max_key, entries=entries}
        end
    and read_module_map_value s = 
        let val name =  (read_qid s)
          val file =  (StdPrimsUtil.read_string s) in {name=name, file=file}
        end
    and read_module_map_entry s = 
        let val key =  (StdPrimsUtil.read_int s)
          val v =  (read_module_map_value s) in {key=key, v=v}
        end
    and read_module_map s = 
        let val max_key =  (StdPrimsUtil.read_int s)
          val entries =  (read_list read_module_map_entry s) in {max_key=max_key, entries=entries}
        end
    and read_type_env s = 
        let val version =  (StdPrimsUtil.read_int s)
          val magic =  (StdPrimsUtil.read_int s)
          val mmap =  (read_module_map s)
          val tmap =  (read_type_map s)
          val cmap =  (read_cnstr_map s) in {version=version, magic=magic, mmap=mmap, tmap=tmap, cmap=cmap}
        end
    and read_type_map_entry_list s = 
        (read_list read_type_map_entry s)
    and read_module_map_entry_list s = 
        (read_list read_module_map_entry s)
    and read_field_list s = 
        (read_list read_field s)
    and read_cnstr_map_entry_list s = 
        (read_list read_cnstr_map_entry s)
    and write_qid x s = 
        (case (x) of 
              {qualifier, base} : TypePickle.qid => ((write_list StdPrimsUtil.write_identifier qualifier s); (StdPrimsUtil.write_identifier base s)))
    and write_prim x s = 
        (case (x) of 
              TypePickle.String => ((write_tag 1 s))
            | TypePickle.Identifier => ((write_tag 2 s))
            | TypePickle.Int => ((write_tag 3 s)))
    and write_field x s = 
        (case (x) of 
              (TypePickle.Id{type_map_key, label}) => ((write_tag 1 s); (StdPrimsUtil.write_int type_map_key s); (StdPrimsUtil.write_identifier label s))
            | (TypePickle.Option{type_map_key, label}) => ((write_tag 2 s); (StdPrimsUtil.write_int type_map_key s); (StdPrimsUtil.write_identifier label s))
            | (TypePickle.Sequence{type_map_key, label}) => ((write_tag 3 s); (StdPrimsUtil.write_int type_map_key s); (StdPrimsUtil.write_identifier label s)))
    and write_cnstr_map_value x s = 
        (case (x) of 
              {pkl_tag, type_map_key, name, fields} : TypePickle.cnstr_map_value => ((StdPrimsUtil.write_int pkl_tag s); (StdPrimsUtil.write_int type_map_key s); (write_qid name s); (write_list write_field fields s)))
    and write_cnstr_map_entry x s = 
        (case (x) of 
              {key, v} : TypePickle.cnstr_map_entry => ((StdPrimsUtil.write_int key s); (write_cnstr_map_value v s)))
    and write_cnstr_map x s = 
        (case (x) of 
              {max_key, entries} : TypePickle.cnstr_map => ((StdPrimsUtil.write_int max_key s); (write_list write_cnstr_map_entry entries s)))
    and write_type_map_value x s = 
        (case (x) of 
              (TypePickle.Defined{pkl_tag, name, fields, cnstr_map_keys}) => ((write_tag 1 s); (StdPrimsUtil.write_int pkl_tag s); (write_qid name s); (write_list write_field fields s); (write_list StdPrimsUtil.write_int cnstr_map_keys s))
            | (TypePickle.Prim{pkl_tag, p}) => ((write_tag 2 s); (StdPrimsUtil.write_int pkl_tag s); (write_prim p s)))
    and write_type_map_entry x s = 
        (case (x) of 
              {key, v} : TypePickle.type_map_entry => ((StdPrimsUtil.write_int key s); (write_type_map_value v s)))
    and write_type_map x s = 
        (case (x) of 
              {max_key, entries} : TypePickle.type_map => ((StdPrimsUtil.write_int max_key s); (write_list write_type_map_entry entries s)))
    and write_module_map_value x s = 
        (case (x) of 
              {name, file} : TypePickle.module_map_value => ((write_qid name s); (StdPrimsUtil.write_string file s)))
    and write_module_map_entry x s = 
        (case (x) of 
              {key, v} : TypePickle.module_map_entry => ((StdPrimsUtil.write_int key s); (write_module_map_value v s)))
    and write_module_map x s = 
        (case (x) of 
              {max_key, entries} : TypePickle.module_map => ((StdPrimsUtil.write_int max_key s); (write_list write_module_map_entry entries s)))
    and write_type_env x s = 
        (case (x) of 
              {version, magic, mmap, tmap, cmap} : TypePickle.type_env => ((StdPrimsUtil.write_int version s); (StdPrimsUtil.write_int magic s); (write_module_map mmap s); (write_type_map tmap s); (write_cnstr_map cmap s)))
    and write_type_map_entry_list x s = 
        (write_list write_type_map_entry x s)
    and write_module_map_entry_list x s = 
        (write_list write_module_map_entry x s)
    and write_field_list x s = 
        (write_list write_field x s)
    and write_cnstr_map_entry_list x s = 
        (write_list write_cnstr_map_entry x s)
    
    
  
  end
