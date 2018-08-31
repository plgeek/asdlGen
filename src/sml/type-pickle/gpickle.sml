(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


(* should be using the "MODULE" interface rather than this ad hoc 
 approach
*)

signature GENERIC_PICKLER =
    sig	
	structure T: TYPE_PICKLE
	structure V: ASDL_VALUE
	    
	val read_asdl_value  :
	    T.type_env -> SourceId.sid -> T.instream -> V.asdl_value

	val type_labels : T.type_env -> (T.qid option * T.qid option)
	    -> 'a list ->  ('a * Identifier.identifier) list

(*	val write_asdl_value :
	    T.type_env -> Identifier.identifier ->
	    V.asdl_value -> T.outstream
*)	    
(*	val pp_asdl_value:  V.asdl_value -> TextIO.outstream -> unit*)
    end

functor GenericPickler(structure T:TYPE_PICKLE
		       structure V:ASDL_VALUE ):GENERIC_PICKLER =

    struct
	structure T = T
	structure V = struct open V StdPrimsUtil  end

	fun mk_map name add {max_key,entries} =
	    let
		val array = Array.array(max_key+1,NONE)
		fun add' x =
		    let
			val {key,v} = add x
		    in
			Array.update(array,key,SOME v)
		    end
		val _ = List.app add' entries
		val vec = Array.extract(array,0,NONE)
		fun sub i =
		    case (Vector.sub(vec,i)) of
			NONE => raise Error.error
			    ["lookup failed in map ", name]
		      | SOME v => v
	    in
		sub
	    end

	fun mk_map' name add x dflt =
	    let
		val sub = mk_map name add x
		fun sub' x =
		    (sub x handle _ => dflt)
	    in
		sub'
	    end

  	fun mk_qid s = {base=Identifier.fromString s,qualifier=[]}:T.qid


	val int_qid = mk_qid "int"
	val str_qid = mk_qid "string"
	val id_qid =  mk_qid "identifier"

	fun type_name x =
	    case (x) of
		(T.Defined{name,...}) => name
	      | (T.Prim{p=T.Int,...}) => int_qid
	      | (T.Prim{p=T.String,...}) => str_qid
	      | (T.Prim{p=T.Identifier,...}) => id_qid

	fun type_fields x =
	    case (x) of
		(T.Defined{fields,...}) => fields
	      | _ => []

	fun qid2Id {base,qualifier} =
	    SourceId.fromPath{base=Identifier.toString base,
			qualifier=List.map Identifier.toString qualifier}


	fun type_labels ({version,magic,mmap,tmap,cmap}:T.type_env) =
	    let
		fun insert ({key,v},tmapi) =
		    Env.insert(tmapi,qid2Id (type_name v),type_fields v)

		fun insert_con ({key,v}:T.cnstr_map_entry,cmapi) =
		    Env.insert(cmapi,qid2Id (#name v),#fields v)
		    
		val tmapi = List.foldl insert Env.empty (#entries tmap)
		val cmapi = List.foldl insert_con Env.empty (#entries   cmap)
		fun find_con_fields id =
		    case (Env.find(cmapi,qid2Id id)) of
			NONE => []
		      | (SOME x) => x
		fun find_typ_fields id =
		    case (Env.find(tmapi,qid2Id id)) of
			NONE => []
		      | (SOME x) => x

		fun get_ids (NONE,NONE) =  []
		  | get_ids (SOME t,NONE) =
		    (find_typ_fields t)
		  | get_ids (NONE,SOME c) =  (find_con_fields c)
		  | get_ids (SOME t,SOME c) =
		    (find_typ_fields t)@(find_con_fields c)
		fun get_label (T.Id{label,...}) = label
		  | get_label (T.Option{label,...}) = label
		  | get_label (T.Sequence{label,...}) = label
		fun do_it x y =
		    ListPair.zip (y,(List.map get_label (get_ids x)))
	    in
		do_it
	    end
	
	fun read_asdl_value ({version,magic,mmap,tmap,cmap}:T.type_env) =
	    let
		fun insert ({key,v},tmapi) =
		    Env.insert(tmapi,qid2Id (type_name v),key)

		val tmapi = List.foldl insert Env.empty (#entries tmap)
		fun add x = x
		val mmap = mk_map "module"      add mmap
		val tmap = mk_map "types"       add tmap
		val cmap = mk_map "constructor" add cmap

		fun prim_reader T.Int  = (V.IntValue o V.read_int)
		  | prim_reader T.Identifier =
		    (V.IdentifierValue o V.read_identifier)
		  | prim_reader T.String = (V.StringValue o V.read_string)

		fun type_reader (x as (T.Prim{pkl_tag,p})) =
		    let
			val name = type_name x
			val reader = prim_reader p
			fun mk s = V.PrimValue{typename=name,v=reader s}
		    in
			mk
		    end
		  | type_reader (T.Defined{pkl_tag,name,fields,
					   cnstr_map_keys=[]}) =
		    let
			val reader = fields_reader fields
			fun next s =
			    case (reader s) of
				(v::vs) =>
				    V.ProductValue {typename=name,v=v,vs=vs}
			      | _ => raise Error.internal
		    in
			next
		    end
		  | type_reader (T.Defined{pkl_tag,name,fields,
					   cnstr_map_keys}) =
		    cnstrs_reader name fields cnstr_map_keys
		and fields_reader x =
		    let
			val ty =
			    tmap o (fn {type_map_key,label} => type_map_key)
			fun field_reader (T.Id x) =  type_reader (ty x)
			  | field_reader (T.Sequence x) =
			    let
				val name = type_name (ty x)
				val reader = T.read_list (type_reader (ty x))
				fun mk s =
				    V.SequenceValue{typename=name,vs=reader s}
			    in
				mk
			    end
			  | field_reader (T.Option x) =
			    let
				val name = type_name (ty x)
				val reader = T.read_option (type_reader (ty x))
				fun mk s =
				    case (reader s) of
					NONE => V.NoneValue{typename=name}
				      | (SOME v)=> V.SomeValue
					    {typename=name,v=v}
			    in
				mk
			    end
			val readers = List.map field_reader x
			fun mk s =
			    List.map (fn rd => rd s) readers
		    in
			mk
		    end
		and cnstrs_reader name attrbs cnstrs_map_keys =
		    let
			fun add (c) =
			    let
				val {pkl_tag,name=con,fields,...} = cmap c
				fun mk s =
				    let
					val attrbs_reader =
					    fields_reader attrbs
					val vs_reader =
					    fields_reader fields

					val attrbs = attrbs_reader s
					val vs = vs_reader s
				    in
					(V.SumValue
					{typename=name,con=con,
					 attrbs=attrbs,vs=vs})
				    end
			    in
				{key=pkl_tag,v=mk}
			    end
			val max = (List.length cnstrs_map_keys)
			val {qualifier=_,base=tname} = name
			val tname = Identifier.toString tname
			fun error s = V.NoneValue{typename=name}
			val crmap = mk_map' ("Con Reader:;"^tname) add
			    {max_key=max,entries=cnstrs_map_keys} error
			fun mk s =
			    let
				val tag = (V.read_tag s)
				val rd = (crmap tag)
			    in
				rd s
			    end
		    in
			mk
		    end
		fun do_it i =
		    case (Env.find(tmapi,i)) of
			NONE => raise Error.error ["No such type"]
		      | (SOME i) => type_reader (tmap i)
			    
	    in
		do_it
	    end
    end
    
