
signature PROPERTIES =
    sig
	type props_desc
	type props
	type prop_value
	type path = {base:string,qualifier:string list}
	type init = (string * prop_value)
	type 'a prop_decl = props_desc ->  
	    {name:string,default:'a} ->  ((props -> 'a) * ('a -> init))

	val decl_bool       : bool       prop_decl
	val decl_string     : string     prop_decl
	val decl_int        : int        prop_decl
	val decl_path       : path       prop_decl

	val decl_bool_opt   : bool    option prop_decl
	val decl_string_opt : string  option prop_decl
	val decl_int_opt    : int     option prop_decl
	val decl_props_opt  : props   option prop_decl
	val decl_path_opt   : path    option prop_decl

	val make_desc : string -> props_desc
	    
	val parse_inits : props_desc -> (string * string) list -> init list
	val from_inits  : props_desc -> init list -> props

    end

structure Properties :> PROPERTIES =
    struct
	structure Map =
	    ListMapFn(struct
			  type ord_key = String.string
			  val compare = String.compare
		      end)

	type key = int ref
	type path = {base:string,qualifier:string list}
	datatype prop_value =
	    B of bool   option
	  | I of int    option 
	  | S of string option
	  | Q of path option 
	  | P of props  option
	withtype props = {id:string ref,v:prop_value Vector.vector}
	and props_desc = {id:string ref,
			  map:{key:key,default:prop_value} Map.map ref}
	type init = (string * prop_value)
	type 'a prop_decl = props_desc ->  {name:string,default:'a} ->  ((props -> 'a) * ('a -> init))

	fun make_desc n = {id=ref n,map=ref Map.empty}
	fun add_prop p {name,default} =
	    case Map.find(!p,name) of
		SOME _ => raise Error.error ["Duplicate property definitions"]
	      | NONE =>
		    let
			val key = (ref ~1)
		    in
			p := Map.insert(!p,name,{key=key,default=default});
			key
		    end

	fun get_val i p = Vector.sub(p,!i)

	fun toB (B x) = x
	  | toB _ = raise Error.error ["Bad prop expected bool"]
	fun toS (S x) = x
	  | toS _ = raise Error.error ["Bad prop expected string"]
	fun toI (I x) = x
	  | toI _ = raise Error.error ["Bad prop expected int"]
	fun toQ (Q x) = x
	  | toQ _ = raise Error.error ["Bad prop expected path"]
	fun toP (P x) = x
	  | toP _ = raise Error.error ["Bad prop expected prop"]


	fun mkdecl c d {id,map} {name,default} =
	    let
		val k = add_prop map {name=name,default=c default}
		fun get {id=id',v} =
		    if id = id' then d(Vector.sub(v,!k))
		    else raise Error.error
			["Wrong prop type  got:",!id'," expected:",!id]
		fun init x = (name,c x)
	    in
		(get,init)
	    end

	val decl_bool = mkdecl   (B o SOME) (valOf o toB)
	val decl_string = mkdecl (S o SOME) (valOf o toS)
	val decl_int = mkdecl    (I o SOME) (valOf o toI)
	val decl_path = mkdecl   (Q o SOME) (valOf o toQ)
	val decl_props = mkdecl  (P o SOME) (valOf o toP)

	val decl_bool_opt = mkdecl    B  toB
	val decl_string_opt = mkdecl  S  toS
	val decl_int_opt = mkdecl     I  toI
	val decl_path_opt = mkdecl    Q  toQ
	val decl_props_opt = mkdecl   P  toP

	fun fromOpt p (NONE,x) = 
	    (Error.warn
	     ["Error while parsing property ",p," using default"];x)
	  | fromOpt p (SOME y,_) = (SOME y)

	(* hack for now *)

	fun parse_path x =
	    let
		fun kill_spc x =
		    if (Char.isSpace x) then ""
		    else Char.toString x
		val x = String.translate kill_spc x
		val x = String.tokens (fn x => x = #".") x
		val len = List.length x
		val (qualifier,base) =
		    (List.take (x,len-1),List.drop (x,len - 1))
	    in {base=List.hd base,qualifier=qualifier}
	    end
	fun parse_prop p (B x,s) = B(fromOpt p (Bool.fromString s,x))
	  | parse_prop p (I x,s) = I(fromOpt p (Int.fromString s,x))
	  | parse_prop p (S _,s) = (S (SOME s))
	  | parse_prop p (Q _,s) = (Q (SOME (parse_path s)))
	  | parse_prop p (P _,s) = raise Error.unimplemented


	fun parse_inits {id,map=ref p} args =
	    let
		fun parse((k,v)) =
		    case (Map.find(p,k)) of
			NONE => (Error.warn ["Unknown prop ",k];NONE)
		      | (SOME {key,default}) =>  SOME(k,parse_prop k (default,v))
	    in	List.mapPartial parse args
	    end

	fun from_inits {id,map} args =
	    let
		fun insert((k,v),p) =
		    case (Map.find(p,k)) of
			NONE => (Error.warn ["Unknown prop ",k];p)
		      | (SOME {key,default}) =>
			    Map.insert(p,k,{key=key,default=v})
		val props = Vector.fromList
		    (Map.listItems (List.foldl insert (!map) args))
		fun fix_key (i,{key,default}) = (key:=i;default)
	    in {id=id,v=Vector.mapi fix_key (props,0,NONE)}
	    end

end
