(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)


structure ModuleId :> MODULE_ID =
    struct
	datatype mid =
	    M of {base:Atom.atom,qualifier:Atom.atom list}
	type path = {base:string,qualifier:string list}

	fun mapP f {base,qualifier} =
	    {base=f base,qualifier=List.map f qualifier}
	    
	fun compare (M{base=x,qualifier=xs},
		     M{base=y,qualifier=ys}) =
	    let
		fun compl ([],[]) = EQUAL
		  | compl (_,[]) = GREATER
		  | compl ([],_) = LESS
		  | compl (x::xs,y::ys) =
		    (case (Atom.compare(x,y)) of
			 EQUAL => compl(xs,ys)
		       | x => x)
	    in
		compl (x::xs,y::ys)
	    end
	val fromPath  =   M o (mapP Atom.atom)
	fun toPath (M x) = mapP Atom.toString x
	
	fun subst f x =
	    (case (f (toPath x)) of
		 (SOME x) => (fromPath x)
	       | NONE => x)
	fun prefixB s {base,qualifier} =
	    SOME {base=s^base,qualifier=qualifier}

	fun suffixB s {base,qualifier} =
	    SOME {base=base^s,qualifier=qualifier}

	fun prefixBase x = subst (prefixB x)
	fun suffixBase x = subst (suffixB x)

	fun getBase (M{base,...}) = Atom.toString base
	fun getQualifier (M{qualifier,...}) =
	    List.map Atom.toString qualifier
	
	fun toString' sp x =
	    let
		val {base,qualifier} = toPath x
		val qualifier =
		    if qualifier = [""] then []
		    else qualifier
	    in
		ListFormat.fmt
		{init="",sep=sp,final="",fmt=(fn x => x)}
		(qualifier @[base])
	    end
	val toString = toString' "."
	    
	fun fromString s =  fromPath {base=s,qualifier=[]}
	fun eq x = (compare x) = EQUAL
	fun eqQualifier (M{base=_,qualifier=xs},
			 M{base=_,qualifier=ys}) =
	    let
		fun compl ([],[]) = EQUAL
		  | compl (_,[]) = GREATER
		  | compl ([],_) = LESS
		  | compl (x::xs,y::ys) =
		    (case (Atom.compare(x,y)) of
			 EQUAL => compl(xs,ys)
		       | x => x)
	    in
		compl (xs,ys) = EQUAL
	    end
    end


	    
