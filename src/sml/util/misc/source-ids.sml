(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
structure SourceIds :> SOURCE_IDS =
  struct
    structure HT = HashTable
      
    type namespace = (word * string)
    type path = {ns:namespace,base:string,qualifier:string list}
    type sid = {id:word,unique:bool,p:path}
    type ord_key = sid
    val nextns = ref 0w0
    val nextid = ref 0w0
    fun mkNameSpace s =
      let val ns = !nextns
      in nextns := ns+ 0w1;(ns,s)
      end
    val bogusNS = mkNameSpace "bogusNS"
     
    fun hash_path {ns=(w,s),base,qualifier} =
      List.foldl (fn (s,w) => w + HashString.hashString s)
      ((HashString.hashString base)+w)  qualifier

    fun eq_path ({ns=(wx,_),base=bx,qualifier=qx},
		 {ns=(wy,_),base=by,qualifier=qy}) =
      wx = wy andalso bx = by andalso qx = qy

    val ht : (path,sid) HT.hash_table =
      HT.mkTable (hash_path,eq_path) (128,Fail "Source Id")

    fun newId p =
      let
	val id = !nextid
	val sid = {id=id,unique=false,p=p}
      in nextid := (id+0w1);
	HT.insert ht (p,sid);
	sid
      end
    
    fun uniqueId p =
      let
	val id = !nextid
      in nextid := (id+0w1);
	{id=id,p=p,unique=true}
      end

    fun fromPath p =
      case (HT.find ht p) of
	NONE => (newId p)
      | SOME s => s
    fun toPath ({id,p,...}:sid) = p

    fun compare ({id=x,...}:sid,{id=y,...}:sid) = Word.compare(x,y)

  end


