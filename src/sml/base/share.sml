(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(* work in progress*)
signature SHARE =
  sig

    datatype 'a share = vDEF of string * 'a | vUSE of string
(*
    type name = string
    datatype 'a value pklv = DEF of name * 'value | USE of name
    type ('share_ref,'value) env
    type 'a sref
    type 'a simple_env = ('a sref,'a) env

    val ref2value: ('share_ref,'value) env -> 'share_ref -> 'value
    val ref2pklv : ('share_ref,'value) env -> 'share_ref -> 'value pklv
    val pklv2ref : ('share_ref,'value) env -> 'value pklv -> 'share_ref

   val mk_env :  {newRef: 'value -> 'share_ref,
		 refName: 'share_ref -> name,
 		refValue: 'share_ref -> 'value,
                 bindRef: 'share_ref -> unit,
	        boundRef: 'share_ref -> bool,
                 findRef: name -> 'share_ref} ->  ('share_ref,'value) env

   val mk_simple_env : unit ->  {env: 'a simple_env,
 			       clear: unit -> unit,
			      newRef: 'a -> 'a sref,
			      refRef: 'a sref -> 'a ref,
			     refName: 'a sref -> name}
*)
end

structure Share :> SHARE =
struct
  datatype 'a share = vDEF of string * 'a | vUSE of string

  (*
type name = string
datatype 'value pklv = DEF of name * 'value | USE of name
  datatype ('share_ref,'value) env =
      E of {newRef: 'value -> 'share_ref,
           refName: 'share_ref -> name,
          refValue: 'share_ref -> 'value,
	   bindRef: 'share_ref -> unit,
	  boundRef: 'share_ref -> bool,
	   findRef: name -> 'share_ref}

    type 'a sref = (int * 'a ref)
    type 'a simple_env = ('a sref,'a) env

    val mk_env = E
    fun ref2value (E{refValue,...}) = refValue
    fun ref2pklv (E{boundRef,refValue,refName,bindRef,...}) r =
      (if (boundRef r) then (USE (refName r))
       else (bindRef r;DEF (refName r,refValue r)))

    fun pklv2ref (E{findRef,...}) (USE n) = findRef n
      | pklv2ref (E{newRef,bindRef,...}) (DEF (n,v)) =
	 let val r = newRef v
	 in bindRef r; r
	 end
    structure Key =
	 struct
	   type hash_key = int
	   val hashVal = Word.fromInt 
	   fun sameKey (x,y:int) = x = y
	 end
    structure Ht = HashTableFn(Key) 
    fun mk_simple_env () =
      let
	val ht = Ht.mkTable (128,Fail "unbound")
	fun newRef v =
	  let
	    val i = Ht.numItems ht
	    val r = (ref v)
	  in
	    Ht.insert ht (i,(false,r));
	    (i,r)
	  end
	fun refName (i,_) = Int.toString i
	fun refValue (_,r) = !r
	fun bindRef (i,r) = Ht.insert ht (i,(true,r))
	fun boundRef (i,r) = #1 (Ht.lookup ht i)
	fun findRef s =
	  let val i = (Option.valOf (Int.fromString s))
	  in (i,#2 (Ht.lookup ht i))  end
      in
	 {env=mk_env {newRef=newRef,
		  refName=refName,
		  refValue=refValue,
		  bindRef=bindRef,
		  boundRef=boundRef,
		      findRef=findRef},
	  newRef=newRef,
	  refRef=(fn (_,x) => x),
	  refName=refName,
	  clear=(fn () => Ht.clear ht)}
      end
    *)
end
