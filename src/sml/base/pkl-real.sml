(* 
 *
 * COPYRIGHT (c) 1997, 1998 by Princeton University. 
 * See COPYRIGHT file for details
 *
 * Author: Daniel C. Wang
 *
 *)
(* work in progress*)
functor PklReal(structure R : REAL): PKL =
    struct
	(* fix bug in fromManExp *)
	val zero_r = R.fromInt 0

	structure R =
	    struct
		open R
		fun toManExp x =
		    (if (R.==(x,zero_r)) then {man=zero_r,exp=0}
		     else R.toManExp x)
		fun fromManExp {man,exp} =
		    (if (R.==(man,zero_r)) then zero_r
		     else (R.fromManExp {man=man,exp=exp}))
	    end

	type T = R.real
	val zero_r = R.fromInt 0
	val nibble = 7
	fun realTobits r =
	    let
		val sign = R.signBit r
		val r = R.abs r
		val {man,exp} = R.toManExp r
		fun loop (r,e,acc) =
		    if (R.==(zero_r,R.realMod(r))) then (e,acc)
		    else let
			     val tmp = R.fromManExp{man=r,exp=nibble}
			     val {whole,frac} = R.split(tmp)
			     val bits = R.trunc whole;
			 in
			     loop(frac,e-nibble,bits::acc)
			 end
		val (e,man) = loop(man,exp,[])
	    in
		{sign=sign,exp=exp,mantissa=man}
	    end

	fun bitsToReal  {sign,exp,mantissa} =
	    let
		val acc = zero_r
		fun loop ([],acc) = acc
		  | loop (x::xs,acc) =
		    loop(xs,R.fromManExp{man=R.+(acc,R.fromInt x),
					 exp=(~nibble)})
		val man = loop(mantissa,zero_r)
		val r = R.fromManExp{man=man,exp=exp}
		val r = if sign then R.~(r) else r
	    in
		r
	    end
    end
