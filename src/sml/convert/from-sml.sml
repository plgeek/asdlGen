structure FromSML :>
  sig
    val fromSML : Compiler.Ast.dec -> Asdl.decl list
    val doit : string -> unit
  end =
  struct
    structure F = Compiler.Ast
    structure T = Asdl
    val extern_f : Asdl.field = 
      {typ={qualifier=NONE,base=Identifier.fromString "(int)--???"},
       label_opt=NONE,tycon_opt=NONE}

    val sym2id = Identifier.fromString o Compiler.Symbol.name 
    fun fromDec (F.SigDec sigbs,xs) = List.foldr fromSigb xs sigbs 
      | fromDec (F.MarkDec (s,_),xs) = fromDec (s,xs)
      | fromDec (F.SeqDec s,xs) = List.foldr fromDec xs s
      | fromDec (_,xs) = xs
    and fromSigb (F.MarkSigb(sigb,_),xs) = fromSigb(sigb,xs)
      | fromSigb (F.Sigb{def,name},xs) = fromSigExp (name,def,xs)
    and fromSigExp (name,F.BaseSig specs,xs) =
      let
	val decls =  List.foldr fromSpec [] specs
	val decl = T.Module{name=sym2id name,imports=[], decls=decls}
      in
	decl::xs
      end
      | fromSigExp (name,F.MarkSig(e,_),xs) = fromSigExp(name,e,xs)
      | fromSigExp (name,_,xs) = xs
    and fromSpec (F.TycSpec(tys,_),xs) = List.foldr fromTy xs tys 
      | fromSpec (F.DataSpec{datatycs,withtycs},xs) =
      List.foldr fromTB (List.foldr fromDB xs datatycs) withtycs 
      | fromSpec (F.MarkSpec(spec,_),xs) = fromSpec(spec,xs)
      | fromSpec (_,xs) = xs
    and fromDB (F.Db{rhs=F.Constrs (c::cs),tyc,tyvars},xs) =
      let
	val name = sym2id tyc
	fun mkCon (s,SOME ty) =
	  let val (f,fs) = ty2fields ty
	  in {name=sym2id s,fs=f::fs}
	  end
	  | mkCon (s,NONE) = {name=sym2id s,fs=[]}
      in
	(T.SumType{name=name,attribs=[],c=mkCon c,cs=List.map mkCon cs})::xs
      end
      | fromDB (F.MarkDb (db,r),xs) = fromDB(db,xs)
      | fromDB (_,xs) = xs
    and fromTB (F.Tb{def,tyc,tyvars},xs) =
      let
	val name = sym2id tyc
	val (f,fs) = ty2fields def
      in (T.ProductType{name=name,f=f,fs=fs})::xs
      end
      | fromTB (F.MarkTb(tb,_),xs) = fromTB(tb,xs)
    and fromTy ((s,tyvars,NONE),xs) =
      (T.ProductType{name=sym2id s,f=extern_f,fs=[]})::xs
      | fromTy ((s,tyvars,SOME ty),xs) =
      let val (f,fs) = ty2fields ty
      in (T.ProductType{name=sym2id s,f=f,fs=fs})::xs
      end
    and ty2fields ty =
      let
	fun mktyfd qo (F.ConTy (s,[])) =
	  let
	    val s = List.map sym2id s
	    val l = (List.length s)
	    val (q,b) = (List.take (s,l-1),List.last s)
	    val path = case q of
	      [] => {qualifier=NONE,base=b}
	    | [q] => {qualifier=SOME q,base=b}
	  in
	   (fn l => SOME {typ=path,label_opt=l,tycon_opt=qo})
	  end
	  | mktyfd q (F.ConTy ([s],[ty])) =
	  (case (Compiler.Symbol.name s) of
	     "list" => mktyfd (SOME T.Sequence) ty
	   | "option" =>  mktyfd (SOME T.Option) ty
	   | "ref" =>  mktyfd (SOME T.Shared) ty
	   | _ => (fn _ => NONE))
	  | mktyfd q (F.MarkTy (ty,_)) = mktyfd q ty
	  | mktyfd q _ = (fn _ => NONE)
	fun doty (F.RecordTy tys) =
	  let fun join ((s,ty),xs) = (mktyfd NONE ty (SOME(sym2id s)))::xs
	  in List.foldr join [] tys
	  end
	  | doty (F.TupleTy tys) =
	  let fun join (ty,xs) = (mktyfd NONE ty NONE)::xs
	  in List.foldr join [] tys
	  end
	  | doty (F.MarkTy(ty,_)) = doty ty
	  | doty (x as (F.ConTy _)) = [mktyfd NONE x NONE]
	  | doty  _ = [SOME extern_f]
	  val fds = List.mapPartial (fn x => x) (doty ty)
      in
	case fds of [] => (extern_f,[])
      | (x::xs) => (x,xs)
      end

    fun fromSML d = (fromDec (d,[]))
    fun doit s =
      let val ins = (TextIO.openIn s)
	  val decl = ParseSML.parse(s,ins)
	  val decls = fromSML decl
	  val pp = PPUtil.seq_term{fmt=PPASDL.pp_decl,sep=PPUtil.nl} decls
      in
	PPUtil.pp_to_outstream TextIO.stdOut 72 pp
      end
  end