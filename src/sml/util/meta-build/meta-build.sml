functor CombineBuilds(structure R : RUNNABLE_BUILD
		     structure E : EXPORTABLE_BUILD) :> META_BUILD =
  struct

    type 'a one_or_more = ('a * 'a list)
    type 'a cmd = ('a R.cmd * 'a E.cmd)
    type var = (R.var * E.var)
    type valid = (R.valid * E.valid)
    type rule = (R.rule * E.rule)

    fun fst (x,_) = x
    fun snd (_,y) = y
    fun pair (f,g) x = (f x,g x)

    fun mkVAR {doc,init,name} =
      (R.mkVAR{doc=doc,name=name,init=fst init},
       E.mkVAR{doc=doc,name=name,init=snd init})
    fun VAR x = pair (R.VAR o fst,E.VAR o snd) x
    fun INT x = pair (R.INT,E.INT) x
    fun STR x = pair (R.STR,E.STR) x
    fun WRITE x = pair (R.WRITE o fst,E.WRITE o snd) x
    fun CONCAT x =
      pair (R.CONCAT o (List.map fst),E.CONCAT o (List.map snd)) x
    fun EXEC ((x,y),cs) =
      (R.EXEC(x,List.map fst cs),E.EXEC(y,List.map snd cs))
    fun EXEC_WITH_INPUT ((x,y),cs,(z,w)) =
      (R.EXEC_WITH_INPUT(x,List.map fst cs,z),
       E.EXEC_WITH_INPUT(y,List.map snd cs,w))
    fun EXIT x = pair (R.EXIT o fst,E.EXIT o snd) x
    fun OR x = pair (R.OR o (List.map fst), E.OR o (List.map snd)) x
    fun AND x = pair (R.AND o (List.map fst), E.AND o (List.map snd)) x
    fun SEQ x = pair (R.SEQ o (List.map fst), E.SEQ o (List.map snd)) x
    fun IGN x = pair (R.IGN o fst,E.IGN o snd) x

    val INVALID  = (R.INVALID,E.INVALID)
    fun VALIDATE x = pair (R.VALIDATE,E.VALIDATE) x

    fun RULE {valid=(vx,vy),update=(ux,uy)} =
      (R.RULE{valid=vx,update=ux},E.RULE{valid=vy,update=uy})
    fun BUILD {name,rules} =
      (R.BUILD{name=name,rules=List.map fst rules},
       E.BUILD{name=name,rules=List.map snd rules})

    fun export s (_,x)    = (E.export s x)
    fun run    (x,_)    = R.run x
    fun setVAR  (v,_) s = R.setVAR v s
  end

structure MetaBuild = CombineBuilds (structure R = BuildRun
				     structure E = BuildExport)