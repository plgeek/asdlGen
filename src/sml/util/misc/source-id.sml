functor mkSourceId(val namespace : SourceIds.namespace
		         val sep : string) :> SOURCE_ID =
  struct
    type id = SourceIds.sid
    type path = {base:string,qualifier:string list}
    val namespace = namespace
    fun addNS {base,qualifier} =
      {base=base,qualifier=qualifier,ns=namespace}

    fun dropNS {ns,base,qualifier} = {base=base,qualifier=qualifier}

    val fromPath = SourceIds.fromPath o addNS
    val toPath = dropNS o SourceIds.toPath

    val uniqueId = SourceIds.uniqueId o addNS
    fun tempId s = SourceIds.uniqueId {base=s,ns=namespace,qualifier=[]}

    val getBase = #base o SourceIds.toPath
    val getQualifier = #qualifier o SourceIds.toPath      
    fun subst f id =
      (case (f (toPath id)) of
	 NONE => id
       | SOME id => fromPath id)

    fun suffixBase str id =
      let val {base,qualifier} = toPath id
      in fromPath {base=base^str,qualifier=qualifier}
      end

    fun prefixBase str id =
      let val {base,qualifier} = toPath id
      in fromPath {base=str^base,qualifier=qualifier}
      end

    fun pathToString sep {base,qualifier} =
      let
	val qualifier = if qualifier = [""] then []
			else qualifier
      in ListFormat.fmt
	{init="",sep=sep,final="",fmt=(fn x => x)}
	(qualifier @[base])
      end
    fun fromString s = fromPath{base=s,qualifier=[]}
    val compare = SourceIds.compare
    fun eq (x,y) = compare(x,y) = EQUAL
    fun toString' sep s = pathToString sep (toPath s)
    val toString = toString' sep
    fun toSid x = x
    val fromSid = fromPath o dropNS o SourceIds.toPath
  end

