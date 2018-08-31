signature Asdl_SIG = 
    sig
    include BASE
    datatype asdl_type = SumType of (identifier * field list * constructor
                                     * constructor list)
                       | ProductType of (identifier * field * field list)
    and field = Id of (identifier list * identifier option)
              | Option of (identifier list * identifier option)
              | Sequence of (identifier list * identifier option)
    and constructor = Con of (identifier * field list)
    withtype asdl_module = {name:identifier,
                            imports:identifier list,
                            defs:asdl_type list}
    
    val write_asdl_type : asdl_type -> outstream -> unit
    val read_asdl_type : instream -> asdl_type
    val write_field : field -> outstream -> unit
    val read_field : instream -> field
    val write_constructor : constructor -> outstream -> unit
    val read_constructor : instream -> constructor
    val write_asdl_module : asdl_module -> outstream -> unit
    val read_asdl_module : instream -> asdl_module
    
end