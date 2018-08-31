(*
  Machine generated. Edit at your own risk 
  Reproduce with the following
 --line_width=74
 --no_action=false
 --output_directory=frontend
 --sexp_pickler=true
 --view=SML
 --xml_pickler=false
 *)
signature AsdlUtil_SIG = 
  sig
    
    val attrbs_type_decl : Asdl.type_decl -> {name:StdPrims.identifier}
    val attrbs_decl : Asdl.decl -> {name:StdPrims.identifier}
    val sexp_rd_import : SexpPkl.instream -> Asdl.import
    val sexp_rd_path : SexpPkl.instream -> Asdl.path
    val sexp_rd_tycon : SexpPkl.instream -> Asdl.tycon
    val sexp_rd_field : SexpPkl.instream -> Asdl.field
    val sexp_rd_constructor : SexpPkl.instream -> Asdl.constructor
    val sexp_rd_type_decl : SexpPkl.instream -> Asdl.type_decl
    val sexp_rd_view_decl : SexpPkl.instream -> Asdl.view_decl
    val sexp_rd_decl : SexpPkl.instream -> Asdl.decl
    val sexp_rd_constructor_list : SexpPkl.instream ->
        Asdl.constructor list
    val sexp_rd_field_list : SexpPkl.instream -> Asdl.field list
    val sexp_rd_tycon_option : SexpPkl.instream -> Asdl.tycon option
    val sexp_rd_view_decl_list : SexpPkl.instream -> Asdl.view_decl list
    val sexp_rd_type_decl_list : SexpPkl.instream -> Asdl.type_decl list
    val sexp_rd_import_list : SexpPkl.instream -> Asdl.import list
    val sexp_wr_import : Asdl.import -> SexpPkl.outstream -> unit
    val sexp_wr_path : Asdl.path -> SexpPkl.outstream -> unit
    val sexp_wr_tycon : Asdl.tycon -> SexpPkl.outstream -> unit
    val sexp_wr_field : Asdl.field -> SexpPkl.outstream -> unit
    val sexp_wr_constructor : Asdl.constructor ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_type_decl : Asdl.type_decl -> SexpPkl.outstream -> unit
    val sexp_wr_view_decl : Asdl.view_decl -> SexpPkl.outstream -> unit
    val sexp_wr_decl : Asdl.decl -> SexpPkl.outstream -> unit
    val sexp_wr_constructor_list : Asdl.constructor list ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_field_list : Asdl.field list -> SexpPkl.outstream -> unit
    val sexp_wr_tycon_option : Asdl.tycon option ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_view_decl_list : Asdl.view_decl list ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_type_decl_list : Asdl.type_decl list ->
        SexpPkl.outstream ->
        unit
    val sexp_wr_import_list : Asdl.import list ->
        SexpPkl.outstream ->
        unit
  end (* sig *)

