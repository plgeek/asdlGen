signature STD_PRIMS_UTIL =
  sig
    val write_int       : StdPrims.int -> StdPkl.outstream -> unit
    val write_big_int   : StdPrims.big_int -> StdPkl.outstream -> unit
    val write_string    : StdPrims.string -> StdPkl.outstream ->unit
    val write_identifier: StdPrims.identifier -> StdPkl.outstream ->unit
      
    val read_int        : StdPkl.instream -> StdPrims.int
    val read_big_int    : StdPkl.instream -> StdPrims.big_int
    val read_string     : StdPkl.instream -> StdPrims.string
    val read_identifier : StdPkl.instream -> StdPrims.identifier

    (* textual readers and writers *)
    val sexp_wr_int       : StdPrims.int -> SexpPkl.outstream -> unit
    val sexp_wr_big_int   : StdPrims.big_int -> SexpPkl.outstream -> unit
    val sexp_wr_string    : StdPrims.string -> SexpPkl.outstream ->unit
    val sexp_wr_identifier: StdPrims.identifier -> SexpPkl.outstream ->unit
      
    val sexp_rd_int        : SexpPkl.instream -> StdPrims.int
    val sexp_rd_big_int    : SexpPkl.instream -> StdPrims.big_int
    val sexp_rd_string     : SexpPkl.instream -> StdPrims.string
    val sexp_rd_identifier : SexpPkl.instream -> StdPrims.identifier

  end