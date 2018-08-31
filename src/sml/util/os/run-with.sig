signature RUN_WITH =
  sig
    datatype ins =
      Tins of TextIO.instream
    | Bins of BinIO.instream 

    datatype outs =
      Touts of TextIO.outstream
    | Bouts of BinIO.outstream

    val run_with : (ins option * outs option) -> string -> OS.Process.status
  end

