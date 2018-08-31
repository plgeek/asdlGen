(*
 * Stolen from John Reppy's ML-DOC tool set.
 * parse-sml.sml 
 *
 * COPYRIGHT (c) 1996 AT&T Research.
 *)

structure ParseSML : sig

    val parse : string * TextIO.instream -> Compiler.Ast.dec

  end = struct

    structure C = Compiler
    structure Env = C.BareEnvironment

    val outs = TextIO.stdErr
    fun ppflush () = TextIO.flushOut outs
    fun ppout s = TextIO.output (outs, s)

    fun parse (fname, ins) = let
          val ppcons = {consumer = ppout, linewidth=80, flush = ppflush}
          val src = C.Source.newSource(fname,1,ins,false,ppcons)
          in
            (C.Compile.parse src) before (TextIO.closeIn ins)
          end handle e => (TextIO.closeIn ins; raise e)

  end