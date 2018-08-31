(*#line 65 "sourcemap.nw"*)(* sourcemap.sig *)
(*#line 253 "sourcemap.nw"*)(* 
 * $Log: sourcemap.sig,v $
 * Revision 1.2  1997/12/03 06:50:55  danwang
 * *** empty log message ***
 *
 * Revision 1.2  1997/12/03 06:49:16  danwang
 * *** empty log message ***
 *
 * Revision 1.1  1997/12/03 06:40:53  danwang
 * Initial revision
 *
 * Revision 1.1.1.1  1997/01/14  01:38:49  george
 *   Version 109.24
 *
# Revision 1.2  1996/08/16  19:35:47  nr
# Corrected bug in which an error was misreported if it occured in an
# empty range located precisely at a resynchronization.  The fix was to
# use the condition (pos >= hi andalso pos > 0) instead of just (pos >= hi).
#
 * Revision 1.1  1996/08/08 19:30:30  nr
 * split old Source structure into Source and SourceMap
 *
 * changed ErrorMsg to use SourceMap to get source locations; only the
 * formatting is done internally
 *
 * added SourceMap structure
 *
 * .sig and .sml for sourcemap, source, and errormsg are derived from .nw
 * files.  to extract, try
 *   for base in sourcemap source errormsg
 *   do
 *     for suffix in sml sig
 *     do
 *       $cmd -L'(*#line %L "%F"*)' -R$base.$suffix $base.nw > $base.$suffix
 *     done
 *   done
 * where
 *   cmd=notangle
 * or
 *   cmd="nountangle -ml"
 *
 * At some point, it may be desirable to move noweb support into CM
 * *)
(*#line 67 "sourcemap.nw"*)signature SOURCE_MAP = sig
  type charpos (* = int *)
  type 'a pair (* = 'a * 'a *)
  type region  (* = charpos pair *)
  val span : region * region -> region (* smallest region containing the two regions *)
  val nullRegion : region              (* left and right identity of span *)

  type sourceloc (* = {fileName:string, line:int, column:int} *)

  type sourcemap (* = opaque mutable *)
  val newmap  : charpos * sourceloc -> sourcemap
  val newline : sourcemap -> charpos -> unit
  val resynch : sourcemap -> 
                charpos * {fileName:string option, line:int, column:int option} -> unit
  val forgetOldPositions : sourcemap -> unit

  val filepos     : sourcemap -> charpos -> sourceloc
  val fileregion  : sourcemap -> region  -> sourceloc pair list
  val positions   : sourcemap -> sourceloc -> charpos list

  val lastChange  : sourcemap -> charpos
  val newlineCount: sourcemap -> region -> int
end
