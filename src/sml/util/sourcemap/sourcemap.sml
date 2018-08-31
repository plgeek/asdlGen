(*#line 97 "sourcemap.nw"*)(* sourcemap.sml *)
(*#line 294 "sourcemap.nw"*)(* 
 * $Log: sourcemap.sml,v $
 * Revision 1.2  1997/12/03 06:51:15  danwang
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
(*#line 99 "sourcemap.nw"*)structure SourceMap : SOURCE_MAP = struct
  
(*#line 108 "sourcemap.nw"*)type charpos = int
type 'a pair = 'a * 'a
type region = charpos pair
val nullRegion : region = (0,0)
type sourceloc = {fileName:string, line:int, column:int}
(*#line 115 "sourcemap.nw"*)fun span ((0,0), r) = r
  | span (r, (0,0)) = r
  | span ((l1, h1), (l2, h2)) = if l1 < h2 then (l1, h2) else (l2, h1)
(*#line 139 "sourcemap.nw"*)type sourcemap = {resynchPos: (charpos * string * int) list ref,
                  linePos:    (charpos * int)          list ref}

fun newmap (pos, {fileName, line, column}: sourceloc) : sourcemap =
  {resynchPos = ref [(pos, fileName, column)], linePos = ref [(pos, line)]}

fun resynch ({resynchPos, linePos}: sourcemap) (pos, {fileName, line, column}) =
  let val curFile = #2 (hd (!resynchPos))
      fun thefile (SOME file) = if file = curFile then curFile else file
                                   (* pathetic attempt at hash-consing *)
        | thefile NONE        = #2 (hd (!resynchPos))
      fun thecol NONE     = 1
        | thecol (SOME c) = c
  in  resynchPos := (pos, thefile fileName, thecol column) :: !resynchPos;
      linePos := (pos, line) :: !linePos
  end
(*#line 159 "sourcemap.nw"*)fun newline ({resynchPos, linePos}: sourcemap) pos =
  let val (_, line) = hd (!linePos)
  in  linePos := (pos+1, line+1) :: !linePos
  end

fun lastChange({linePos, ...}: sourcemap) = #1 (hd (!linePos))
(*#line 170 "sourcemap.nw"*)fun remove p ({resynchPos,linePos}: sourcemap) =
  let fun strip (l as (pos, _   )::rest) = if p pos then strip  rest else l
        | strip [] = []
      fun strip'(l as (pos, _, _)::rest) = if p pos then strip' rest else l
        | strip'[] = []
  in  (strip'(!resynchPos), strip (!linePos))
  end
(*#line 182 "sourcemap.nw"*)fun column ((pos, file, col), (pos', line), p) =
  if pos = pos' then p - pos + col else p - pos' + 1

fun filepos smap p : sourceloc =
    let val (files, lines) = remove (fn pos : int => pos > p) smap
        val xx as (_, file, _) = hd files
        val yy as (_, line)    = hd lines
    in  {fileName = file, line = line, column = column(xx, yy, p)}
    end
(*#line 196 "sourcemap.nw"*)fun fileregion smap (lo, hi) =
  if (lo,hi) = nullRegion then [] else
  let exception Impossible
      fun gather((p, file, col)::files, (p', line)::lines, region_end, answers) =
        if p' <= lo then (* last item *)
          ({fileName=file, line=line, column=column((p, file, col), (p', line), lo)}, 
           region_end) :: answers
        else
          if p < p' then
            gather((p, file, col)::files, lines, region_end, answers)
          else (* p = p'; new region *)
            gather(files, lines, end_of (p, hd files, hd lines), 
                 ({fileName = file, line = line, column = col}, region_end) :: answers)
        | gather _ = raise Impossible
      and end_of(lastpos, xx as (p, file, col), yy as (p', line)) = 
             {fileName=file, line=line, column=column(xx, yy, lastpos)}
      val (files, lines) = remove (fn pos : int => pos >= hi andalso pos > lo) smap
      val _ = if null files orelse null lines then raise Impossible else ()
      val answer = gather(files, lines, end_of(hi, hd files, hd lines), [])
      fun validate(({fileName=f,  line=l,  column=c}, 
                    {fileName=f', line=l', column=c'}) :: rest) = 
            if f = f' andalso (l' > l orelse (l' = l andalso c' >= c)) then
              validate rest 
            else 
              raise Impossible
        | validate [] = ()
  in  validate answer; answer
  end
(*#line 230 "sourcemap.nw"*)fun positions({resynchPos,linePos}: sourcemap) (src:sourceloc) =
  let exception Unimplemented
  in  raise Unimplemented
  end
(*#line 238 "sourcemap.nw"*)fun forgetOldPositions ({resynchPos, linePos} : sourcemap) =
  let val r as (p,  file, col) = hd (!resynchPos)
      val l as (p', line)      = hd (!linePos)
  in  linePos := [l];
      resynchPos := [if p = p' then r else (p', file, 1)]
  end
(*#line 246 "sourcemap.nw"*)fun newlineCount smap (lo, hi) =
  let val (hifiles, hilines) = remove (fn pos : int => pos >= hi andalso pos > lo) smap
      val (lofiles, lolines) = remove (fn pos : int =>                   pos > lo) smap
  in  length hilines - length hifiles - (length lolines - length lofiles)
  end
(*#line 101 "sourcemap.nw"*)end
