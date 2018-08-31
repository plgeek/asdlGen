structure Tokens = Tokens
type pos = int
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult= (svalue,pos) token
type lexarg = {sourceMap: SourceMap.sourcemap,
	       err: (pos * pos) -> string -> unit}
type arg = lexarg

fun mkTok yypos tok = tok(yypos,yypos+1)
fun mkSTok yypos yytext tok = tok(yytext,yypos, yypos + (String.size yytext))
val zapws  = (Substring.dropl Char.isSpace) o (Substring.dropr Char.isSpace)
(* Doesn't handle quote string filename correctly *)
fun mySynch(sm,err,yypos,yytext) =
    let
	val zap_quotes =
	    String.translate (fn #"\"" => "" | x => (Char.toString x))
	val tokens = String.tokens Char.isSpace yytext
    in
	case tokens of
	    [_,d,s] =>
		SourceMap.resynch sm (yypos,{fileName=SOME (zap_quotes s),
				       line=Option.valOf(Int.fromString d),
				       column=NONE})
	  | [_,d] => 
		SourceMap.resynch sm (yypos,{fileName=NONE,
				       line=Option.valOf(Int.fromString d),
				       column=NONE})
	  | _ => err (yypos,yypos) ("Ignoring bad line directive:"^yytext)
    end

val eof = fn ({err,sourceMap}:lexarg) => 
    let
	val pos = SourceMap.lastChange sourceMap
    in
	Tokens.EOF(pos,pos)
    end
val charlist = ref ([]:string list)
fun addString (charlist,s:string) = charlist := s :: (!charlist)
fun addChar (charlist, c:char) = addString(charlist, String.str c)
fun makeString charlist = (concat(rev(!charlist)) before charlist := nil)

%%
%arg ({sourceMap,err});
%header (functor AsdlLexFun(structure Tokens: Asdl_TOKENS));
%s LT;
ws = [\ \t];
comment="--".*;
alpha=[A-Za-z];
alpha_num=[_0-9A-Za-z];
id={alpha}{alpha_num}*;
resynch="--#line"{ws}+[0-9]+({ws}+\"[^\"]*\")?;
%%

<INITIAL>\n     => (SourceMap.newline sourceMap yypos; continue());
<INITIAL>{resynch} => (mySynch(sourceMap,err,yypos,yytext);continue());
<INITIAL>{comment}         => (continue());
<INITIAL>{ws}+             => (continue());
<INITIAL>"["               => (mkTok yypos Tokens.LBRACK);
<INITIAL>"]"               => (mkTok yypos Tokens.RBRACK);
<INITIAL>"<Top>"           => (mkTok yypos Tokens.TOP);
<INITIAL>"<"               => (mkTok yypos Tokens.LANGLE);
<INITIAL>">"               => (mkTok yypos Tokens.RANGLE);
<INITIAL>"("               => (mkTok yypos Tokens.LPAREN);
<INITIAL>")"               => (mkTok yypos Tokens.RPAREN);
<INITIAL>"{"               => (mkTok yypos Tokens.LBRACE);
<INITIAL>"}"               => (mkTok yypos Tokens.RBRACE);
<INITIAL>","               => (mkTok yypos Tokens.FIELDSEP);
<INITIAL>"*"               => (mkTok yypos Tokens.SEQ);
<INITIAL>"."               => (mkTok yypos Tokens.DOT);
<INITIAL>"?"               => (mkTok yypos Tokens.OPT);
<INITIAL>"!"               => (mkTok yypos Tokens.SHARED);
<INITIAL>"|"               => (mkTok yypos Tokens.PIPE);
<INITIAL>"="               => (mkTok yypos Tokens.EQ);
<INITIAL>"attributes"      => (mkSTok yypos yytext Tokens.ATTRIBUTES);
<INITIAL>"module"          => (mkSTok yypos yytext Tokens.MODULE);
<INITIAL>"primitive"       => (mkSTok yypos yytext Tokens.PRIMITIVE);
<INITIAL>"imports"         => (mkSTok yypos yytext Tokens.IMPORTS);
<INITIAL>"alias"           => (mkSTok yypos yytext Tokens.ALIAS);
<INITIAL>"view"            => (mkSTok yypos yytext Tokens.VIEW);
<INITIAL>{id}              => (mkSTok yypos yytext Tokens.ID);
<INITIAL>":".*             => (mkSTok yypos
			       (Substring.string
				(zapws (Substring.triml 1
					(Substring.all yytext))))
			       Tokens.QUOTE);
<INITIAL>"%%"{ws}*         => (YYBEGIN LT; continue ());
<LT>"%%"{ws}*		   => (YYBEGIN INITIAL;
			       mkSTok yypos (makeString(charlist))
			       Tokens.QUOTE);
<LT>.*		           => (addString(charlist,yytext);continue());
<LT>\n                     => (SourceMap.newline sourceMap  yypos;
			       addString(charlist,yytext); continue());	    

<INITIAL>. => (err (yypos,yypos)  "ignoring bad character"; continue());
	  























