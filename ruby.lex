(* Ruby lexer for sml-lex 
   tjt: updated for SML/NJ 110+
   - change character consts from 1-elem strings to SML character literals
     e.g. "." => #"."
   - similarly, chnage ator.splitdot to suit explode returning a list of
	 character literals rather than a list of strings
	 i.e. is^x => is ^ str(x)
   - revfold => List.foldr
   - lex_error => Errors.lex_error
   - seems to work
*)
(* for Spectrum, using {} to take place of <> 
comment = "#"[^\n]* | "{"[^\}]*"}";
*)

structure Tokens = Tokens

type pos = (int * int)
type svalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (svalue,pos) token

val pos = ref (1,1)
val eof = fn() => Tokens.EOF(!pos,!pos)

fun incline() = let val (l,c) = !pos in (pos := (l+1,c); !pos) end
fun inccol (n:int) = let val (l,c) = !pos in (pos := (l,c+n); !pos) end
fun zerocol () = let val (l,c) = !pos in (pos := (l,1); !pos) end
fun zeroline () = let val (l,c) = !pos in (pos := (1,c); !pos) end
fun strlen s = length (explode s)
fun strtl s = implode (tl (explode s))
fun stripQuotes s = implode (rev (tl (rev (tl (explode s)))))

fun Val base str =
    (foldr (fn(a,r)=> (if ord(a)>ord(#"9") then ord(a)-ord(#"a")+10
                         else ord(a)-ord(#"0")) + base*r)
     0 str)

fun todigit a = if (ord(a) <= ord(#"9") andalso ord(a) >= ord(#"0"))
                  then ord(a) - ord(#"0") else 0 ;

fun atoi a = let fun pow x 0 = 1
                   | pow x n = x * (pow x (n-1)) ;
                 fun sum [] n = 0
                   | sum (x::xs) n = x * (pow 10 n) + (sum xs (n+1))
             in (sum (rev (map todigit (explode a))) 0)
             end;

fun tofract []      = 0.0
  | tofract (x::xs) = (real (todigit x) + tofract xs) / 10.0;

fun ator a = let fun splitdot (   [],is) = real (atoi is)
                   | splitdot (x::xs,is) = if x <> #"."
                                             then splitdot (xs,is^str(x))
                                             else real (atoi is) + tofract xs
             in
               splitdot (explode a,"")
             end;

fun stob "T" = true
  | stob "F" = false;


%%
%header(functor RubyLexFun(structure Tokens : Ruby_TOKENS));
ident=[A-Za-z\?_'][A-Za-z{}\?_'0-9]*;
digit = [0-9];



comment = "#"[^\n]* ;

symbol = "\""[^\n^\"]*"\"";
int = {digit}{digit}*;
real = {int}"."{digit}{digit}*;
ws=[\ \t];
%%
\n                     => (incline(); zerocol(); lex());
{ws}+                  => (inccol (strlen yytext); lex());
{comment}              => (inccol (size yytext); lex());
"("                    => (Tokens.LPAREN(!pos, inccol 1));
")"                    => (Tokens.RPAREN(!pos, inccol 1));
"<"                    => (Tokens.LANGLE(!pos, inccol 1));
">"                    => (Tokens.RANGLE(!pos, inccol 1));
"="                    => (Tokens.EQUAL(!pos, inccol 1));
"."                    => (Tokens.DOT(!pos, inccol 1));
","                    => (Tokens.COMMA(!pos, inccol 1));
"["                    => (Tokens.LBKT(!pos, inccol 1));
"]"                    => (Tokens.RBKT(!pos, inccol 1));
"$wire"                => (Tokens.WIRE(!pos, inccol 5));
"$rel"                 => (Tokens.REL(!pos, !pos, inccol 4));
"INCLUDE"              => (Tokens.INCLUDE(!pos, inccol 7));
"SELECT"               => (Tokens.SELECT(!pos, inccol 6));
"IF"                   => (Tokens.IF(!pos, inccol 2));
"THEN"                 => (Tokens.THEN(!pos, inccol 4));
"ELSE"                 => (Tokens.ELSE(!pos, inccol 4));
"LET"                  => (Tokens.LET(!pos, inccol 3));
"IN"                   => (Tokens.IN(!pos, inccol 2));
"END"                  => (Tokens.END(!pos, inccol 3));
"VAR"                  => (Tokens.VAR(!pos, inccol 3));
";"                    => (Tokens.SEMICOLON(!pos, !pos, inccol 1));
"~"                    => (Tokens.NEG(yytext, !pos, inccol 1));
"+"                    => (Tokens.SUM(yytext, !pos, inccol 1));
"-"                    => (Tokens.SUM(yytext, !pos, inccol 1));
"*"                    => (Tokens.PRODUCT(yytext, !pos, inccol 1));
"/"                    => (Tokens.PRODUCT(yytext, !pos, inccol 1));
"\^~1"                 => (Tokens.POSTFIX(yytext, !pos, inccol 3));
"\^H"                  => (Tokens.POSTFIX(yytext, !pos, inccol 2));
"\^V"                  => (Tokens.POSTFIX(yytext, !pos, inccol 2));
"\^"                   => (Tokens.INFIX(yytext, !pos, inccol 1));
"||"                   => (Tokens.INFIX(yytext, !pos, inccol 2));
"\\"                   => (Tokens.INFIX(yytext, !pos, inccol 1));
"\\\\"                 => (Tokens.INFIX(yytext, !pos, inccol 2));
"<->"                  => (Tokens.INFIX(yytext, !pos, inccol 3));
"<|>"                  => (Tokens.INFIX(yytext, !pos, inccol 3));
"[-]"                  => (Tokens.ID(yytext, !pos, inccol 3));
"/\\"                  => (Tokens.ID(yytext, !pos, inccol 2));
"/\\~"                 => (Tokens.ID(yytext, !pos, inccol 3));
"T"                    => (Tokens.BOOLEAN(stob yytext, !pos, inccol 1));
"F"                    => (Tokens.BOOLEAN(stob yytext,!pos, inccol 1));
"`"                    => (Tokens.QUOTE(!pos, inccol 1));
{symbol}               => (Tokens.SYMBOL(stripQuotes yytext, !pos, inccol (strlen yytext)));
"$"{ident}             => (Tokens.INFIX(yytext, !pos, inccol (strlen yytext)));
"&"{ident}             => (Tokens.DID(yytext, !pos, inccol (strlen yytext)));
{ident}                => (Tokens.ID(yytext, !pos, inccol (strlen yytext)));
{int}                  => (Tokens.INTEGER(atoi yytext,!pos,inccol (strlen yytext)));
"~"{int}               => (Tokens.INTEGER(~(atoi (strtl yytext)),!pos,inccol (strlen yytext)));
{real}                 => (Tokens.FLOAT(ator yytext, !pos, inccol (strlen yytext)));
"~"{real}              => (Tokens.FLOAT(~(ator (strtl yytext)), !pos, inccol (strlen yytext)));
.                      => (Errors.lex_error (!pos, "Bad character: "^yytext);
                           inccol 1;
                           lex());

