functor RubyLrValsFun(structure Token : TOKEN)
 : sig structure ParserData : PARSER_DATA
       structure Tokens : Ruby_TOKENS
   end
 = 
struct
structure ParserData=
struct
structure Header = 
struct
(*#line 1.2 "ruby.yacc"*)(* Ruby parser for sml-yacc *)

(* tjt: updated for SML/NJ 110+:
 aAPP =>A.aAPP
 (+ other AST elements)
 char literals: "{" => #"{" etc.
 in Change_brace_2_bracket
*)

structure A = Rubytype

(* for Spectrum system *)

fun Change_brace2_bracket_inString str = 
       let fun change_brace2_bracket      [] = []
             | change_brace2_bracket (x::xs) = 
                if x = #"{" then  #"<" :: (change_brace2_bracket xs)
                else if x = #"}" then  #">" :: (change_brace2_bracket xs)
                else x :: (change_brace2_bracket xs)
       in implode (change_brace2_bracket (explode str)) end

fun brace2bracket (A.VAR(ID))     = A.VAR (Change_brace2_bracket_inString ID)
  | brace2bracket (A.CONST CON)   = A.CONST(CON)
  | brace2bracket (A.PROD(TUPLE)) = 
        A.PROD (map brace2bracket TUPLE)

(* tjt: change operators on-the-fly into equivalent word form 
 + => add
 - => minus
 * => mult
 / => div
 < => ltn
 <= => leq
 > => gtn
 >= => geq
*)
fun operator2Name ("+") = "add"
  | operator2Name ("-") = "minus"
  | operator2Name ("*") = "mult"
  | operator2Name ("/") = "div"
  | operator2Name ("~") = "~"
  | operator2Name str = raise Errors.PARSE_ERROR ("unknown operator '" ^ str ^ "'")

(* tjt: build binary app from arguments *)
fun buildBinaryApp (name, lhs, rhs) =
	(A.aAPP(A.Var(name),A.aPROD([lhs,rhs])))

(* tjt: build unary app from arguments *)
fun buildUnaryApp (name, operand) =
	(A.aAPP(A.Var(name),A.aPROD([operand])))


(*#line 63.1 "ruby.yacc.sml"*)
end
structure LrTable = Token.LrTable
structure Token = Token
local open LrTable in 
val table=let val actionRows =
"\
\\001\000\001\000\153\000\006\000\153\000\007\000\153\000\008\000\166\000\
\\012\000\153\000\014\000\153\000\015\000\153\000\021\000\153\000\
\\022\000\153\000\024\000\153\000\025\000\153\000\026\000\153\000\
\\027\000\153\000\000\000\
\\001\000\001\000\154\000\002\000\045\000\006\000\154\000\007\000\154\000\
\\008\000\165\000\011\000\043\000\012\000\154\000\014\000\154\000\
\\015\000\154\000\021\000\154\000\022\000\154\000\024\000\154\000\
\\025\000\154\000\026\000\154\000\027\000\154\000\028\000\022\000\
\\029\000\060\000\031\000\020\000\032\000\019\000\033\000\018\000\000\000\
\\001\000\001\000\087\000\021\000\055\000\024\000\054\000\025\000\053\000\
\\026\000\052\000\027\000\051\000\000\000\
\\001\000\001\000\117\000\024\000\116\000\025\000\115\000\026\000\114\000\000\000\
\\001\000\001\000\126\000\000\000\
\\001\000\001\000\127\000\000\000\
\\001\000\001\000\128\000\000\000\
\\001\000\001\000\129\000\000\000\
\\001\000\002\000\045\000\003\000\023\000\010\000\044\000\011\000\043\000\
\\012\000\068\000\013\000\042\000\016\000\041\000\019\000\040\000\
\\023\000\039\000\028\000\022\000\029\000\038\000\031\000\020\000\
\\032\000\019\000\033\000\018\000\000\000\
\\001\000\002\000\045\000\003\000\023\000\010\000\044\000\011\000\043\000\
\\013\000\042\000\016\000\041\000\019\000\040\000\023\000\039\000\
\\028\000\022\000\029\000\038\000\031\000\020\000\032\000\019\000\
\\033\000\018\000\000\000\
\\001\000\002\000\045\000\011\000\043\000\028\000\022\000\029\000\060\000\
\\031\000\020\000\032\000\019\000\033\000\018\000\000\000\
\\001\000\002\000\095\000\003\000\094\000\004\000\104\000\022\000\093\000\
\\028\000\022\000\029\000\092\000\031\000\020\000\032\000\019\000\
\\033\000\018\000\000\000\
\\001\000\002\000\095\000\003\000\094\000\022\000\093\000\023\000\106\000\
\\028\000\022\000\029\000\092\000\031\000\020\000\032\000\019\000\
\\033\000\018\000\000\000\
\\001\000\002\000\095\000\003\000\094\000\022\000\093\000\028\000\022\000\
\\029\000\092\000\031\000\020\000\032\000\019\000\033\000\018\000\000\000\
\\001\000\003\000\023\000\004\000\032\000\028\000\022\000\029\000\021\000\
\\031\000\020\000\032\000\019\000\033\000\018\000\000\000\
\\001\000\003\000\023\000\028\000\022\000\029\000\021\000\031\000\020\000\
\\032\000\019\000\033\000\018\000\000\000\
\\001\000\004\000\049\000\000\000\
\\001\000\004\000\113\000\000\000\
\\001\000\005\000\009\000\000\000\
\\001\000\005\000\024\000\000\000\
\\001\000\005\000\026\000\000\000\
\\001\000\005\000\047\000\000\000\
\\001\000\005\000\083\000\000\000\
\\001\000\006\000\028\000\000\000\
\\001\000\006\000\029\000\000\000\
\\001\000\006\000\056\000\021\000\055\000\024\000\054\000\025\000\053\000\
\\026\000\052\000\027\000\051\000\000\000\
\\001\000\006\000\071\000\021\000\055\000\024\000\054\000\025\000\053\000\
\\026\000\052\000\027\000\051\000\000\000\
\\001\000\006\000\080\000\000\000\
\\001\000\006\000\088\000\021\000\055\000\024\000\054\000\025\000\053\000\
\\026\000\052\000\027\000\051\000\000\000\
\\001\000\008\000\050\000\000\000\
\\001\000\011\000\043\000\000\000\
\\001\000\012\000\084\000\000\000\
\\001\000\014\000\082\000\000\000\
\\001\000\015\000\108\000\021\000\055\000\024\000\054\000\025\000\053\000\
\\026\000\052\000\027\000\051\000\000\000\
\\001\000\017\000\081\000\000\000\
\\001\000\018\000\107\000\000\000\
\\001\000\020\000\100\000\000\000\
\\001\000\021\000\055\000\022\000\111\000\024\000\054\000\025\000\053\000\
\\026\000\052\000\027\000\051\000\000\000\
\\001\000\028\000\014\000\000\000\
\\001\000\029\000\027\000\000\000\
\\001\000\029\000\065\000\000\000\
\\001\000\034\000\000\000\000\000\
\\131\000\009\000\007\000\029\000\006\000\030\000\005\000\000\000\
\\132\000\009\000\007\000\029\000\006\000\030\000\005\000\000\000\
\\133\000\000\000\
\\134\000\000\000\
\\135\000\000\000\
\\136\000\000\000\
\\137\000\000\000\
\\138\000\000\000\
\\139\000\000\000\
\\140\000\000\000\
\\141\000\024\000\054\000\025\000\053\000\026\000\052\000\027\000\051\000\000\000\
\\142\000\024\000\054\000\025\000\053\000\000\000\
\\143\000\025\000\053\000\000\000\
\\144\000\000\000\
\\145\000\000\000\
\\146\000\000\000\
\\147\000\000\000\
\\148\000\000\000\
\\149\000\000\000\
\\150\000\000\000\
\\151\000\000\000\
\\152\000\000\000\
\\153\000\000\000\
\\154\000\000\000\
\\155\000\000\000\
\\156\000\000\000\
\\157\000\002\000\045\000\011\000\043\000\028\000\022\000\029\000\060\000\
\\031\000\020\000\032\000\019\000\033\000\018\000\000\000\
\\158\000\000\000\
\\159\000\007\000\109\000\021\000\055\000\024\000\054\000\025\000\053\000\
\\026\000\052\000\027\000\051\000\000\000\
\\160\000\000\000\
\\161\000\000\000\
\\162\000\000\000\
\\163\000\000\000\
\\164\000\000\000\
\\165\000\000\000\
\\166\000\000\000\
\\167\000\000\000\
\\168\000\000\000\
\\169\000\000\000\
\\170\000\000\000\
\\171\000\000\000\
\\172\000\007\000\085\000\021\000\055\000\024\000\054\000\025\000\053\000\
\\026\000\052\000\027\000\051\000\000\000\
\\173\000\000\000\
\\174\000\007\000\048\000\000\000\
\\175\000\000\000\
\\176\000\026\000\013\000\027\000\012\000\029\000\011\000\000\000\
\\176\000\029\000\011\000\000\000\
\\177\000\000\000\
\\178\000\000\000\
\\179\000\000\000\
\\180\000\000\000\
\\181\000\000\000\
\\182\000\000\000\
\\183\000\000\000\
\\184\000\000\000\
\\185\000\000\000\
\\186\000\000\000\
\\187\000\000\000\
\\188\000\000\000\
\\189\000\007\000\112\000\000\000\
\\190\000\000\000\
\"
val actionRowNumbers =
"\043\000\044\000\042\000\018\000\
\\087\000\038\000\045\000\015\000\
\\019\000\088\000\020\000\039\000\
\\023\000\024\000\078\000\077\000\
\\074\000\073\000\072\000\076\000\
\\075\000\014\000\009\000\089\000\
\\009\000\021\000\049\000\050\000\
\\085\000\016\000\079\000\029\000\
\\066\000\000\000\051\000\025\000\
\\001\000\010\000\088\000\010\000\
\\040\000\008\000\010\000\009\000\
\\026\000\009\000\015\000\080\000\
\\015\000\057\000\009\000\009\000\
\\009\000\009\000\046\000\064\000\
\\058\000\068\000\065\000\056\000\
\\027\000\034\000\032\000\022\000\
\\031\000\083\000\081\000\030\000\
\\002\000\048\000\028\000\086\000\
\\059\000\053\000\055\000\054\000\
\\052\000\069\000\013\000\010\000\
\\009\000\009\000\082\000\009\000\
\\060\000\067\000\047\000\092\000\
\\036\000\091\000\090\000\009\000\
\\011\000\012\000\035\000\033\000\
\\070\000\084\000\013\000\037\000\
\\101\000\017\000\099\000\003\000\
\\013\000\010\000\062\000\040\000\
\\063\000\013\000\013\000\100\000\
\\013\000\013\000\013\000\098\000\
\\004\000\061\000\071\000\093\000\
\\102\000\005\000\006\000\007\000\
\\097\000\094\000\096\000\095\000\
\\041\000"
val gotoT =
"\
\\001\000\128\000\002\000\002\000\003\000\001\000\000\000\
\\000\000\
\\003\000\006\000\000\000\
\\000\000\
\\015\000\008\000\000\000\
\\000\000\
\\000\000\
\\008\000\015\000\009\000\014\000\014\000\013\000\000\000\
\\000\000\
\\015\000\023\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\015\000\009\000\014\000\012\000\029\000\014\000\028\000\000\000\
\\004\000\035\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\000\000\
\\004\000\044\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\057\000\006\000\056\000\008\000\055\000\010\000\032\000\000\000\
\\005\000\059\000\008\000\055\000\010\000\032\000\000\000\
\\015\000\060\000\000\000\
\\005\000\061\000\008\000\055\000\010\000\032\000\000\000\
\\007\000\062\000\000\000\
\\004\000\065\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\011\000\064\000\014\000\031\000\000\000\
\\005\000\067\000\008\000\055\000\010\000\032\000\000\000\
\\004\000\068\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\000\000\
\\004\000\070\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\008\000\015\000\009\000\014\000\012\000\071\000\014\000\028\000\000\000\
\\000\000\
\\008\000\015\000\009\000\014\000\014\000\072\000\000\000\
\\000\000\
\\004\000\073\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\004\000\074\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\004\000\075\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\004\000\076\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\005\000\057\000\006\000\077\000\008\000\055\000\010\000\032\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\010\000\084\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\089\000\017\000\088\000\018\000\087\000\000\000\
\\005\000\094\000\008\000\055\000\010\000\032\000\000\000\
\\004\000\095\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\004\000\096\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\000\000\
\\004\000\065\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\011\000\097\000\014\000\031\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\004\000\099\000\005\000\034\000\008\000\033\000\009\000\014\000\
\\010\000\032\000\014\000\031\000\000\000\
\\008\000\089\000\016\000\101\000\017\000\100\000\018\000\087\000\000\000\
\\008\000\089\000\017\000\103\000\018\000\087\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\089\000\017\000\108\000\018\000\087\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\008\000\089\000\017\000\116\000\018\000\087\000\000\000\
\\005\000\117\000\008\000\055\000\010\000\032\000\000\000\
\\000\000\
\\007\000\118\000\000\000\
\\000\000\
\\008\000\089\000\017\000\119\000\018\000\087\000\000\000\
\\008\000\089\000\016\000\120\000\017\000\100\000\018\000\087\000\000\000\
\\000\000\
\\008\000\089\000\017\000\121\000\018\000\087\000\000\000\
\\008\000\089\000\017\000\122\000\018\000\087\000\000\000\
\\008\000\089\000\017\000\123\000\018\000\087\000\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\\000\000\
\"
val numstates = 129
val numrules = 60
val s = ref "" and index = ref 0
val string_to_int = fn () => 
let val i = !index
in index := i+2; Char.ord(String.sub(!s,i)) + Char.ord(String.sub(!s,i+1)) * 256
end
val string_to_list = fn s' =>
    let val len = String.size s'
        fun f () =
           if !index < len then string_to_int() :: f()
           else nil
   in index := 0; s := s'; f ()
   end
val string_to_pairlist = fn (conv_key,conv_entry) =>
     let fun f () =
         case string_to_int()
         of 0 => EMPTY
          | n => PAIR(conv_key (n-1),conv_entry (string_to_int()),f())
     in f
     end
val string_to_pairlist_default = fn (conv_key,conv_entry) =>
    let val conv_row = string_to_pairlist(conv_key,conv_entry)
    in fn () =>
       let val default = conv_entry(string_to_int())
           val row = conv_row()
       in (row,default)
       end
   end
val string_to_table = fn (convert_row,s') =>
    let val len = String.size s'
        fun f ()=
           if !index < len then convert_row() :: f()
           else nil
     in (s := s'; index := 0; f ())
     end
local
  val memo = Array.array(numstates+numrules,ERROR)
  val _ =let fun g i=(Array.update(memo,i,REDUCE(i-numstates)); g(i+1))
       fun f i =
            if i=numstates then g i
            else (Array.update(memo,i,SHIFT (STATE i)); f (i+1))
          in f 0 handle Subscript => ()
          end
in
val entry_to_action = fn 0 => ACCEPT | 1 => ERROR | j => Array.sub(memo,(j-2))
end
val gotoT=Array.fromList(string_to_table(string_to_pairlist(NT,STATE),gotoT))
val actionRows=string_to_table(string_to_pairlist_default(T,entry_to_action),actionRows)
val actionRowNumbers = string_to_list actionRowNumbers
val actionT = let val actionRowLookUp=
let val a=Array.fromList(actionRows) in fn i=>Array.sub(a,i) end
in Array.fromList(map actionRowLookUp actionRowNumbers)
end
in LrTable.mkLrTable {actions=actionT,gotos=gotoT,numRules=numrules,
numStates=numstates,initialState=STATE 0}
end
end
local open Header in
type pos =  ( int * int ) 
type arg = unit
structure MlyValue = 
struct
datatype svalue = VOID | ntVOID of unit ->  unit | FLOAT of unit ->  (real) | INTEGER of unit ->  (int) | BOOLEAN of unit ->  (bool) | DID of unit ->  (string) | ID of unit ->  (string) | SYMBOL of unit ->  (string) | POSTFIX of unit ->  (string) | INFIX of unit ->  (string) | PRODUCT of unit ->  (string) | SUM of unit ->  (string) | NEG of unit ->  (string) | SEMICOLON of unit ->  ( ( int*int ) ) | REL of unit ->  ( ( int*int ) ) | APPTUPLE of unit ->  ( ( A.appexp list ) ) | APPEXP of unit ->  ( ( A.appexp ) ) | APPEXPS of unit ->  ( ( A.appexp list ) ) | IDS of unit ->  ( ( string list ) ) | IO of unit ->  (A.io) | RHS of unit ->  (A.exp) | IOS of unit ->  ( ( A.io list ) ) | EXPS of unit ->  ( ( A.exp list ) ) | LIST of unit ->  ( ( A.exp list ) ) | TUPLE of unit ->  ( ( A.io list ) ) | CON of unit ->  (A.const) | LETEXPS of unit ->  ( ( (string * A.exp) list ) ) | SEXPS of unit ->  ( ( A.exp list ) ) | SEXP of unit ->  (A.exp) | EXP of unit ->  (A.exp) | DEFN of unit ->  (A.defn) | PROG of unit ->  ( ( A.defn list ) ) | START of unit ->  ( ( A.defn list ) )
end
type svalue = MlyValue.svalue
type result =  ( A.defn list ) 
end
structure EC=
struct
open LrTable
infix 5 $$
fun x $$ y = y::x
val is_keyword =
fn _ => false
val preferred_change : (term list * term list) list = 
nil
val noShift = 
fn (T 33) => true | _ => false
val showTerminal =
fn (T 0) => "RPAREN"
  | (T 1) => "LPAREN"
  | (T 2) => "LANGLE"
  | (T 3) => "RANGLE"
  | (T 4) => "EQUAL"
  | (T 5) => "DOT"
  | (T 6) => "COMMA"
  | (T 7) => "WIRE"
  | (T 8) => "INCLUDE"
  | (T 9) => "SELECT"
  | (T 10) => "LBKT"
  | (T 11) => "RBKT"
  | (T 12) => "LET"
  | (T 13) => "IN"
  | (T 14) => "END"
  | (T 15) => "IF"
  | (T 16) => "THEN"
  | (T 17) => "ELSE"
  | (T 18) => "VAR"
  | (T 19) => "REL"
  | (T 20) => "SEMICOLON"
  | (T 21) => "QUOTE"
  | (T 22) => "NEG"
  | (T 23) => "SUM"
  | (T 24) => "PRODUCT"
  | (T 25) => "INFIX"
  | (T 26) => "POSTFIX"
  | (T 27) => "SYMBOL"
  | (T 28) => "ID"
  | (T 29) => "DID"
  | (T 30) => "BOOLEAN"
  | (T 31) => "INTEGER"
  | (T 32) => "FLOAT"
  | (T 33) => "EOF"
  | _ => "bogus-term"
local open Header in
val errtermvalue=
fn _ => MlyValue.VOID
end
val terms : term list = nil
 $$ (T 33) $$ (T 21) $$ (T 18) $$ (T 17) $$ (T 16) $$ (T 15) $$ (T 14) $$ (T 13) $$ (T 12) $$ (T 11) $$ (T 10) $$ (T 9) $$ (T 8) $$ (T 7) $$ (T 6) $$ (T 5) $$ (T 4) $$ (T 3) $$ (T 2) $$ (T 1) $$ (T 0)end
structure Actions =
struct 
exception mlyAction of int
local open Header in
val actions = 
fn (i392,defaultPos,stack,
    (()):arg) =>
case (i392,stack)
of  ( 0, ( ( _, ( MlyValue.PROG PROG1, PROG1left, PROG1right)) :: rest671)) => let val  result = MlyValue.START (fn _ => let val  (PROG as PROG1) = PROG1 ()
 in ((*#line 96.50 "ruby.yacc"*)PROG(*#line 512.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 0, ( result, PROG1left, PROG1right), rest671)
end
|  ( 1, ( rest671)) => let val  result = MlyValue.START (fn _ => ((*#line 97.50 "ruby.yacc"*)[](*#line 518.1 "ruby.yacc.sml"*)
))
 in ( LrTable.NT 0, ( result, defaultPos, defaultPos), rest671)
end
|  ( 2, ( ( _, ( MlyValue.DEFN DEFN1, DEFN1left, DEFN1right)) :: rest671)) => let val  result = MlyValue.PROG (fn _ => let val  (DEFN as DEFN1) = DEFN1 ()
 in ((*#line 99.50 "ruby.yacc"*)[DEFN](*#line 522.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, DEFN1left, DEFN1right), rest671)
end
|  ( 3, ( ( _, ( MlyValue.DEFN DEFN1, _, DEFN1right)) :: ( _, ( MlyValue.PROG PROG1, PROG1left, _)) :: rest671)) => let val  result = MlyValue.PROG (fn _ => let val  (PROG as PROG1) = PROG1 ()
 val  (DEFN as DEFN1) = DEFN1 ()
 in ((*#line 100.50 "ruby.yacc"*)DEFN::PROG(*#line 528.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 1, ( result, PROG1left, DEFN1right), rest671)
end
|  ( 4, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.IDS IDS1, _, _)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.DEFN (fn _ => let val  (ID as ID1) = ID1 ()
 val  (IDS as IDS1) = IDS1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 102.50 "ruby.yacc"*)A.Fdefn(ID,IDS,EXP)(*#line 535.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, ID1left, DOT1right), rest671)
end
|  ( 5, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID2, _, _)) :: ( _, ( MlyValue.INFIX INFIX1, _, _)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.DEFN (fn _ => let val  ID1 = ID1 ()
 val  (INFIX as INFIX1) = INFIX1 ()
 val  ID2 = ID2 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 103.50 "ruby.yacc"*)A.Fdefn(INFIX,[ID1,ID2],EXP)(*#line 543.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, ID1left, DOT1right), rest671)
end
|  ( 6, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.POSTFIX POSTFIX1, _, _)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.DEFN (fn _ => let val  (ID as ID1) = ID1 ()
 val  (POSTFIX as POSTFIX1) = POSTFIX1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 104.50 "ruby.yacc"*)A.Fdefn(POSTFIX,[ID],EXP)(*#line 552.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, ID1left, DOT1right), rest671)
end
|  ( 7, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.SYMBOL SYMBOL1, _, _)) :: ( _, ( _, INCLUDE1left, _)) :: rest671)) => let val  result = MlyValue.DEFN (fn _ => let val  (SYMBOL as SYMBOL1) = SYMBOL1 ()
 in ((*#line 105.50 "ruby.yacc"*)A.Include(SYMBOL)(*#line 560.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, INCLUDE1left, DOT1right), rest671)
end
|  ( 8, ( ( _, ( _, _, DOT1right)) :: ( _, ( MlyValue.IO IO1, _, _)) :: _ :: ( _, ( MlyValue.DID DID1, DID1left, _)) :: rest671)) => let val  result = MlyValue.DEFN (fn _ => let val  (DID as DID1) = DID1 ()
 val  (IO as IO1) = IO1 ()
 in ((*#line 106.50 "ruby.yacc"*)A.Ddefn(DID,(brace2bracket IO))(*#line 566.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 2, ( result, DID1left, DOT1right), rest671)
end
|  ( 9, ( ( _, ( MlyValue.SEXP SEXP1, SEXP1left, SEXP1right)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (SEXP as SEXP1) = SEXP1 ()
 in ((*#line 108.50 "ruby.yacc"*)SEXP(*#line 573.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, SEXP1left, SEXP1right), rest671)
end
|  ( 10, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( MlyValue.SEMICOLON SEMICOLON1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  (SEMICOLON as SEMICOLON1) = SEMICOLON1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 109.50 "ruby.yacc"*)A.Seq(SEMICOLON,EXP1,EXP2)(*#line 579.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 11, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( MlyValue.INFIX INFIX1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  (INFIX as INFIX1) = INFIX1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 110.50 "ruby.yacc"*)A.App(INFIX,[EXP1,EXP2])(*#line 587.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 12, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( MlyValue.SUM SUM1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  (SUM as SUM1) = SUM1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 111.50 "ruby.yacc"*)A.App(SUM,[EXP1,EXP2])(*#line 595.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 13, ( ( _, ( MlyValue.EXP EXP2, _, EXP2right)) :: ( _, ( MlyValue.PRODUCT PRODUCT1, _, _)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  EXP1 = EXP1 ()
 val  (PRODUCT as PRODUCT1) = PRODUCT1 ()
 val  EXP2 = EXP2 ()
 in ((*#line 112.50 "ruby.yacc"*)A.App(PRODUCT,[EXP1,EXP2])(*#line 603.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, EXP2right), rest671)
end
|  ( 14, ( ( _, ( MlyValue.SEXP SEXP1, _, SEXP1right)) :: ( _, ( MlyValue.NEG NEG1, NEG1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (NEG as NEG1) = NEG1 ()
 val  (SEXP as SEXP1) = SEXP1 ()
 in ((*#line 113.50 "ruby.yacc"*)A.App(NEG,[SEXP])(*#line 611.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, NEG1left, SEXP1right), rest671)
end
|  ( 15, ( ( _, ( MlyValue.POSTFIX POSTFIX1, _, POSTFIX1right)) :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (POSTFIX as POSTFIX1) = POSTFIX1 ()
 in ((*#line 114.50 "ruby.yacc"*)A.App(POSTFIX,[EXP])(*#line 618.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, EXP1left, POSTFIX1right), rest671)
end
|  ( 16, ( ( _, ( MlyValue.SEXPS SEXPS1, _, SEXPS1right)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (ID as ID1) = ID1 ()
 val  (SEXPS as SEXPS1) = SEXPS1 ()
 in ((*#line 115.50 "ruby.yacc"*)A.App(ID,SEXPS)(*#line 625.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, ID1left, SEXPS1right), rest671)
end
|  ( 17, ( ( _, ( MlyValue.IO IO2, _, IO2right)) :: _ :: ( _, ( MlyValue.IO IO1, IO1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  IO1 = IO1 ()
 val  IO2 = IO2 ()
 in ((*#line 116.50 "ruby.yacc"*)A.Wiring(IO1,IO2)(*#line 632.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, IO1left, IO2right), rest671)
end
|  ( 18, ( ( _, ( MlyValue.LIST LIST1, _, LIST1right)) :: ( _, ( MlyValue.SEXP SEXP1, _, _)) :: ( _, ( _, SELECT1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (SEXP as SEXP1) = SEXP1 ()
 val  (LIST as LIST1) = LIST1 ()
 in ((*#line 117.50 "ruby.yacc"*)A.Select(SEXP,LIST)(*#line 639.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, SELECT1left, LIST1right), rest671)
end
|  ( 19, ( ( _, ( MlyValue.SEXP SEXP3, _, SEXP3right)) :: _ :: ( _, ( MlyValue.SEXP SEXP2, _, _)) :: _ :: ( _, ( MlyValue.SEXP SEXP1, _, _)) :: ( _, ( _, IF1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  SEXP1 = SEXP1 ()
 val  SEXP2 = SEXP2 ()
 val  SEXP3 = SEXP3 ()
 in ((*#line 118.50 "ruby.yacc"*)A.Select(SEXP1,[SEXP3,SEXP2])(*#line 646.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, IF1left, SEXP3right), rest671)
end
|  ( 20, ( ( _, ( _, _, END1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.LETEXPS LETEXPS1, _, _)) :: ( _, ( _, LET1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (LETEXPS as LETEXPS1) = LETEXPS1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 119.50 "ruby.yacc"*)A.Let(LETEXPS,EXP)(*#line 654.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, LET1left, END1right), rest671)
end
|  ( 21, ( ( _, ( MlyValue.APPEXP APPEXP2, _, APPEXP2right)) :: ( _, ( MlyValue.REL REL1, _, _)) :: ( _, ( MlyValue.APPEXP APPEXP1, _, _)) :: _ :: ( _, ( MlyValue.IDS IDS1, _, _)) :: ( _, ( _, VAR1left, _)) :: rest671)) => let val  result = MlyValue.EXP (fn _ => let val  (IDS as IDS1) = IDS1 ()
 val  APPEXP1 = APPEXP1 ()
 val  (REL as REL1) = REL1 ()
 val  APPEXP2 = APPEXP2 ()
 in ((*#line 120.43 "ruby.yacc"*)A.Rel(REL,IDS,APPEXP1,APPEXP2)(*#line 661.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 3, ( result, VAR1left, APPEXP2right), rest671)
end
|  ( 22, ( ( _, ( MlyValue.CON CON1, CON1left, CON1right)) :: rest671)) => let val  result = MlyValue.SEXP (fn _ => let val  (CON as CON1) = CON1 ()
 in ((*#line 122.50 "ruby.yacc"*)A.Const(CON)(*#line 670.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, CON1left, CON1right), rest671)
end
|  ( 23, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.SEXP (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 123.50 "ruby.yacc"*)A.Var(ID)(*#line 676.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, ID1left, ID1right), rest671)
end
|  ( 24, ( ( _, ( MlyValue.LIST LIST1, LIST1left, LIST1right)) :: rest671)) => let val  result = MlyValue.SEXP (fn _ => let val  (LIST as LIST1) = LIST1 ()
 in ((*#line 124.50 "ruby.yacc"*)A.Par(LIST)(*#line 682.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, LIST1left, LIST1right), rest671)
end
|  ( 25, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.SEXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 125.50 "ruby.yacc"*)EXP(*#line 688.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 4, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 26, ( ( _, ( MlyValue.SEXP SEXP1, SEXP1left, SEXP1right)) :: rest671)) => let val  result = MlyValue.SEXPS (fn _ => let val  (SEXP as SEXP1) = SEXP1 ()
 in ((*#line 127.50 "ruby.yacc"*)[SEXP](*#line 694.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, SEXP1left, SEXP1right), rest671)
end
|  ( 27, ( ( _, ( MlyValue.SEXPS SEXPS1, _, SEXPS1right)) :: ( _, ( MlyValue.SEXP SEXP1, SEXP1left, _)) :: rest671)) => let val  result = MlyValue.SEXPS (fn _ => let val  (SEXP as SEXP1) = SEXP1 ()
 val  (SEXPS as SEXPS1) = SEXPS1 ()
 in ((*#line 128.50 "ruby.yacc"*)SEXP::SEXPS(*#line 700.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 5, ( result, SEXP1left, SEXPS1right), rest671)
end
|  ( 28, ( ( _, ( MlyValue.EXP EXP1, _, EXP1right)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.LETEXPS (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 in ((*#line 130.50 "ruby.yacc"*)[(ID,EXP)](*#line 707.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, ID1left, EXP1right), rest671)
end
|  ( 29, ( ( _, ( MlyValue.LETEXPS LETEXPS1, _, LETEXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: _ :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.LETEXPS (fn _ => let val  (ID as ID1) = ID1 ()
 val  (EXP as EXP1) = EXP1 ()
 val  (LETEXPS as LETEXPS1) = LETEXPS1 ()
 in ((*#line 131.50 "ruby.yacc"*)(ID,EXP)::LETEXPS(*#line 714.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 6, ( result, ID1left, LETEXPS1right), rest671)
end
|  ( 30, ( ( _, ( MlyValue.BOOLEAN BOOLEAN1, BOOLEAN1left, BOOLEAN1right)) :: rest671)) => let val  result = MlyValue.CON (fn _ => let val  (BOOLEAN as BOOLEAN1) = BOOLEAN1 ()
 in ((*#line 133.50 "ruby.yacc"*)A.BOOL(BOOLEAN)(*#line 722.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 7, ( result, BOOLEAN1left, BOOLEAN1right), rest671)
end
|  ( 31, ( ( _, ( MlyValue.INTEGER INTEGER1, INTEGER1left, INTEGER1right)) :: rest671)) => let val  result = MlyValue.CON (fn _ => let val  (INTEGER as INTEGER1) = INTEGER1 ()
 in ((*#line 134.50 "ruby.yacc"*)A.INT(INTEGER)(*#line 728.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 7, ( result, INTEGER1left, INTEGER1right), rest671)
end
|  ( 32, ( ( _, ( MlyValue.FLOAT FLOAT1, FLOAT1left, FLOAT1right)) :: rest671)) => let val  result = MlyValue.CON (fn _ => let val  (FLOAT as FLOAT1) = FLOAT1 ()
 in ((*#line 135.50 "ruby.yacc"*)A.REAL(FLOAT)(*#line 734.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 7, ( result, FLOAT1left, FLOAT1right), rest671)
end
|  ( 33, ( ( _, ( MlyValue.SYMBOL SYMBOL1, SYMBOL1left, SYMBOL1right)) :: rest671)) => let val  result = MlyValue.CON (fn _ => let val  (SYMBOL as SYMBOL1) = SYMBOL1 ()
 in ((*#line 136.50 "ruby.yacc"*)A.SYM(SYMBOL)(*#line 740.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 7, ( result, SYMBOL1left, SYMBOL1right), rest671)
end
|  ( 34, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.IO (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 138.50 "ruby.yacc"*)A.VAR(ID)(*#line 746.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 13, ( result, ID1left, ID1right), rest671)
end
|  ( 35, ( ( _, ( MlyValue.CON CON1, CON1left, CON1right)) :: rest671)) => let val  result = MlyValue.IO (fn _ => let val  (CON as CON1) = CON1 ()
 in ((*#line 139.50 "ruby.yacc"*)A.CONST(CON)(*#line 752.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 13, ( result, CON1left, CON1right), rest671)
end
|  ( 36, ( ( _, ( MlyValue.TUPLE TUPLE1, TUPLE1left, TUPLE1right)) :: rest671)) => let val  result = MlyValue.IO (fn _ => let val  (TUPLE as TUPLE1) = TUPLE1 ()
 in ((*#line 140.50 "ruby.yacc"*)A.PROD(TUPLE)(*#line 758.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 13, ( result, TUPLE1left, TUPLE1right), rest671)
end
|  ( 37, ( ( _, ( _, _, RANGLE1right)) :: ( _, ( _, LANGLE1left, _)) :: rest671)) => let val  result = MlyValue.TUPLE (fn _ => ((*#line 142.50 "ruby.yacc"*)[](*#line 764.1 "ruby.yacc.sml"*)
))
 in ( LrTable.NT 8, ( result, LANGLE1left, RANGLE1right), rest671)
end
|  ( 38, ( ( _, ( _, _, RANGLE1right)) :: ( _, ( MlyValue.IOS IOS1, _, _)) :: ( _, ( _, LANGLE1left, _)) :: rest671)) => let val  result = MlyValue.TUPLE (fn _ => let val  (IOS as IOS1) = IOS1 ()
 in ((*#line 143.50 "ruby.yacc"*)IOS(*#line 768.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 8, ( result, LANGLE1left, RANGLE1right), rest671)
end
|  ( 39, ( ( _, ( _, _, RBKT1right)) :: ( _, ( _, LBKT1left, _)) :: rest671)) => let val  result = MlyValue.LIST (fn _ => ((*#line 145.50 "ruby.yacc"*)[](*#line 774.1 "ruby.yacc.sml"*)
))
 in ( LrTable.NT 9, ( result, LBKT1left, RBKT1right), rest671)
end
|  ( 40, ( ( _, ( _, _, RBKT1right)) :: ( _, ( MlyValue.EXPS EXPS1, _, _)) :: ( _, ( _, LBKT1left, _)) :: rest671)) => let val  result = MlyValue.LIST (fn _ => let val  (EXPS as EXPS1) = EXPS1 ()
 in ((*#line 146.50 "ruby.yacc"*)EXPS(*#line 778.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 9, ( result, LBKT1left, RBKT1right), rest671)
end
|  ( 41, ( ( _, ( MlyValue.EXP EXP1, EXP1left, EXP1right)) :: rest671)) => let val  result = MlyValue.EXPS (fn _ => let val  (EXP as EXP1) = EXP1 ()
 in ((*#line 148.50 "ruby.yacc"*)[EXP](*#line 784.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 10, ( result, EXP1left, EXP1right), rest671)
end
|  ( 42, ( ( _, ( MlyValue.EXPS EXPS1, _, EXPS1right)) :: _ :: ( _, ( MlyValue.EXP EXP1, EXP1left, _)) :: rest671)) => let val  result = MlyValue.EXPS (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (EXPS as EXPS1) = EXPS1 ()
 in ((*#line 149.50 "ruby.yacc"*)EXP::EXPS(*#line 790.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 10, ( result, EXP1left, EXPS1right), rest671)
end
|  ( 43, ( ( _, ( MlyValue.IO IO1, IO1left, IO1right)) :: rest671)) => let val  result = MlyValue.IOS (fn _ => let val  (IO as IO1) = IO1 ()
 in ((*#line 151.50 "ruby.yacc"*)[IO](*#line 797.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 11, ( result, IO1left, IO1right), rest671)
end
|  ( 44, ( ( _, ( MlyValue.IOS IOS1, _, IOS1right)) :: _ :: ( _, ( MlyValue.IO IO1, IO1left, _)) :: rest671)) => let val  result = MlyValue.IOS (fn _ => let val  (IO as IO1) = IO1 ()
 val  (IOS as IOS1) = IOS1 ()
 in ((*#line 152.50 "ruby.yacc"*)IO::IOS(*#line 803.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 11, ( result, IO1left, IOS1right), rest671)
end
|  ( 45, ( rest671)) => let val  result = MlyValue.IDS (fn _ => ((*#line 154.50 "ruby.yacc"*)[](*#line 810.1 "ruby.yacc.sml"*)
))
 in ( LrTable.NT 14, ( result, defaultPos, defaultPos), rest671)
end
|  ( 46, ( ( _, ( MlyValue.IDS IDS1, _, IDS1right)) :: ( _, ( MlyValue.ID ID1, ID1left, _)) :: rest671)) => let val  result = MlyValue.IDS (fn _ => let val  (ID as ID1) = ID1 ()
 val  (IDS as IDS1) = IDS1 ()
 in ((*#line 155.50 "ruby.yacc"*)ID::IDS(*#line 814.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 14, ( result, ID1left, IDS1right), rest671)
end
|  ( 47, ( ( _, ( MlyValue.ID ID1, ID1left, ID1right)) :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  (ID as ID1) = ID1 ()
 in ((*#line 157.50 "ruby.yacc"*)A.aVAR(ID)(*#line 821.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 16, ( result, ID1left, ID1right), rest671)
end
|  ( 48, ( ( _, ( MlyValue.CON CON1, CON1left, CON1right)) :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  (CON as CON1) = CON1 ()
 in ((*#line 158.22 "ruby.yacc"*)A.aCONST(CON)(*#line 827.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 16, ( result, CON1left, CON1right), rest671)
end
|  ( 49, ( ( _, ( MlyValue.APPTUPLE APPTUPLE1, APPTUPLE1left, APPTUPLE1right)) :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  (APPTUPLE as APPTUPLE1) = APPTUPLE1 ()
 in ((*#line 159.50 "ruby.yacc"*)A.aPROD(APPTUPLE)(*#line 833.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 16, ( result, APPTUPLE1left, APPTUPLE1right), rest671)
end
|  ( 50, ( ( _, ( MlyValue.APPEXP APPEXP1, _, APPEXP1right)) :: _ :: ( _, ( MlyValue.EXP EXP1, _, _)) :: ( _, ( _, QUOTE1left, _)) :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  (EXP as EXP1) = EXP1 ()
 val  (APPEXP as APPEXP1) = APPEXP1 ()
 in ((*#line 160.50 "ruby.yacc"*)A.aAPP(EXP,APPEXP)(*#line 839.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 16, ( result, QUOTE1left, APPEXP1right), rest671)
end
|  ( 51, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.APPEXP APPEXP2, _, _)) :: ( _, ( MlyValue.INFIX INFIX1, _, _)) :: ( _, ( MlyValue.APPEXP APPEXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  APPEXP1 = APPEXP1 ()
 val  (INFIX as INFIX1) = INFIX1 ()
 val  APPEXP2 = APPEXP2 ()
 in ((*#line 162.16 "ruby.yacc"*)buildBinaryApp(implode(tl(explode(INFIX))),APPEXP1,APPEXP2)(*#line 846.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 52, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.APPEXP APPEXP2, _, _)) :: ( _, ( MlyValue.SUM SUM1, _, _)) :: ( _, ( MlyValue.APPEXP APPEXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  APPEXP1 = APPEXP1 ()
 val  (SUM as SUM1) = SUM1 ()
 val  APPEXP2 = APPEXP2 ()
 in ((*#line 165.16 "ruby.yacc"*)buildBinaryApp(operator2Name(SUM),APPEXP1,APPEXP2)(*#line 854.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 53, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.APPEXP APPEXP2, _, _)) :: ( _, ( MlyValue.PRODUCT PRODUCT1, _, _)) :: ( _, ( MlyValue.APPEXP APPEXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  APPEXP1 = APPEXP1 ()
 val  (PRODUCT as PRODUCT1) = PRODUCT1 ()
 val  APPEXP2 = APPEXP2 ()
 in ((*#line 168.16 "ruby.yacc"*)buildBinaryApp(operator2Name(PRODUCT),APPEXP1,APPEXP2)(*#line 862.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 54, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.APPEXP APPEXP1, _, _)) :: ( _, ( MlyValue.NEG NEG1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  (NEG as NEG1) = NEG1 ()
 val  (APPEXP as APPEXP1) = APPEXP1 ()
 in ((*#line 171.16 "ruby.yacc"*)buildUnaryApp(operator2Name(NEG),APPEXP)(*#line 870.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 55, ( ( _, ( _, _, RPAREN1right)) :: ( _, ( MlyValue.APPEXP APPEXP1, _, _)) :: ( _, ( _, LPAREN1left, _)) :: rest671)) => let val  result = MlyValue.APPEXP (fn _ => let val  (APPEXP as APPEXP1) = APPEXP1 ()
 in ((*#line 173.50 "ruby.yacc"*)APPEXP(*#line 877.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 16, ( result, LPAREN1left, RPAREN1right), rest671)
end
|  ( 56, ( ( _, ( _, _, RANGLE1right)) :: ( _, ( _, LANGLE1left, _)) :: rest671)) => let val  result = MlyValue.APPTUPLE (fn _ => ((*#line 175.50 "ruby.yacc"*)[](*#line 883.1 "ruby.yacc.sml"*)
))
 in ( LrTable.NT 17, ( result, LANGLE1left, RANGLE1right), rest671)
end
|  ( 57, ( ( _, ( _, _, RANGLE1right)) :: ( _, ( MlyValue.APPEXPS APPEXPS1, _, _)) :: ( _, ( _, LANGLE1left, _)) :: rest671)) => let val  result = MlyValue.APPTUPLE (fn _ => let val  (APPEXPS as APPEXPS1) = APPEXPS1 ()
 in ((*#line 176.50 "ruby.yacc"*)APPEXPS(*#line 887.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 17, ( result, LANGLE1left, RANGLE1right), rest671)
end
|  ( 58, ( ( _, ( MlyValue.APPEXP APPEXP1, APPEXP1left, APPEXP1right)) :: rest671)) => let val  result = MlyValue.APPEXPS (fn _ => let val  (APPEXP as APPEXP1) = APPEXP1 ()
 in ((*#line 178.50 "ruby.yacc"*)[APPEXP](*#line 893.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 15, ( result, APPEXP1left, APPEXP1right), rest671)
end
|  ( 59, ( ( _, ( MlyValue.APPEXPS APPEXPS1, _, APPEXPS1right)) :: _ :: ( _, ( MlyValue.APPEXP APPEXP1, APPEXP1left, _)) :: rest671)) => let val  result = MlyValue.APPEXPS (fn _ => let val  (APPEXP as APPEXP1) = APPEXP1 ()
 val  (APPEXPS as APPEXPS1) = APPEXPS1 ()
 in ((*#line 179.43 "ruby.yacc"*)APPEXP::APPEXPS(*#line 899.1 "ruby.yacc.sml"*)
)
end)
 in ( LrTable.NT 15, ( result, APPEXP1left, APPEXPS1right), rest671)
end
| _ => raise (mlyAction i392)
end
val void = MlyValue.VOID
val extract = fn a => (fn MlyValue.START x => x
| _ => let exception ParseInternal
	in raise ParseInternal end) a ()
end
end
structure Tokens : Ruby_TOKENS =
struct
type svalue = ParserData.svalue
type ('a,'b) token = ('a,'b) Token.token
fun RPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 0,(ParserData.MlyValue.VOID,p1,p2))
fun LPAREN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 1,(ParserData.MlyValue.VOID,p1,p2))
fun LANGLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 2,(ParserData.MlyValue.VOID,p1,p2))
fun RANGLE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 3,(ParserData.MlyValue.VOID,p1,p2))
fun EQUAL (p1,p2) = Token.TOKEN (ParserData.LrTable.T 4,(ParserData.MlyValue.VOID,p1,p2))
fun DOT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 5,(ParserData.MlyValue.VOID,p1,p2))
fun COMMA (p1,p2) = Token.TOKEN (ParserData.LrTable.T 6,(ParserData.MlyValue.VOID,p1,p2))
fun WIRE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 7,(ParserData.MlyValue.VOID,p1,p2))
fun INCLUDE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 8,(ParserData.MlyValue.VOID,p1,p2))
fun SELECT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 9,(ParserData.MlyValue.VOID,p1,p2))
fun LBKT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 10,(ParserData.MlyValue.VOID,p1,p2))
fun RBKT (p1,p2) = Token.TOKEN (ParserData.LrTable.T 11,(ParserData.MlyValue.VOID,p1,p2))
fun LET (p1,p2) = Token.TOKEN (ParserData.LrTable.T 12,(ParserData.MlyValue.VOID,p1,p2))
fun IN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 13,(ParserData.MlyValue.VOID,p1,p2))
fun END (p1,p2) = Token.TOKEN (ParserData.LrTable.T 14,(ParserData.MlyValue.VOID,p1,p2))
fun IF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 15,(ParserData.MlyValue.VOID,p1,p2))
fun THEN (p1,p2) = Token.TOKEN (ParserData.LrTable.T 16,(ParserData.MlyValue.VOID,p1,p2))
fun ELSE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 17,(ParserData.MlyValue.VOID,p1,p2))
fun VAR (p1,p2) = Token.TOKEN (ParserData.LrTable.T 18,(ParserData.MlyValue.VOID,p1,p2))
fun REL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 19,(ParserData.MlyValue.REL (fn () => i),p1,p2))
fun SEMICOLON (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 20,(ParserData.MlyValue.SEMICOLON (fn () => i),p1,p2))
fun QUOTE (p1,p2) = Token.TOKEN (ParserData.LrTable.T 21,(ParserData.MlyValue.VOID,p1,p2))
fun NEG (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 22,(ParserData.MlyValue.NEG (fn () => i),p1,p2))
fun SUM (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 23,(ParserData.MlyValue.SUM (fn () => i),p1,p2))
fun PRODUCT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 24,(ParserData.MlyValue.PRODUCT (fn () => i),p1,p2))
fun INFIX (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 25,(ParserData.MlyValue.INFIX (fn () => i),p1,p2))
fun POSTFIX (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 26,(ParserData.MlyValue.POSTFIX (fn () => i),p1,p2))
fun SYMBOL (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 27,(ParserData.MlyValue.SYMBOL (fn () => i),p1,p2))
fun ID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 28,(ParserData.MlyValue.ID (fn () => i),p1,p2))
fun DID (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 29,(ParserData.MlyValue.DID (fn () => i),p1,p2))
fun BOOLEAN (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 30,(ParserData.MlyValue.BOOLEAN (fn () => i),p1,p2))
fun INTEGER (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 31,(ParserData.MlyValue.INTEGER (fn () => i),p1,p2))
fun FLOAT (i,p1,p2) = Token.TOKEN (ParserData.LrTable.T 32,(ParserData.MlyValue.FLOAT (fn () => i),p1,p2))
fun EOF (p1,p2) = Token.TOKEN (ParserData.LrTable.T 33,(ParserData.MlyValue.VOID,p1,p2))
end
end
