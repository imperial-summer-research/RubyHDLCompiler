(* Ruby parser for sml-yacc *)

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

%%

%pos (int * int) 
%name Ruby
%start START
%eop EOF
%noshift EOF

%term     RPAREN | LPAREN | LANGLE | RANGLE
        | EQUAL | DOT | COMMA | WIRE | INCLUDE
        | SELECT | LBKT | RBKT | LET | IN | END
        | IF | THEN | ELSE
	| VAR
        | REL       of (int*int)
        | SEMICOLON of (int*int)
        | QUOTE
        | NEG       of string
        | SUM       of string
        | PRODUCT   of string
        | INFIX     of string
        | POSTFIX   of string
        | SYMBOL    of string
        | ID        of string
        | DID       of string
        | BOOLEAN   of bool
        | INTEGER   of int
        | FLOAT     of real
        | EOF

%nonterm  START of (A.defn list) | PROG of (A.defn list) | DEFN of A.defn
        | EXP of A.exp | SEXP of A.exp | SEXPS of (A.exp list)
        | LETEXPS of ((string * A.exp) list)
        | CON of A.const | TUPLE of (A.io list) | LIST of (A.exp list)
        | EXPS of (A.exp list) | IOS of (A.io list) | RHS of A.exp
        | IO of A.io | IDS of (string list)
        | APPEXPS of (A.appexp list) | APPEXP of (A.appexp)
        | APPTUPLE of (A.appexp list)

%left SEMICOLON
%left INFIX WIRE POSTFIX
%left SUM
%left PRODUCT

%%
START     : PROG                                (PROG)
          |                                     ([])

PROG      : DEFN                                ([DEFN])
          | PROG DEFN                           (DEFN::PROG)

DEFN      : ID IDS EQUAL EXP DOT                (A.Fdefn(ID,IDS,EXP))
          | ID INFIX ID EQUAL EXP DOT           (A.Fdefn(INFIX,[ID1,ID2],EXP))
          | ID POSTFIX EQUAL EXP DOT            (A.Fdefn(POSTFIX,[ID],EXP))
          | INCLUDE SYMBOL DOT                  (A.Include(SYMBOL))
          | DID EQUAL IO DOT                    (A.Ddefn(DID,(brace2bracket IO)))

EXP       : SEXP                                (SEXP)
          | EXP SEMICOLON EXP                   (A.Seq(SEMICOLON,EXP1,EXP2))
          | EXP INFIX EXP                       (A.App(INFIX,[EXP1,EXP2]))
          | EXP SUM EXP                         (A.App(SUM,[EXP1,EXP2]))
          | EXP PRODUCT EXP                     (A.App(PRODUCT,[EXP1,EXP2]))
          | NEG SEXP                            (A.App(NEG,[SEXP]))
          | EXP POSTFIX                         (A.App(POSTFIX,[EXP]))
          | ID SEXPS                            (A.App(ID,SEXPS))
          | IO WIRE IO                          (A.Wiring(IO1,IO2))
          | SELECT SEXP LIST                    (A.Select(SEXP,LIST))
          | IF SEXP THEN SEXP ELSE SEXP         (A.Select(SEXP1,[SEXP3,SEXP2]))
          | LET LETEXPS IN EXP END              (A.Let(LETEXPS,EXP))
	  | VAR IDS DOT APPEXP REL APPEXP       (A.Rel(REL,IDS,APPEXP1,APPEXP2))

SEXP      : CON                                 (A.Const(CON))
          | ID                                  (A.Var(ID))
          | LIST                                (A.Par(LIST))
          | LPAREN EXP RPAREN                   (EXP)

SEXPS     : SEXP                                ([SEXP])
          | SEXP SEXPS                          (SEXP::SEXPS)
          
LETEXPS   : ID EQUAL EXP                        ([(ID,EXP)])
          | ID EQUAL EXP COMMA LETEXPS          ((ID,EXP)::LETEXPS)

CON       : BOOLEAN                             (A.BOOL(BOOLEAN))
          | INTEGER                             (A.INT(INTEGER))
          | FLOAT                               (A.REAL(FLOAT))
          | SYMBOL                              (A.SYM(SYMBOL))

IO        : ID                                  (A.VAR(ID))
          | CON                                 (A.CONST(CON))
          | TUPLE                               (A.PROD(TUPLE))

TUPLE     : LANGLE RANGLE                       ([])
          | LANGLE IOS RANGLE                   (IOS)

LIST      : LBKT RBKT                           ([])
          | LBKT EXPS RBKT                      (EXPS)

EXPS      : EXP                                 ([EXP])
          | EXP COMMA EXPS                      (EXP::EXPS)

IOS       : IO                                  ([IO])
          | IO COMMA IOS                        (IO::IOS)

IDS       :                                     ([])
          | ID IDS                              (ID::IDS)

APPEXP    : ID                                  (A.aVAR(ID))
          | CON					(A.aCONST(CON))
          | APPTUPLE                            (A.aPROD(APPTUPLE))
          | QUOTE EXP QUOTE APPEXP              (A.aAPP(EXP,APPEXP))
          | LPAREN APPEXP INFIX APPEXP RPAREN   
              (buildBinaryApp(implode(tl(explode(INFIX))),APPEXP1,APPEXP2))
			  (*tjt: allow '+' and '-' in appexp *)
          | LPAREN APPEXP SUM APPEXP RPAREN   
              (buildBinaryApp(operator2Name(SUM),APPEXP1,APPEXP2))
			  (*tjt: allow '*' and '/' in appexp *)
          | LPAREN APPEXP PRODUCT APPEXP RPAREN   
              (buildBinaryApp(operator2Name(PRODUCT),APPEXP1,APPEXP2))
			  
          | LPAREN NEG APPEXP RPAREN   
              (buildUnaryApp(operator2Name(NEG),APPEXP))
			  
          | LPAREN APPEXP RPAREN                (APPEXP)

APPTUPLE  : LANGLE RANGLE                       ([])
          | LANGLE APPEXPS RANGLE               (APPEXPS)

APPEXPS   : APPEXP                              ([APPEXP])
          | APPEXP COMMA APPEXPS	        (APPEXP::APPEXPS)
