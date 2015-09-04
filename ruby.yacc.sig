signature Ruby_TOKENS =
sig
type ('a,'b) token
type svalue
val EOF:  'a * 'a -> (svalue,'a) token
val FLOAT: (real) *  'a * 'a -> (svalue,'a) token
val INTEGER: (int) *  'a * 'a -> (svalue,'a) token
val BOOLEAN: (bool) *  'a * 'a -> (svalue,'a) token
val DID: (string) *  'a * 'a -> (svalue,'a) token
val ID: (string) *  'a * 'a -> (svalue,'a) token
val SYMBOL: (string) *  'a * 'a -> (svalue,'a) token
val POSTFIX: (string) *  'a * 'a -> (svalue,'a) token
val INFIX: (string) *  'a * 'a -> (svalue,'a) token
val PRODUCT: (string) *  'a * 'a -> (svalue,'a) token
val SUM: (string) *  'a * 'a -> (svalue,'a) token
val NEG: (string) *  'a * 'a -> (svalue,'a) token
val QUOTE:  'a * 'a -> (svalue,'a) token
val SEMICOLON: ( ( int*int ) ) *  'a * 'a -> (svalue,'a) token
val REL: ( ( int*int ) ) *  'a * 'a -> (svalue,'a) token
val VAR:  'a * 'a -> (svalue,'a) token
val ELSE:  'a * 'a -> (svalue,'a) token
val THEN:  'a * 'a -> (svalue,'a) token
val IF:  'a * 'a -> (svalue,'a) token
val END:  'a * 'a -> (svalue,'a) token
val IN:  'a * 'a -> (svalue,'a) token
val LET:  'a * 'a -> (svalue,'a) token
val RBKT:  'a * 'a -> (svalue,'a) token
val LBKT:  'a * 'a -> (svalue,'a) token
val SELECT:  'a * 'a -> (svalue,'a) token
val INCLUDE:  'a * 'a -> (svalue,'a) token
val WIRE:  'a * 'a -> (svalue,'a) token
val COMMA:  'a * 'a -> (svalue,'a) token
val DOT:  'a * 'a -> (svalue,'a) token
val EQUAL:  'a * 'a -> (svalue,'a) token
val RANGLE:  'a * 'a -> (svalue,'a) token
val LANGLE:  'a * 'a -> (svalue,'a) token
val LPAREN:  'a * 'a -> (svalue,'a) token
val RPAREN:  'a * 'a -> (svalue,'a) token
end
signature Ruby_LRVALS=
sig
structure Tokens : Ruby_TOKENS
structure ParserData:PARSER_DATA
sharing type ParserData.Token.token = Tokens.token
sharing type ParserData.svalue = Tokens.svalue
end
