(* These incantations tie the lexer and parser together.
   See ML-Yacc manual

   Wayne Luk 9 May 1992 // John W. O'Leary  9 April 1992 

 *)
structure ruby = 
   struct

structure RubyLrVals = RubyLrValsFun(structure Token = LrParser.Token)
structure RubyLex = RubyLexFun(structure Tokens = RubyLrVals.Tokens)
structure RubyParser = Join (structure Lex = RubyLex
                            structure ParserData = RubyLrVals.ParserData
                            structure LrParser = LrParser)

val invoke = fn lexstream => 
(* tjt, Jan 2014 *)
    RubyParser.parse(0,lexstream,Errors.parse_error,())

fun input_line k = fn f:int =>
    let fun loop result =
        let 
            val c = TextIO.inputN (k,1)
            val result = c::result
        in
            if String.size c = 0 orelse c = "\n" then
                String.concat (rev result)
            else loop result
        end
    in
        loop nil
    end

fun parse inp = 
    let val dummy_reset = RubyLex.UserDeclarations.pos := (1,1)
        val k = if inp = "std_in" then TextIO.stdIn else TextIO.openIn(inp)
        val lexer = RubyParser.makeLexer (input_line k)
        val dummyEOF = RubyLrVals.Tokens.EOF((0,0),(0,0))
        fun loop lexer =
            let val (result,lexer) = invoke lexer
                val (nextToken,lexer) = RubyParser.Stream.get lexer
            in
                if RubyParser.sameToken(nextToken,dummyEOF) then 
                        (if inp = "std_in" then () else TextIO.closeIn k ; result)
                else loop lexer
            end
    in
        loop lexer
    end
end;


