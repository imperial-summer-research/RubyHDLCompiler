(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Error-raising routines:                                         ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Errors:
  sig
        exception DEF_ERROR    of string
        exception SIMPLE_ERROR of string
        exception SEQ_ERROR    of string
        exception USAGE_ERROR  of string
        exception LEX_ERROR    of string
        exception PARSE_ERROR  of string
        exception LOOP_ERROR   of string list

        val TextPosition    : (int*int) ref

        val def_error       : string -> 'a
        val simple_error    : string -> 'a
        val seq_error       : string -> 'a
        val usage_error     : string -> 'a
        val lex_error       : ((int*int)*string) -> 'a
        val parse_error     : (string*(int*int)*(int*int)) -> 'a
        val loop_error      : string list -> 'a
  end =
struct

(**************************************************************************)
(**************************************************************************)

val TextPosition = ref (0,0);

(**************************************************************************)
        
exception DEF_ERROR of string;
exception SIMPLE_ERROR of string;
exception SEQ_ERROR of string;
exception USAGE_ERROR of string;
exception LEX_ERROR of string;
exception PARSE_ERROR of string;
exception LOOP_ERROR of string list;

(**************************************************************************)
(**************************************************************************)
(***    An error within a function:                                     ***)

fun def_error s = raise DEF_ERROR s;

(**************************************************************************)
(***    An error without a particular location:                         ***)

fun simple_error s = raise SIMPLE_ERROR s;

(**************************************************************************)
(***    A sequential composition error at a given location:             ***)

fun seq_error s = raise SEQ_ERROR s;

(**************************************************************************)
(***    A command-line error:                                           ***)

fun usage_error s = raise USAGE_ERROR s;

(**************************************************************************)
(***    A lexical parsing error:                                        ***)

fun lex_error (pos,s) = (TextPosition := pos;
                         raise LEX_ERROR s);

(**************************************************************************)
(***    A syntax error:                                                 ***)

fun parse_error (s, lpos, rpos) = (TextPosition := lpos;
                                   raise LEX_ERROR s);

(**************************************************************************)
(***    A loop in the resulting circuit:                                ***)

fun loop_error ds = raise LOOP_ERROR ds;

(**************************************************************************)
(**************************************************************************)

end (* of structure Errors *);
(* open Errors *)
