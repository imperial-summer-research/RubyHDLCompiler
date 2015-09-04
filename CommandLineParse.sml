(*** 
 *** Command line parser 
 parses arguments with the grammar:
    clargs = clarg*
	clarg = OPTION "=" ARG         -- option with option-arg
		  | OPTION                 -- option without option-arg
		  | ARG                    -- argument (e.g. filename)
 
 where OPTION is anything starting with '-'
 note option arguments MUST be prefixed with '='
 ARG is anything without spaces, or anything enclosed in quotes
 so  'foo bar' is two args, 'foo' and 'bar'
 but '"foo bar"' is single argument "foo bar"
 strings lexed on their own, allowing args to have spaces
 options and args are stored in assoc-list mapping name -> option-args
*)


(*
 CLArgKindStruct: put this in a separate structure, otherwise modules using
 this type (e.g. printMax) won't be able to see it.

 Ideally, isCLArgOptionArgs should be here too, but there seem to be visibility
 errors.
*)
structure CLArgKindStruct =
	struct
		(* represent command-line arguments *)
		datatype CLArgKind =
			CLArgOptionArgs of (string list)        (* --option [option-args] *)
			| CLArgOptionNoArg                      (* --option *)
			| CLArgPlain                            (* argument *)
	end



(* 
 * main interface to command-line parser 
*)
structure CommandLineParse :
	sig
		(* Tokens type *)
		type Token

		val lexCL : char list -> Token list

		(* Haskell's intersperse function *)
		val intersperse : 'a -> 'a list -> 'a list

		(* functions for assoc-lists *)
		val assocLookup  : ''a -> (''a * 'b) list -> 'b Option.option
	  	val assocReplace : string -> 'b -> (string * 'b) list -> (string * 'b) list
		val assocStore   : string -> 'b -> (string * 'b) list -> (string * 'b) list

		(* represent command-line arguments *)
		(*type CLArgKind*)
		
		(* Haskell-style pair selectors *)
		val fst : ('a * 'b) -> 'a
		val snd : ('a * 'b) -> 'b
		
		(* utilities for filtering lists *) 
		val isCLArgOptionArgs :  CLArgKindStruct.CLArgKind -> bool
		val isCLArgOptionNoArg : CLArgKindStruct.CLArgKind -> bool
		val isCLArgPlain :       CLArgKindStruct.CLArgKind -> bool

		(* definitions type is an assoc-list *)
		type Defs

		(* parse list of tokens into list of defs *)
		val parseCL : (Defs * Token list) -> (Defs * Token list)


	end =
struct


(*remove spaces from string *)
(*not ordering of defs: most specific first, or error results*)
fun sub_sp (#" "::xs) = sub_sp xs
  | sub_sp (x::xs)    = (x::(sub_sp xs))
  | sub_sp []         = []


fun 
    accumTok (#" ", ([[]]))  =  [[]]
  | accumTok (#" ", ([]::bs)) =  ([]::bs)
  | accumTok (#" ", (b::bs)) =  ([]::b::bs)
  (*| accumTok (x,    ([[]])) = ([[x]]) *)
  | accumTok (x,    (b::bs)) = ((x::b)::bs)

(* Haskell's intersperse function *)
fun
    intersperse y [] = []
  | intersperse y [x] = [x]
  | intersperse y (x::xs) = (x::y::(intersperse y xs))


datatype Token = TokenOption of string     (* -x, --foo *)
               | TokenArgument of string   (* foo bar 3 roo.rby *)
               | TokenString of string     (* "foo" *)
               | TokenEq                   (* = *)


(* Haskell-style pair selectors 
   Amazingly, these aren't standard in SML (?)
 *)
fun fst (a, _) = a
fun snd (_, b) = b


(* Haskell-style takeWhile *)
fun
	  takeWhile _ [] = []
	| takeWhile f (x::xs) = 
	  if f x 
	  then (x::(takeWhile f xs)) 
	  else []

(* splitOn: split list into pair of lists:
  (those up to first match, first match + rest)
 *)
fun
	  splitOnHelper x (sofar, []) = (sofar, [])
	| splitOnHelper x (sofar, y::ys) = 
	  if x = y
	  then (sofar, y::ys)
	  else splitOnHelper x (y::sofar, ys)

fun splitOn x ys = 
	let val (fst, snd) = splitOnHelper x ([], ys) 
	in (rev fst, snd)
	end

(* Any: true iff one elem of list matches predicate 
 use de Morgan's theorem: any f l = not(all (not . f) l)
 *)
fun any pred l =
	not (List.all (not o pred) l)

(* splitOnFun: like splitOn, but with function predicate as parameter *)
fun
	  splitOnFunHelper pred (sofar, []) = (sofar, [])
	| splitOnFunHelper pred (sofar, y::ys) = 
	  if pred y
	  then (sofar, y::ys)
	  else splitOnFunHelper pred (y::sofar, ys)

fun splitOnFun pred ys = 
	let val (fst, snd) = splitOnFunHelper pred ([], ys) 
	in (rev fst, snd)
	end

(* isEndOption: true iff char can end option*)
fun isEndOption x =
	(x = #" " orelse x = #"=" orelse x = #"\"")


(* command-line lexer, takes list of char, return list of tokens *)
fun 
	  lexCL ([])     = []
	| lexCL (#" "::xs) = lexCL xs
	| lexCL (#"="::xs) = (TokenEq)::(lexCL xs)
	| lexCL (#"-"::xs) = 
	  let val (str, rest) = splitOnFun isEndOption (#"-"::xs)
	  in (TokenOption (implode str))::(lexCL rest)
	  end
	| lexCL (#"\""::xs) = 
	  let val (str, rest) = splitOn #"\"" xs
	  in 
	  	(TokenString (implode str))
			(* guard against tail of empty list *)
			::lexCL(if (null rest) then rest else (tl rest))
	  end
	| lexCL (x::xs) =
	  let val (str, rest) = splitOn #" " (x::xs)
	  in (TokenArgument (implode str))::(lexCL rest)
	  end



(* declare some exceptions for options symbol table / parsing command line *)
exception ERROR_NOT_FOUND of string   (* not found *)
exception ERROR_DESIGN_ERROR     (*  throw when logic should prevent getting
                                     to that point in code *)
exception ERROR_USER_MIXED_ARG_ERROR   (* user mixing args, e.g --foo --foo=bar *)
exception ERROR_USER_MISPLACED_EQ      (* unmatched '=' on its own *)




(* functions for assoc-lists *)
fun 
	assocLookup key [] = Option.NONE   (* not found *)
	| assocLookup key ((k, v)::rest) =
	  if (key = k)
	  then Option.SOME v               (* found *)
	  else assocLookup key rest        (* recurse to tail *)

fun 
	  assocReplace key value [] = raise ERROR_NOT_FOUND key
	| assocReplace key value ((k, v)::rest) =
	  if key = k
	  then (key, value)::rest
	  else (k, v)::(assocReplace key value rest)

fun assocStore key value l =
	case (assocLookup key l) of
		Option.NONE => ((key, value)::l)
		| Option.SOME _ => (assocReplace key value l)







(* utilities for filtering lists 
   possibly superfluous
*)
fun 
	isCLArgOptionArgs (CLArgKindStruct.CLArgOptionArgs _) = true
	| isCLArgOptionArgs (_) =  false

fun 
	isCLArgOptionNoArg (CLArgKindStruct.CLArgOptionNoArg) = true
	| isCLArgOptionNoArg (_) =  false

fun 
	isCLArgPlain (CLArgKindStruct.CLArgPlain) =   true
	| isCLArgPlain (_) =  false


(* Datatype of command-line definitions *)
type Defs = (string * CLArgKindStruct.CLArgKind) list



(* helpers for parseCL *)
fun storeArgInDefsAndRecurse arg defs rest parseCL =
		(case assocLookup arg defs of
			Option.SOME currVal => (* found => must be a dupe *)
				(case currVal of 
					CLArgKindStruct.CLArgOptionArgs _ =>
					(* ERROR: should not be possible *)
						raise ERROR_DESIGN_ERROR
					| CLArgKindStruct.CLArgOptionNoArg =>
					(* ERROR: should not be possible *)
						raise ERROR_DESIGN_ERROR
					| CLArgKindStruct.CLArgPlain =>
					(* argument is duplicate, but add it anyway *)
						parseCL ( assocStore arg (CLArgKindStruct.CLArgPlain) defs, rest )
				)
			| Option.NONE =>       (* not found => add *)
				parseCL ( assocStore arg (CLArgKindStruct.CLArgPlain) defs, rest )
		)

fun storeOptionAndArgInDefsAndRecurse optionName optionArg defs rest parseCL =
		(case assocLookup optionName defs of
			(* found => add to list *)
			Option.SOME currVal => 
				(case currVal of 
					CLArgKindStruct.CLArgOptionArgs currOptArgs =>
					(* add another option-arg to this option *)
						parseCL (assocReplace optionName (CLArgKindStruct.CLArgOptionArgs (optionArg::currOptArgs)) defs, rest)
					| CLArgKindStruct.CLArgOptionNoArg =>
					(* user error: mixing --foo and --foo=bar *)
						raise ERROR_USER_MIXED_ARG_ERROR
					| CLArgPlain =>  
					(* ERROR: should not be possible *)
						raise ERROR_DESIGN_ERROR
				)
			(* not found => create new option with single arg s *)
			| Option.NONE =>
				parseCL ( (assocStore optionName (CLArgKindStruct.CLArgOptionArgs [optionArg]) defs), rest )
		)


(* parse list of tokens into assoc-list of (string, CLArgKind)
  grammar:
    clargs = clarg*
	clarg = OPTION "=" ARG         -- option with option-arg
	      | OPTION ARG             -- COMMENTED OUT: ambiguous
		  | OPTION                 -- option without option-arg
		  | ARG                    -- argument (e.g. filename)

options are grouped with arguments, so e.g. --foo=bar --bob --foo=baz rob.rby
becomes:
[("--foo", CLOptionArgs ["bar", "baz"],), 
 ("--bar", CLOptionNoArg), 
 ("rob.rby", CLArgPlain)]
*)
fun
	(* base case *)
	  parseCL (defs, []) = (defs, [])
	(* --option=val *)
	| parseCL (defs, (TokenOption name)::(TokenEq)::(TokenArgument v)::rest) =
		storeOptionAndArgInDefsAndRecurse name v defs rest parseCL
	(* --option="some string" *)
	| parseCL (defs, (TokenOption name)::(TokenEq)::(TokenString s)::rest) =
		storeOptionAndArgInDefsAndRecurse name s defs rest parseCL
(*  COMMENTED OUT: do not allow as ambiguous whether it is:
    --foo bar => option foo with option-arg bar
	          OR option foo (no arg) and argument bar
	(* --option val *)
	| parseCL (defs, (TokenOption name)::(TokenArgument v)::rest) =
		(case assocLookup name defs of
			Option.SOME vals => (assocReplace name (v::vals) defs, rest)
			| Option.NONE => (assocStore name [v] defs, rest))
*)
	(* --option *)
	| parseCL (defs, (TokenOption name)::rest) =
		(case assocLookup name defs of
			(* found => *)
			Option.SOME currVal =>
				(case currVal of
					CLArgKindStruct.CLArgOptionArgs _ => 
					(* user error: mixing --foo and --foo=bar *)
						raise ERROR_USER_MIXED_ARG_ERROR
					| CLArgKindStruct.CLArgOptionNoArg =>
						(* do nothing: duplicate option, e.g --foo --foo *)
						parseCL (defs, rest)
					|CLArgKindStruct. CLArgPlain => 
					(* ERROR: should not be possible *)
						raise ERROR_DESIGN_ERROR
				)
			| Option.NONE => 
				parseCL (assocStore name (CLArgKindStruct.CLArgOptionNoArg) defs, rest) 
		) 
(*
*)
	(* arg on own *)
	| parseCL (defs, (TokenArgument name)::rest) =
		storeArgInDefsAndRecurse name defs rest parseCL
	(* string on its own: convert to argument*)
	| parseCL (defs, (TokenString s)::rest) =
		storeArgInDefsAndRecurse s defs rest parseCL
	(* '=' on its own is an error *)
	| parseCL (defs, (TokenEq)::rest) =
		raise ERROR_USER_MISPLACED_EQ      (* unmatched '=' on its own *)

end   (* of structure CommandLineParse *)
