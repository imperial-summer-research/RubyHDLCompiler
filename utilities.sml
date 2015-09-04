(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    A small library of utility functions:                           ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)


structure Utilities :
  sig
        val flatmap             : ('a -> 'b list) -> 'a list -> 'b list
        val concat              : 'a list list -> 'a list
        val sum                 : int list -> int
        val listmax             : int list -> int
        val zip                 : ('a list*'b list) -> ('a*'b) list
        val unzip               : ('a*'b) list -> ('a list*'b list)
        val for                 : (int*int) -> (int->'a) -> 'a list
        val pairWithIntsFrom    : int -> 'a list -> ('a*int) list
        val occurrences         : ''a -> ''a list -> int
        val elem                : ''a -> ''a list -> bool
        val notElem             : ''a -> ''a list -> bool
        val remdups             : ''a list -> ''a list
        val orderedRemdups      : ''a list -> ''a list
        val remduprefs          : ''a ref list -> ''a ref list
        val orderedRemduprefs   : ''a ref list -> ''a ref list
		val tjt97remDupRefs     : ''a ref list -> ''a ref list
        val take                : (int*'a list) -> 'a list
        val drop                : (int*'a list) -> 'a list
        val takewhile           : ('a -> bool) -> 'a list -> 'a list
        val dropwhile           : ('a -> bool) -> 'a list -> 'a list
        val takeuntil           : ('a -> bool) -> 'a list -> 'a list
        val filter              : ('a -> bool) -> 'a list -> 'a list
        val rept                : int -> string -> string
        val concatWith          : string -> string list -> string
        val ljustify            : int -> string -> string
        val rjustify            : int -> string -> string
        val fileContents        : string -> string
        val array2list          : 'a array -> 'a list

		(* tjt: add some set functions: set implemented as list 
		 * should really be impl'd as opaque type, but no time for that
		 *)
		val insertSet           : ''a list -> ''a -> ''a list
		val existsSet           : ''a list -> ''a -> bool

		(* tjt: amazingly, these are not standard in SML *)
		val fst                 : ('a * 'b) -> 'a
		val snd                 : ('a * 'b) -> 'b
  end =
struct

(**************************************************************************)
(**************************************************************************)


fun flatmap f []      = []
  | flatmap f (x::xs) = f x @ flatmap f xs

fun concat []        = []
  | concat (xs::xss) = xs @ concat xss

(* tjt97 - use Int.max instead of max*)
fun listmax [] = 0
  | listmax (x::xs) = Int.max (x, listmax xs)

fun sum [] = 0
  | sum (x::xs) = (x:int) + sum xs

fun zip ([],_)        = []
  | zip (_,[])        = []
  | zip (x::xs,y::ys) = (x,y) :: zip (xs,ys)

fun unzip [] = ([],[])
  | unzip ((x,y)::xyss) = let  val (xs,ys) = unzip xyss
                          in   (x::xs, y::ys)
                          end

fun for (i,j) f = if    i>j
                  then  []
                  else  f i :: for (i+1,j) f

fun pairWithIntsFrom n []      = []
  | pairWithIntsFrom n (x::xs) = (x,n) :: pairWithIntsFrom (n+1) xs

fun occurrences y []      = 0
  | occurrences y (x::xs) = if    y=x
                            then  1 + occurrences y xs
                            else  occurrences y xs

fun elem y []      = false
  | elem y (x::xs) = y=x orelse elem y xs

fun notElem y [] = true
  | notElem y (x::xs) = x<>y andalso notElem y xs

fun remdups []      = []
  | remdups (x::xs) = if    elem x xs
                      then  remdups xs
                      else  x::remdups xs

(* tjt97 - replace rhs (was rev o remdups o rev) with this lambda expression
 * seeL http://www.cs.fiu.edu/~smithg/cop4555/valrestr.html
 *)
val orderedRemdups = (fn xs => (rev o remdups o rev) xs)

(**************************************************************************)
(***                                                                    ***)
(***    given a list of references, return a list in which the objects  ***)
(***    referenced are not duplicated:                                  ***)
(***                                                                    ***)
(**************************************************************************)

(* tjt97: exists => List.exists *)
(* tjt: DOES NOT WORK *)
fun remduprefs []      = []
  | remduprefs (x::xs) = let  val xs' = remduprefs xs
                         in   if    List.exists (fn y=>(!y)=(!x)) xs'
                              then  xs'
                              else  x :: xs'
                         end

(* tjt97: code from http://www.cs.hofstra.edu/~cscccl/www_docs/c415/sample.sml *)
(* deleting all occurences of an element from a list *)
fun delete A nil = nil 
  | delete A (B::R) = if (A=B) then (delete A R) else (B::(delete A R));

fun remDups nil = nil     	
  | remDups (A::R) = (A::(remDups (delete A R)));
						 
(* tjt97 - above funcs with references 
           should be in order?
*)
fun deleteRefs A nil = nil 
  | deleteRefs A l = if ((!A)=(!(hd l))) then (deleteRefs A (tl l)) else ((hd l)::(deleteRefs A (tl l)));
  
fun tjt97remDupRefs nil = nil     	
  | tjt97remDupRefs l = ((hd l)::(tjt97remDupRefs (deleteRefs (hd l) (tl l))));
						 
(* tjt97: replace as  orderedRemdups *)
val orderedRemduprefs = (fn xs => (rev o remduprefs o rev) xs)

fun take (0, _)     = []
  | take (_, [])    = []
  | take (n, x::xs) = x :: take (n-1, xs)

fun drop (0, xs)    = xs
  | drop (_, [])    = []
  | drop (n, x::xs) = drop (n-1, xs)

fun takewhile p []      = []
  | takewhile p (x::xs) = if    p x
                          then  x :: takewhile p xs
                          else  []

fun dropwhile p []      = []
  | dropwhile p (x::xs) = if    p x
                          then  dropwhile p xs
                          else  x::xs

fun takeuntil p []      = []
  | takeuntil p (x::xs) = if    p x
                          then  [x]
                          else  x :: takeuntil p xs

fun filter p []      = []
  | filter p (x::xs) = if    p x
                       then  x :: filter p xs
                       else  filter p xs

fun concatWith x []      = ""
  | concatWith x [s]     = s
  | concatWith x (s::ss) = s ^ x ^ concatWith x ss

fun rept n a = if    n>0
               then  a ^ rept (n-1) a
               else  ""

fun ljustify n xs = xs ^ (rept (n - (size xs)) " ")
fun rjustify n xs = (rept (n - (size xs)) " ") ^ xs


(*tjt97 
 * add TextIO. , standard function names now use CamelCase
 * use valOf to get at result of inputLine (string option)
 * assume that inputLine cannot return NULL because have previously
 * checked endOfStream
 *)
fun fileContents f = let  val file =  TextIO.openIn f
                          fun getLines () = if     TextIO.endOfStream file
                                            then  ""
                                            else   valOf (TextIO.inputLine file) ^ getLines ()
                          val contents = getLines ()
                     in   (TextIO.closeIn file;
                           contents)
                     end

fun array2list a = for (0, Array.length a - 1) (fn i => Array.sub (a,i))


(* tjt: sets implemented as lists *)


(* add value iff not in set l *)
fun insertSet  l value =
	if List.exists (fn x => x = value) l
	then l
	else value::l


(* true iff value in set l *)
fun existsSet  l value =
	List.exists (fn x => x = value) l


(* tjt: Haskell-style fst and snd *)
fun fst (a, b) = a

fun snd (a, b) = b


(**************************************************************************)
(**************************************************************************)

end (* of structure Utilities *);
(* open Utilities *)
