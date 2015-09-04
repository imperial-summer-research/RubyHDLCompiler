(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    This is the abstract grammar of the Ruby notation:              ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Rubytype =
struct

(**************************************************************************)
(**************************************************************************)

datatype defn   = Fdefn of string * (string list) * exp  (* f x1 .. xn = e . *)
                | Ddefn of string * io                   (* &x = pattern . *)
                | Include of string                      (* INCLUDE x . *)

and exp         = Const  of const
                | Var    of string
                | Wiring of (io * io)                 (* pattern $wire pattern *)
                | Par    of exp list                  (* [ e0, .. en ] *)
                | Seq    of (int*int) * exp * exp     (* (line,col), e0;e1 *)
                | Select of (exp * exp list)          (* SELECT n [e0, .. en] *)
                | App    of string * (exp list)       (* f e0 .. en *)
                | Let    of (string * exp) list * exp (* let x0=e0, x1=e1 in e *)
                | Rel    of (int*int)*(string list)*appexp*appexp   (* applied $rel applied *)

and const       = BOOL of bool          (* literal T or F *)
                | INT  of int           (* literal integer *)
                | REAL of real          (* literal real *)
                | SYM  of string        (* literal symbol *)

and io          = VAR   of string
                | CONST of const
                | PROD  of (io list)    (* <d0,d1..dn> *)

and appexp      = aVAR   of string
                | aCONST of const
		| aPROD  of (appexp list)
                | aAPP   of (exp * appexp)

(**************************************************************************)
(**************************************************************************)

end (* of structure Rubytype *);
(* open Rubytype *)
