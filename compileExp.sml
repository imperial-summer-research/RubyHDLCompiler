(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Functions for converting ruby structures into values:           ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure CompileExp :
  sig
  		(*tjt, Jan 2014: value -> Values.value*)
        val exp2value : string -> (string->Values.value) -> Rubytype.exp -> Values.value
  end =
struct

(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    exp2value converts an expression in the source language to a    ***)
(***    value given an environment mapping names to values.             ***)
(***                                                                    ***)
(**************************************************************************)

fun exp2value name env (Rubytype.Const d)
        = Values.CONSTANT d

  | exp2value name env (Rubytype.Var x)
        = let  val x' = env x
               val (v,[]) = Values.apply (x',[])
          in   v
          end

  | exp2value name env (Rubytype.Let ([],e))
        = exp2value name env e
  | exp2value name env (Rubytype.Let ((x0,e0)::es,e))
        = let val v0   = exp2value name env e0
              val env0 = Mappings.extendMapping ((x0,v0),env)
          in  exp2value name env0 (Rubytype.Let (es,e))
          end

(**************************************************************************)
(***    In a (pat $wire pat) exp, we find all variables in the two      ***)
(***    patterns, pair each variable with the number of times it        ***)
(***    occurs, create a mapping from these pairs to new polymorphic    ***)
(***    wires (the number of occurrences of each variable gives the     ***)
(***    number of loose ends of the corresponding wire), and use that   ***)
(***    mapping to create the domain and range of the resulting circuit ***)
(***    (which contains no gates):                                      ***)
(**************************************************************************)

  | exp2value name env (Rubytype.Wiring (w1,w2))
        = let fun io2expr env (Rubytype.VAR x)         = env x
                | io2expr env (Rubytype.CONST d)       = Exprs.con d
                | io2expr env (Rubytype.PROD ds)       = Exprs.list (map (io2expr env) ds)
              val xs = Ios.iovars w1 @ Ios.iovars w2
              val ends = map (fn x=>(x, Utilities.occurrences x xs)) (Utilities.remdups xs)
          in  Values.CIRCUIT (fn () => let val pairs = map (fn (x,e)=>(x,Exprs.poly e)) ends
                                    val mapping = Mappings.pairs2mapping pairs
                                    val dom = io2expr mapping w1
                                    val ran = io2expr mapping w2
                                in  (dom, ran, [])               (*no devices*)
                                end
                               )
          end

(**************************************************************************)
(***    CurrentPrim must be set in case an error message is generated:  ***)
(**************************************************************************)

  | exp2value name env (Rubytype.Seq (pos,e1,e2))
        = Values.CIRCUIT (fn () => #2 (State.pushFun name,
                                let  val dummy = Errors.TextPosition := pos
                                     val v1 = exp2value name env e1
                                     val v2 = exp2value name env e2
                                     val dummy = State.CurrentPrim := ";"
                                     val c1 = Values.value2circuit v1
                                     val c2 = Values.value2circuit v2
                                in   Compose.seq (c1, c2)
                                end,
                                State.popFun name)
                  )

  | exp2value name env (Rubytype.Par es)
        = Values.CIRCUIT (fn () => #2 (State.pushFun name,
                                let  val vs = map (exp2value name env) es
                                     val dummy = State.CurrentPrim := "||"
                                     val cs = map Values.value2circuit vs
                                in   Compose.par cs
                                end,
                                State.popFun name)
                  )

  | exp2value name env (Rubytype.Select (e,es))
        = let  val v = exp2value name env e
               val dummy = State.CurrentPrim := "SELECT"
               val n = Values.value2nat v
          in   if    n >= length es
               then  Errors.def_error "not enough elements to select from"
               else  exp2value name env (List.nth (es,n))
          end

  | exp2value name env (Rubytype.App (f,es))
        = let  val vs = map (exp2value name env) es
               val f' = env f
               val (v,leftover) = Values.apply (f',vs)
          in   if    null leftover
               then  v
               else  Errors.def_error ("\""^f^"\" applied to too many arguments")
          end

  | exp2value name env (Rubytype.Rel (pos,vars,a1,a2))
        = let  val id = Rubytype.Wiring (Rubytype.VAR "x", Rubytype.VAR "x")
               fun forget f z = f (z, Rubytype.Wiring (Rubytype.VAR "x", Rubytype.VAR "y"))
               fun separate f (Rubytype.aVAR(v))    =
		   if (Utilities.elem v vars)
                   then (id, Rubytype.VAR (v))
                   else (forget f (Rubytype.Var (v)), Rubytype.CONST (Rubytype.BOOL (false)))
                 | separate f (Rubytype.aCONST(k))  = (id, Rubytype.CONST (k))
                 | separate f (Rubytype.aPROD(aps)) = 
                     let val (funcs, wires) = Utilities.unzip (map (separate f) aps)
                     in  (Rubytype.Par (funcs), Rubytype.PROD (wires))
                     end
		 | separate f (Rubytype.aAPP(e1,a)) =
		     let val (e2, wire)  = separate f a
		     in  (f (e1, e2), wire)
                     end
               val (left, left_wire)   = 
                     separate 
                       (fn (e1, e2) => Rubytype.Seq (pos, Rubytype.App ("^~1", [e1]), e2))
                       a1
               val (right, right_wire) = 
                     separate 
                       (fn (e1, e2) => Rubytype.Seq (pos, e2, e1))
                       a2
	       val wire                = Rubytype.Wiring (left_wire, right_wire)
          in
               exp2value name env (Rubytype.Seq (pos, (Rubytype.Seq (pos, left, wire)), right))
          end

(**************************************************************************)
(**************************************************************************)

end (* of structure CompileExp *);
(* open CompileExp *)
