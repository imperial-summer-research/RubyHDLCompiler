(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    These functions give an edif 200 implementation of              ***)
(***    pcircuit.                                                       ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Printedf :
  sig
        val showEDF : Circuittype.pcircuit -> string
  end = 
struct

(**************************************************************************)
(**************************************************************************)

fun showExpr e = (case !e of
                    Circuittype.CON k       => [Show.showConst k]
                  | Circuittype.WIRE (Circuittype.POLY,name,_) =>  [Int.toString name]
                                           (*["." ^ Int.toString name]
                                           edif doesn't allow names starting
                                           with "."
                                            use ":"? *)
                  | Circuittype.WIRE (_,   name,_) => [Int.toString name]
                  | Circuittype.LIST es            => (Utilities.flatmap showExpr es)
                  | Circuittype.EXPR x             => showExpr x
                 )

(**************************************************************************)

type portRef 	 = string
type instanceRef = string
type cellRef     = string
type netRef      = string

datatype net_connect = port of portRef
                      |instance of portRef * instanceRef

fun showEDF (dom, ran, rels)
  = let val numAND = ref 0
	val numNOR = ref 0
	val numOR  = ref 0
	val numXOR = ref 0
	val numINV = ref 0
	val numMUX = ref 0
	val numFDC = ref 0
	val numNOT = ref 0
        val numGND = ref 0
        val numONE = ref 0
	fun fstElm (x::xs) = x
        fun sndElm (x::(y::ys)) = y
	fun thdElm (x::(y::(z::zs))) = z
	val portTb = ref ([]: (portRef * string) list)
	fun putIn_portTb x = portTb := x :: (!portTb)
	val instanceTb = ref ([]: (instanceRef * cellRef) list)	
	fun putIn_instanceTb x = instanceTb := x :: (!instanceTb)
        val netTb = ref ([]: (netRef * (net_connect list)ref) list)
        fun putIn_netTb net net_connect = 
            let val NetTb = ! netTb
                fun put_elem y1 y2 [] = 
                      netTb := (net, ref [net_connect]) :: (NetTb)
 	          | put_elem y1 y2 ((z1,z2)::zs) = 
                      if y1 = z1 then z2 := y2 :: (!z2)
                      else put_elem y1 y2 zs
            in if net = "F" then
		let val instanceRef1 = "GND1" ^ Int.toString (!numGND)
		    val net' = net ^  Int.toString (!numGND)
		    val dummy = numGND := (!numGND) +1
	            val instanceRef = (instanceRef1, "GND")
		    val dummy1  = (* if (!numGND) = 1 then *)
                                    putIn_instanceTb instanceRef
                                  (* else () *)
		    val net_connectF = instance ("GROUND", instanceRef1)
		    val dummy4  = (* if (!numGND) = 1 then *)
                                    put_elem net' net_connectF  NetTb
                                  (* else () *)
                 in put_elem net' net_connect NetTb
                 end
               else if net = "T" then
                 let val instanceRef1 = "VCC1" ^ Int.toString (!numONE)
		    val net' = net ^ Int.toString (!numONE)
		    val dummy = numONE := (!numONE) +1
	            val instanceRef = (instanceRef1, "VCC")
		    val dummy1  = (* if (!numONE) = 1 then *)
                                    putIn_instanceTb instanceRef
                                  (* else () *)
		    val net_connectF = instance ("VCC", instanceRef1)
		    val dummy4  = (* if (!numONE) = 1 then *)
                                    put_elem net' net_connectF  NetTb
                                  (* else () *)
                 in put_elem net' net_connect NetTb
                 end
               else put_elem net net_connect NetTb
             end

        fun showPorts e = 
             case (!e) of
                    (Circuittype.CON k) => 
                        ( putIn_netTb (Show.showConst k) (port (Show.showConst k));
                          putIn_portTb (Show.showConst k, "OUTPUT"))
        | (Circuittype.WIRE (Circuittype.IN,name,_))=> 
                        ( putIn_netTb (Int.toString (name:int))
                            (port (Int.toString (name:int))) ;   
                        putIn_portTb (Int.toString (name:int), "INPUT"))
        | (Circuittype.WIRE (Circuittype.OUT,name,_)) => 
                        (putIn_netTb (Int.toString (name:int))
                            (port (Int.toString (name:int))) ; 
                         putIn_portTb (Int.toString (name:int),"OUTPUT"))
                        
        | (Circuittype.LIST es)         => (map showPorts es; ())
        | (Circuittype.EXPR x)          => showPorts x

	val dummy1 = showPorts dom
	val dummy2 = showPorts ran
    
        fun showGate (device, input, output) = 
         let val inputNets  = showExpr input
 	     val outputNets = showExpr output
          fun processGate Circuittype.D = 
		let val instanceRef1 = "FDC" ^ Int.toString (!numFDC)
		    val dummy = numFDC := (!numFDC) +1
		    val instanceRef  = (instanceRef1,"FDC") 
		    val dummy1  = putIn_instanceTb instanceRef 
                    val dummy2 = if (!numFDC) = 2 then ()
				 else putIn_portTb ("CLKIN", "INPUT")(*port*)
		    val net_connect1 = instance ("C",instanceRef1)
		    val dummy3 = putIn_netTb "tclk" net_connect1 
		    val net_connect2 = port ("CLKIN")
		    val dummy4 = putIn_netTb "tclk" net_connect2 (*clock*)
	            val netIn = fstElm inputNets
	            val net_connect3 = instance ("D",instanceRef1)
		    val dummy5 = putIn_netTb netIn net_connect3  (*input*)
		    val netOut = fstElm outputNets
		    val net_connect4 = instance ("Q",instanceRef1)
		    val dummy6 = putIn_netTb netOut net_connect4 (*output*)
	         in () end
             | processGate (Circuittype.DI k) = processGate Circuittype.D
	     | processGate (Circuittype.MUX 2)= 
	        let val instanceRef1 = "MUX" ^ Int.toString (!numMUX)
		    val dummy = numMUX := (!numMUX) +1
		    val instanceRef = (instanceRef1, "M2_1")
		    val dummy1  = putIn_instanceTb instanceRef 
	            val netIn1  = fstElm inputNets
		    val net_connect1 = instance ("D0", instanceRef1)
	            val dummy2  = putIn_netTb netIn1 net_connect1
		    val netIn2  = sndElm inputNets
		    val net_connect2 = instance ("D1", instanceRef1)
		    val dummy3  = putIn_netTb netIn2 net_connect2
                    val netIn3  = thdElm inputNets
		    val net_connect3 = instance ("S0", instanceRef1)
	            val dummy4  = putIn_netTb netIn3 net_connect3
                    val netOut  = fstElm outputNets
		    val net_connect4 = instance ("O", instanceRef1)
		    val dummy5  = putIn_netTb netOut net_connect4
                  in () end
	     | processGate (Circuittype.AND)  = 
		let val instanceRef1 = "AND" ^ Int.toString (!numAND)
		    val dummy = numAND := (!numAND) +1
	            val instanceRef = (instanceRef1, "AND2")
		    val dummy1  = putIn_instanceTb instanceRef
		    val netIn1  = fstElm inputNets
		    val net_connect1 = instance ("I0",instanceRef1)
		    val dummy2  = putIn_netTb netIn1 net_connect1
		    val netIn2  = sndElm inputNets
		    val net_connect2 = instance ("I1", instanceRef1)
		    val dummy3  = putIn_netTb netIn2 net_connect2
                    val netOut  = fstElm outputNets
		    val net_connect3 = instance ("O", instanceRef1)
		    val dummy4  = putIn_netTb netOut net_connect3
		in () end
	     | processGate (Circuittype.NOR)  = 
		let val instanceRef1 = "NOR" ^ Int.toString (!numNOR)
		    val dummy = numNOR := (!numNOR) +1
	            val instanceRef = (instanceRef1, "NOR2")
		    val dummy1  = putIn_instanceTb instanceRef
		    val netIn1  = fstElm inputNets
		    val net_connect1 = instance ("I0",instanceRef1)
		    val dummy2  = putIn_netTb netIn1 net_connect1
		    val netIn2  = sndElm inputNets
		    val net_connect2 = instance ("I1", instanceRef1)
		    val dummy3  = putIn_netTb netIn2 net_connect2
                    val netOut  = fstElm outputNets
		    val net_connect3 = instance ("O", instanceRef1)
		    val dummy4  = putIn_netTb netOut net_connect3
		in () end
	    | processGate (Circuittype.OR)  = 
		let val instanceRef1 = "OR" ^ Int.toString (!numOR)
		    val dummy = numOR := (!numOR) +1
	            val instanceRef = (instanceRef1, "OR2")
		    val dummy1  = putIn_instanceTb instanceRef
		    val netIn1  = fstElm inputNets
		    val net_connect1 = instance ("I0",instanceRef1)
		    val dummy2  = putIn_netTb netIn1 net_connect1
		    val netIn2  = sndElm inputNets
		    val net_connect2 = instance ("I1", instanceRef1)
		    val dummy3  = putIn_netTb netIn2 net_connect2
                    val netOut  = fstElm outputNets
		    val net_connect3 = instance ("O", instanceRef1)
		    val dummy4  = putIn_netTb netOut net_connect3
		in () end  
            | processGate (Circuittype.XOR)  = 
		let val instanceRef1 = "XOR" ^ Int.toString (!numXOR)
		    val dummy = numXOR := (!numXOR) +1
	            val instanceRef = (instanceRef1, "XOR2")
		    val dummy1  = putIn_instanceTb instanceRef
		    val netIn1  = fstElm inputNets
		    val net_connect1 = instance ("I0",instanceRef1)
		    val dummy2  = putIn_netTb netIn1 net_connect1
		    val netIn2  = sndElm inputNets
		    val net_connect2 = instance ("I1", instanceRef1)
		    val dummy3  = putIn_netTb netIn2 net_connect2
                    val netOut  = fstElm outputNets
		    val net_connect3 = instance ("O", instanceRef1)
		    val dummy4  = putIn_netTb netOut net_connect3
		in () end
	    | processGate (Circuittype.NOT) =
		let val instanceRef1 = "INV" ^ Int.toString (!numNOT)  
		    val dummy = numNOT := (!numNOT) +1
		    val instanceRef = (instanceRef1, "INV")
		    val dummy1  = putIn_instanceTb instanceRef
		    val netIn   = fstElm inputNets
		    val net_connect1 = instance ("I", instanceRef1)
		    val dummy2  = putIn_netTb netIn net_connect1
		    val netOut  = fstElm outputNets
	            val net_connect2 = instance ("O", instanceRef1)
	            val dummy3  = putIn_netTb netOut net_connect2
		in () end
        in processGate device end

	val dummy = map (map showGate) rels
	
	fun print_intfs xs = 
          let fun  print_intf (x1,x2) 
                = "      (port " ^ x1 ^ " (direction " ^ x2 ^ "))\n"
          in 
                  (Utilities.concatWith "" (map print_intf xs))
          end
        fun print_instances xs = 
          let fun print_instance (x1,x2) 
                = "(instance "^x1^" (viewRef view_1 (cellRef " ^ x2 ^ ")))\n"
          in (Utilities.concatWith "" (map print_instance xs))
	  end
        fun print_nets xs = 
          let fun print_net (x1, x2) = 
            let fun print_Net []    = ""
	          | print_Net (y::ys) = case y of 
		    (port portRef) => "     (portref " ^ portRef ^")\n" ^
                                     print_Net ys
                  |  (instance(portRef,instanceRef))
                                  =>  "     (portref " ^ portRef ^ 
                                     " (instanceref " ^ instanceRef ^ "))\n" ^
                                     print_Net ys
            in " (net " ^ x1 ^ "\n" ^ 
               "   (joined\n" ^
               print_Net (!x2) ^ "))\n"
            end
          in Utilities.concatWith "" (map print_net xs) 
          end

      in "(edif current\n" ^
         "  (edifVersion 2 0 0)\n" ^
         "  (edifLevel 0)\n" ^
         "  (keywordMap (keywordLevel 0))\n" ^
         "  (status\n" ^
         "    (written\n" ^
         "      (program \"Ruby EDIF translator\" (version \"0.0\"))))\n" ^
         "  (library current \n" ^ 
         "    (edifLevel 0)\n" ^ Library.lib ^
         "(cell current\n" ^
         "(cellType GENERIC)\n" ^
         "(view view_1\n" ^ 
         "  (viewType NETLIST)\n" ^
         "  (interface \n"  ^
          print_intfs (!portTb) ^ 
         ")\n" ^
         "(contents\n" ^ 
          print_instances (!instanceTb)^
          print_nets (!netTb) ^ "))\n"^
          "(design ROOT\n" ^
          "(cellRef current\n" ^
          "(libraryRef current)))\n" ^
         ")\n"
      end
         
        
(**************************************************************************)
(**************************************************************************)

end (* of structure Printedf *);
(* open Printedf *)










