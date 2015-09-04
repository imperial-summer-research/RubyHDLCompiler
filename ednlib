(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    This is a small edif 200 library                                ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Library :
  sig
        val lib :  string
  end = 
struct

val  lib = 
"   (cell BUF                                                  \n" ^ 
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port I                                             \n" ^
"            (direction INPUT))                                \n" ^
"          (port O                                             \n" ^
"            (direction OUTPUT))                               \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"BUF\") (owner \"Ruby\")) \n" ^
"          (property SCHNM (string \"BUF\") (owner \"Ruby\"))  \n" ^
"      ))                                                      \n" ^  
"    (cell INV                                                 \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port I                                             \n" ^
"           (direction INPUT))                                 \n" ^
"          (port O                                             \n" ^
"            (direction OUTPUT))                               \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"INV\") (owner \"Ruby\")) \n" ^
"          (property SCHNM (string \"INV\") (owner \"Ruby\"))  \n" ^
"      ))                                                      \n" ^
"    (cell AND2                                                \n" ^
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port I0                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port I1                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port O                                             \n" ^
"            (direction OUTPUT))                               \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"AND\") (owner \"Ruby\")) \n" ^
"          (property SCHNM (string \"AND2\") (owner \"Ruby\")) \n" ^
"      ))                                                      \n" ^
"     (cell AND2B1                                             \n" ^
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port I0                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port I1                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port O                                             \n" ^
"            (direction OUTPUT))                               \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"AND\") (owner \"Ruby\")) \n" ^
"          (property SCHNM (string \"AND2B1\") (owner \"Ruby\"))\n" ^
"      ))                                                      \n" ^
"     (cell OR2                                                \n" ^
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port I0                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port I1                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port O                                             \n" ^
"           (direction OUTPUT))                                \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"OR\") (owner \"Ruby\"))  \n" ^
"          (property SCHNM (string \"OR2\") (owner \"Ruby\"))  \n" ^
"      ))                                                      \n" ^
"    (cell OR2B1                                               \n" ^
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port I0                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port I1                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port O                                             \n" ^
"            (direction OUTPUT))                               \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"OR\") (owner \"Ruby\"))  \n" ^
"          (property SCHNM (string \"OR2B1\") (owner \"Ruby\"))\n" ^
"      ))                                                      \n" ^
"                                                              \n" ^
"    (cell XOR2                                                \n" ^
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port I0                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port I1                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port O                                             \n" ^
"            (direction OUTPUT))                               \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"XOR\") (owner \"Ruby\")) \n" ^
"          (property SCHNM (string \"XOR2\") (owner \"Ruby\")) \n" ^
"      ))                                                      \n" ^
"    (cell XNOR2                                               \n" ^
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port I0                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port I1                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port O                                             \n" ^
"            (direction OUTPUT))                               \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"XOR\") (owner \"Ruby\")) \n" ^
"          (property SCHNM (string \"XNOR2\") (owner \"Ruby\"))\n" ^
"      ))                                                      \n" ^
"    (cell M2_1                                                \n" ^
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port S0                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port D0                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port D1                                            \n" ^
"            (direction INPUT))                                \n" ^
"          (port O                                             \n" ^
"            (direction OUTPUT))                               \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"MUX\") (owner \"Ruby\")) \n" ^
"          (property SCHNM (string \"M2_1\") (owner \"Ruby\")) \n" ^
"      ))                                                      \n" ^
"    (cell GND                                                 \n" ^
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port GROUND                                        \n" ^
"            (direction OUTPUT))                               \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"ZERO\") (owner \"Ruby\"))\n" ^
"          (property SCHNM (string \"GND\") (owner \"Ruby\"))  \n" ^
"      ))                                                      \n" ^
"                                                              \n" ^
"    (cell VCC                                                 \n" ^
"     (cellType GENERIC)                                       \n" ^
"      (view view_1                                            \n" ^
"        (viewType NETLIST)                                    \n" ^
"        (interface                                            \n" ^
"          (port VCC                                           \n" ^
"           (direction OUTPUT))                                \n" ^
"        )                                                     \n" ^
"          (property DEVICE (string \"ONE\") (owner \"Ruby\")) \n" ^
"          (property SCHNM (string \"VCC\") (owner \"Ruby\"))  \n" ^
"      ))                                                      \n" ^
"    (cell FDC                                                 \n" ^
"     (cellType GENERIC)                                       \n" ^         
"       (view view_1                                           \n" ^     
"         (viewType NETLIST)                                   \n" ^
"       (interface                                             \n" ^ 
"         (port C                                              \n" ^  
"           (direction INPUT))                                 \n" ^
"         (port D                                              \n" ^      
"           (direction INPUT))                                 \n" ^  
"         (port Q                                              \n" ^     
"           (direction OUTPUT))                                \n" ^  
"       )                                                      \n" ^    
"         (property DEVICE (string \"DFF\") (owner \"Ruby\"))  \n" ^
"        (property SCHNM  (string \"FDC\") (owner \"Ruby\"))   \n" ^
"     ))                                                       \n"
end (* of structure Library *);
(* open Library *)
