(**************************************************************************)
(**************************************************************************)
(***                                                                    ***)
(***    Functions for manipulating data ("&id") values:                 ***)
(***                                                                    ***)
(**************************************************************************)
(**************************************************************************)

structure Data :
  sig
        val DATA_alx_clk        : string ref;

        val DATA_blk_clk        : string ref;
        val DATA_blk_clk_ena    : string ref;
        val DATA_blk_domain_io  : Rubytype.io ref;
        val DATA_blk_range_io   : Rubytype.io ref;

        val DATA_xnf_clk        : string ref;
        val DATA_xnf_clk_ena    : string ref;
        val DATA_xnf_out_ena    : string ref;
        val DATA_xnf_domain_io  : Rubytype.io ref;
        val DATA_xnf_range_io   : Rubytype.io ref;
        val DATA_xnf_header     : string ref;

        val recordData  : (string*Rubytype.io) -> unit
  end =
struct

(**************************************************************************)
(**************************************************************************)

val DATA_alx_clk        = ref "ruby_clk";

val DATA_blk_clk        = ref "ruby_clk";
val DATA_blk_clk_ena    = ref "ruby_clk_ena";
val DATA_blk_domain_io  = ref (Rubytype.PROD []);
val DATA_blk_range_io   = ref (Rubytype.PROD []);

val DATA_xnf_clk        = ref "ruby_clk";
val DATA_xnf_clk_ena    = ref "";
val DATA_xnf_out_ena    = ref "";
val DATA_xnf_domain_io  = ref (Rubytype.PROD []);
val DATA_xnf_range_io   = ref (Rubytype.PROD []);
val DATA_xnf_header     = ref "rubyinclude.xnf";

fun name d (Rubytype.CONST (Rubytype.SYM s))      = s
  | name d (Rubytype.VAR s)              = s
  | name d e                    = Errors.simple_error ("inappropriate value for " ^ d)

fun recordData ("&alx_clk",       e)
        = DATA_alx_clk := name "&alx_clk" e

  | recordData ("&blk_clk",       e)
        = DATA_blk_clk       := name "&blk_clk" e
  | recordData ("&blk_clk_ena",   e)
        = DATA_blk_clk_ena   := name "&blk_clk_ena" e
  | recordData ("&blk_domain_io", e)
        = DATA_blk_domain_io := e
  | recordData ("&blk_range_io",  e)
        = DATA_blk_range_io  := e

  | recordData ("&xnf_clk",       e)
        = DATA_xnf_clk       := name "&xnf_clk" e
  | recordData ("&xnf_clk_ena",   e)
        = DATA_xnf_clk_ena   := name "&xnf_clk_ena" e
  | recordData ("&xnf_out_ena",   e)
        = DATA_xnf_out_ena   := name "&xnf_out_ena" e
  | recordData ("&xnf_domain_io", e)
        = DATA_xnf_domain_io := e
  | recordData ("&xnf_range_io",  e)
        = DATA_xnf_range_io  := e
  | recordData ("&xnf_header",    e)
        = DATA_xnf_header    := name "&xnf_header" e
  | recordData (x,                e)
        = Errors.simple_error (x ^ " is not a known data name")

(**************************************************************************)
(**************************************************************************)

end (* of struct Data *);
(* open Data *)
