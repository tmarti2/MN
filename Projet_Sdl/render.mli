open Types
  
val display_conf : (int*int) option-> unit
val display_init : unit -> unit
val display_stats : unit -> unit
val display_text : unit -> unit

  
val print_fps : unit -> unit
val print_pause : unit -> unit
val print_link : unit -> unit
val print_stats : unit -> unit

val sizeAR : int ref
val draw_select : int -> unit
val draw_unselect : int -> unit
val draw_agent : int -> unit
