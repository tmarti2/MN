	       
val win_w : int
val win_h : int
val fps : int ref
val id : int ref
val unlimited : bool ref
val link : bool ref
val nbA : int
val pause : bool ref
val countCalc : int ref

(* Afficher les controles *)			     
val toggle_able : int -> int -> bool

val speed_up : unit -> unit
val speed_down : unit -> unit

val arrondi_float : float -> int

val startPause : unit -> unit
val endPause : unit -> unit
val get_time : unit -> float
val reset_timer : unit -> unit
