	       
val win_w : int
val win_h : int
  
val ups : int ref
val frametime : float
val utime : float ref
val offset : float ref
val maju : unit -> unit
  
val id : int ref
val unlimited : bool ref
val link : bool ref
val nbA : int

val pause : bool ref
val countCalc : int ref

(* Afficher les controles *)			     
val toggle_able : float -> float -> bool

val speed_up : unit -> unit
val speed_down : unit -> unit

val arrondi_float : float -> int

val startPause : unit -> unit
val endPause : unit -> unit
val get_time : unit -> float
val reset_timer : unit -> unit

val selected : int option ref
