open Graphics
  
type algo = {
  initConf : unit -> unit;
  comp : genStates -> genStates -> bool;
  select : unit -> (int*int);
  render : render;
  step : genStates*genStates -> genStates*genStates;
  listStateColor : (genStates*color*string) list;
  objectif : unit -> string
}
  
and genStates =
    E of int
  | Q of int
  | L | F
  | Bool of bool
  | Base of int*int
  | Token of bool * bool * bool
  | Election of bool*bool*bool*bool*bool
  (* Token - Bit - bullet - leader - label - probe - phaseU *)
  | ET of bool*bool*bool*bool*bool*bool*bool
and render = Classic | Ring
    
val algo : algo ref
val conf: genStates array ref
val pos : (int*int) array ref
val comp : genStates -> genStates -> bool
  
val algoList : algo list
val miniNbA : int ref
val minimize : int -> unit
