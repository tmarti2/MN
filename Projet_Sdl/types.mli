open Graphics
  
type algo = {
  entrees : genEntrees list;
  initConf : unit -> unit;
  input : genEntrees -> genStates;
  output : genStates -> string;
  select : int -> (int*int);
  render : render;
  step : genStates*genStates -> genStates*genStates;
  listStateColor : (genStates*color) list;
  objectif : unit -> string
}
and genEntrees = Int of int | L | F
and genStates =
    E of genEntrees
  | Q of int
  | Bool of bool
  | Base of int*int
  | Token of genEntrees * bool * bool
  | Election of bool*bool*bool*bool*bool
and render = Classic | Ring
    
val algo : algo ref
val conf: genStates array ref
val pos : (int*int) array ref
val comp : genStates -> genStates -> bool
  
val algoList : algo list
val miniNbA : int ref
val minimize : int -> unit
