open Graphics

let usage = "usage: ./simu [-nbA x][-algo y]"
  
let tPause = ref (Unix.gettimeofday())
let win_w = 1050
let win_h = 768
let fps = ref 25
let id = ref 0
let unlimited = ref false
let link = ref false
let nbA = ref 36

let pause = ref false
let sPause = ref 0.
  
let countCalc = ref 0
  
(*Registre des dates de modifications de certaines valeurs*)
let last_tick_speedup = ref 0.
let last_tick_speeddown = ref 0.
  
let specs = 
  [
    "-algo", Arg.Set_int id, " choix de l'algo";
    "-nbA", Arg.Set_int nbA, " nombre d'agents pour la simulation"
  ]

let alspecs = Arg.align specs

let () =
  Arg.parse alspecs (fun s -> ()) usage

let () =
  Random.self_init();
  fps := if !fps < 1 then 5 else !fps;
  fps := if !fps > 350 then (unlimited := true; 144 )else !fps;
  id  := if !id < 0 then 0 else !id;
  nbA := if !nbA <= 2 then 16 else !nbA

let nbA = !nbA
  
(*L'enssemble des fonctions toggle* permet de limiter la vitesse
  d'activation de certaines actions*)
let toggle_able last_tick min =
  if (Unix.gettimeofday() -. last_tick) > min then true else false
    
let speed_up () = 
  if (toggle_able !last_tick_speedup 0.3) then
    begin
      let f = !fps in
      if 1 <= f && f < 5  then
	fps := !fps + 1
      else if 5 <= f && f < 25 then
	fps := !fps + 5
      else if 25 <= f && f < 100 then
	fps := !fps + 25
      else if f < 400 then
	fps := !fps + 50;
      last_tick_speedup := Unix.gettimeofday()
    end
      
let speed_down () = 
  if (toggle_able !last_tick_speeddown 0.300) then
    begin
      let f = !fps in
      if 1 < f && f <= 5  then
	fps := !fps - 1
      else if 5 < f && f <= 25 then
	fps := !fps - 5
      else if 25 < f && f <= 100 then
	fps := !fps - 25
      else if f <= 400 && f != 1 then
	fps := !fps - 50;
      last_tick_speeddown := Unix.gettimeofday()
    end

let arrondi_float f =
  if (truncate (f *. 10.0) mod 10) > 5 then truncate (ceil f)
  else (truncate (floor f))
  
let startPause () =
  sPause := Unix.gettimeofday();
  pause := true
    
let endPause () =
  tPause := !tPause +. (Unix.gettimeofday() -. !sPause);
  pause := false
    
let get_time () =
  let curr = Unix.gettimeofday() in
  let temps = curr -. !tPause in
  temps
    
let reset_timer () =
  tPause := Unix.gettimeofday();
  sPause := 0.;
  countCalc := 0;
  startPause()

let selected = ref None
