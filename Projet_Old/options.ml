open Sdltimer
open Graphics

let usage = "usage: ./simu [-nbA x][-algo y]"
  
let win_w = 1050
let win_h = 768
let fps = ref 30
let id = ref 0
let unlimited = ref false
let link = ref false
let nbA = ref 36

let pause = ref false
let tPause = ref 0
let sPause = ref 0
  
let countCalc = ref 0
  
(*Registre des dates de modifications de certaines valeurs*)
let last_tick_speedup = ref 0
let last_tick_speeddown = ref 0
  
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
  if ((get_ticks())-last_tick) > min then true else false
    
let speed_up () = 
  if (toggle_able !last_tick_speedup 300) then
    begin
      if !fps+10 > 144 then
        ()
      else
	fps := !fps + 10;
      last_tick_speedup := get_ticks()
    end
      
let speed_down () = 
  if (toggle_able !last_tick_speeddown 300) then
    begin
      if !fps-10 < 1 then
	fps := 1
      else
	fps := !fps - 10;
      last_tick_speeddown := get_ticks()
    end

let arrondi_float f =
  if (truncate (f *. 10.0) mod 10) > 5 then truncate (ceil f)
  else (truncate (floor f))
  
let startPause () =
  sPause := get_ticks();
  pause := true
    
let endPause () =
  tPause := !tPause + (get_ticks () - !sPause);
  pause := false
    
let get_time () =
  let curr = get_ticks() in
  let temps = curr - !tPause in
  (float_of_int temps)/. 1000.0
    
let reset_timer () =
  tPause := get_ticks();
  sPause := 0;
  countCalc := 0;
  startPause();
