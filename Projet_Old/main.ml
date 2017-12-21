open Graphics
open Sdl
open Sdltimer

open Options
open Render
open Types
      
let calc () =
  incr countCalc;
  let i1,i2 = !algo.select  (Array.length !conf) in
  let a1 = Array.get !conf i1 in
  let a2 = Array.get !conf i2 in
  let new_a1, new_a2 = !algo.step (a1,a2) in
  Array.set !conf i1 new_a1;
  Array.set !conf i2 new_a2;
  i1,i2
      
let rec main =
  (*Boucle principale du programme*)
  fun last_tick ->
    
    (* clavier ici *)
    if key_pressed () then
      begin
	let echap = Char.chr 27 in
	let c = read_key () in
	match c with
	| 'p' | 'P' ->
	   begin
	     if not !pause then
	       startPause ()
	     else
	       endPause ()
	   end;
	  print_pause()
	| '+' -> speed_up(); print_fps()
	| '-' -> speed_down(); print_fps()
	| 'u' -> unlimited := not !unlimited; print_fps()
	| 'l' | 'L' -> link := not !link; print_link () ;display_conf None
	| 'r' ->
	  !algo.initConf ();
	  reset_timer ();
	  display_init ();
	| t when t = echap || t = 'q' ->
	   close_graph ();
	  if !pause then endPause();
	  print_stats(); exit 0
	| t when t = '0' || t = '1' || t = '2' || t = '3' ||  t = '4' || t = '5' ->
	   let id =
	     match t with
	     | '0' -> 0
	     | '1' -> 1
	     | '2' -> 2
	     | '3' -> 3
	     | '4' -> 4
	     | '5' -> 5
	     | _ -> assert false
	   in
	   algo := List.nth algoList id;
	   minimize id;
	   !algo.initConf ();
	   reset_timer ();
	   display_init ();
	| _ -> ()
      end;

    if not !pause then
      begin
	let i1,i2 = calc () in
	display_conf (Some(i1,i2));
	display_text ();
	display_stats ()
      end;
    synchronize();
    if not !unlimited then
      begin
	let frametime = arrondi_float (1000./.(float_of_int !fps)) in
	let remaining = last_tick + frametime - (get_ticks ()) in
	if (remaining > 0) then delay remaining
      end;
    main (get_ticks())
  
let () =
  at_exit quit;
  open_graph (Printf.sprintf " %dx%d" win_w win_h);
  set_font "9x15bold";
  auto_synchronize false;

  Sdl.init [`TIMER];
  startPause();
  display_init ();
  synchronize();
  
  main (get_ticks())
