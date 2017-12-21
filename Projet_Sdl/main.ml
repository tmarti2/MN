open Graphics
open Sdl
open Sdltimer

open Options
open Render
open Types

exception Found

let calc () =
  incr countCalc;
  let i1,i2 = !algo.select  (Array.length !conf) in
  let a1 = Array.get !conf i1 in
  let a2 = Array.get !conf i2 in
  let new_a1, new_a2 = !algo.step (a1,a2) in
  Array.set !conf i1 new_a1;
  Array.set !conf i2 new_a2;
  i1,i2

let dist sx sy x y =
  let sx,sy,x,y =
    float_of_int sx, float_of_int sy, float_of_int x, float_of_int y
  in
  let r = float_of_int !sizeAR in
  match !algo.render with
  | Classic ->
     sx >= x && sx <= x +. r && sy >= y && sy <= y +. r
  | Ring ->
     let d = sqrt ((x-.sx) *. (x-.sx) +. (y-.sy) *. (y-.sy)) in
     d <= r
     
    
let rec select (sx,sy) =
  try
    match !selected with
    | None ->
       Array.iteri (fun i (x,y) ->
	 if (dist sx sy x y) then begin
	   selected := Some(i);
	   draw_select i;
	   raise Found
	 end
       ) !pos
    | Some(i) ->
       selected := None;
      draw_unselect i;
      select (sx,sy)
  with
  | Found -> ()

let rec action () =
  let e = wait_next_event [Key_pressed; Button_down] in
  if e.keypressed then begin
    let key = e.key in
    begin
      let echap = Char.chr 27 in
      match key with
      | 'p' ->
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
	pos := Array.make !miniNbA (-1,-1); 
	reset_timer ();
	display_init ();
      | t when t = echap ->
	 close_graph ();
	if !pause then endPause();
        exit 0
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
	 pos := Array.make !miniNbA (-1,-1);
	 reset_timer ();
	 selected := None;
	 display_init ();
      | 'q' ->
	 begin
	   match !selected with
	   | None -> ()
	   | Some(i) ->
	      let ni =  if i-1 = -1 then !miniNbA-1 else (i-1 mod !miniNbA) in
	      selected := Some(ni);
	      draw_select ni;
	      draw_unselect i
	 end
      | 'd' ->
	 begin
	   match !selected with
	   | None -> ()
	   | Some(i) ->
	      let ni = (i+1) mod !miniNbA in
	      selected := Some(ni);
	      draw_select ni;
	      draw_unselect i
	 end
      | '&' -> (* Token / Leader *)
	 begin
	   match !algo.render, !selected with
	   | Ring, Some(i) ->
	      begin
		match Array.get !conf i with
		| Election(b,l,ll,p,ph) -> 
		   Array.set !conf i (Election(b,not l,ll,p,ph));
		  draw_agent i
		| Token (l,b,v) ->
		   Array.set !conf i (Token(l,not b,v));
		  draw_agent i
		| _ -> ()
	      end
	   | Classic, Some(i) -> ()
	   | _ -> ()
	 end
      | 'é' -> (* Value / Bullet *)
	 begin
	   match !algo.render, !selected with
	   | Ring, Some(i) ->
	      begin
		match Array.get !conf i with
		| Election(b,l,ll,p,ph) ->
		   Array.set !conf i (Election(not b,l,ll,p,ph));
		  draw_agent i
		| Token (l,b,v) ->
		   Array.set !conf i (Token(l,b,not v));
		  draw_agent i
		| _ -> ()
	      end
	   | Classic, Some(i) -> ()
	   | _ -> ()
	 end
      | '"' -> (* LF / Probe *)
	 begin
	   match !algo.render, !selected with
	   | Ring, Some(i) ->
	      begin
		match Array.get !conf i with
		| Election(b,l,ll,p,ph) -> 
		   Array.set !conf i (Election(b,l,ll,not p,ph));
		  draw_agent i
		| Token (l,b,v) ->
		   Array.set !conf i (Token((if l = L then F else L),b,v));
		  draw_agent i
		| _ -> ()
	      end
	   | Classic, Some(i) -> ()
	   | _ -> ()
	 end
      | _ -> ()
    end
  end
  else
    if e.button then begin
      select (e.mouse_x,e.mouse_y);
    end;
  synchronize();
  Thread.yield ();
  action ()
    
let rec main  =
  (*Boucle principale du programme*)
  fun last_tick ->
    
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
	let frametime = arrondi_float (1000./.(float !fps)) in 
	let remaining = last_tick + frametime - (get_ticks ()) in
	if (remaining > 0) then (delay remaining);
	
      end;
    main (get_ticks())
  
let () =
  at_exit quit;
  open_graph (Printf.sprintf " %dx%d" win_w win_h);
  set_font "9x15bold";
  auto_synchronize false;
  init [`TIMER];
  startPause();
  display_init ();
  synchronize();
  endPause ();
  let thread_action = Thread.create action () in
  let thread_main = Thread.create main (get_ticks()) in
  Thread.join thread_action
