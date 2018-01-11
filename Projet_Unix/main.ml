open Graphics
open Options
open Render
open Types

exception Found

let calc () =
  incr countCalc;
  let i1,i2 = !algo.select () in
  let a1 = Array.get !conf i1 in
  let a2 = Array.get !conf i2 in
  let new_a1, new_a2 = !algo.step (a1,a2) in
  Array.set !conf i1 new_a1;
  Array.set !conf i2 new_a2

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

let switch t =
 id :=
   begin
     match t with
     | '0' -> 0
     | '1' -> 1
     | '2' -> 2
     | '3' -> 3
     | '4' -> 4
     | '5' -> 5
     | '6' -> 6
     | _ -> assert false
   end;
  algo := List.nth algoList !id;
  minimize ();
  !algo.initConf ();
  pos := Array.make !miniNbA (-1,-1);
  reset_timer ();
  selected := Some(0);
  display_init ()

let modif = function
  | '&' -> (* Leader *)
     begin
       match !algo.render, !selected with
       | Ring, Some(i) ->
	  begin
	    match Array.get !conf i with
	    | Election(b, l, ll, p, ph) ->
	       Array.set !conf i (Election(b,not l,ll,p,ph));
	      draw_agent i
	    | Token (l, t, v) ->
	       Array.set !conf i (Token(not l,t,v));
	      draw_agent i
	    | ET(t, v, b,l,ll,p,ph) ->
	       Array.set !conf i (ET(t, v, b,not l,ll,p,ph));
	      draw_agent i
	    | _ -> ()
	  end
       | Classic, Some(i) -> ()
       | _ -> ()
     end
  | 'é' -> (* Token / Bullet *)
     begin
       match !algo.render, !selected with
       | Ring, Some(i) ->
	  begin
	    match Array.get !conf i with
	    | Election(b,l,ll,p,ph) ->
	       Array.set !conf i (Election(not b,l,ll,p,ph));
	      draw_agent i
	    | Token (l,b,v) ->
	       Array.set !conf i (Token(l,not b,v));
	      draw_agent i
	    | ET(t, v, b,l,ll,p,ph) ->
	       Array.set !conf i (ET(not t, v, b, l,ll,p,ph));
	      draw_agent i
	    | _ -> ()
	  end
       | Classic, Some(i) -> ()
       | _ -> ()
     end
  | '"' -> (* value / Probe *)
     begin
       match !algo.render, !selected with
       | Ring, Some(i) ->
	  begin
	    match Array.get !conf i with
	    | Election(b,l,ll,p,ph) ->
	       Array.set !conf i (Election(b,l,ll,not p,ph));
	      draw_agent i
	    | Token (l,b,v) ->
	       Array.set !conf i (Token(l,b, not v));
	      draw_agent i
	    | ET(t, v, b,l,ll,p,ph) ->
	       Array.set !conf i (ET(t, not v, b, l,ll,p,ph));
	      draw_agent i
	    | _ -> ()
	  end
       | Classic, Some(i) -> ()
       | _ -> ()
     end
  | '\'' -> (* label *)
     begin
       match !algo.render, !selected with
       | Ring, Some(i) ->
	  begin
	    match Array.get !conf i with
	    | Election(b,l,ll,p,ph) ->
	       Array.set !conf i (Election(b,l,not ll,p,ph));
	      draw_agent i
	    | _ -> ()
	  end
       | Classic, Some(i) -> ()
       | _ -> ()
     end
  | _ -> ()

let array_remove arr id =
  let l = Array.to_list !arr in
  let l = List.mapi (fun i v -> if i = id then [] else [v]) l in
  arr := Array.of_list (List.flatten l)
    
let remove_agent () =
  match !selected with
  | None-> ()
  | Some(i) ->
     if !miniNbA > 2 && (!id <> 6 || i <> 0) then begin
       majObj i;
       decr miniNbA;
       selected := None;
       array_remove conf i;
       selected := Some(0);
       init_render ();
       print_nbA();
       display_conf ()
     end
  
     
let rec action () =
  let e = wait_next_event [Key_pressed; Button_down] in
  if e.keypressed then begin
    let key = e.key in
    begin
      let echap = Char.chr 27 in
      match key with
      | 'p' | 'P' | ' ' ->
	 begin
	   if not !pause then begin
	     startPause ();
             display_conf()
           end
	   else
	     endPause ()
	 end;
	print_pause()
      | '+' -> speed_up()  ;maju (); print_ups()
      | '-' -> speed_down();maju (); print_ups()
      | 'u' -> unlimited := not !unlimited; print_ups()
      | 'l' -> remove_agent ()
      | 'n' -> display_conf ()
      | 'r' ->
	 !algo.initConf ();
	pos := Array.make !miniNbA (-1,-1); 
	reset_timer ();
	display_init ();
      | t when t = echap ->
	 if !pause then endPause();
        Thread.exit ()
      | t when t = '0' || t = '1' || t = '2' || t = '3' ||  t = '4' || t = '5' || t = '6'->
	 switch t
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
      | '&' | 'é' | '"' | '\'' -> modif key
      | _ -> ()
    end
  end
  else if e.button then
    select (e.mouse_x,e.mouse_y);
  Thread.yield ();
  action ()


let rec tran last_tick =
  if not !pause then
    begin
      calc ()
    end;
  if not !unlimited then
    begin
      let remaining = (last_tick +. !utime -. (Unix.gettimeofday())) /. 1000.0 in
	(*Erreur de timer quelque part (surement du aux threads, je compense *)
      let remaining = remaining -. (!offset *. (remaining/. 100.)) in
      if (remaining > 0.) then (Thread.delay remaining);
    end
  else
    Thread.yield ();
  tran (Unix.gettimeofday())

    
let rec main last_tick =
  if not !pause then
    begin
      display_conf ();
      display_text ();
      display_stats ()
    end;
  synchronize();
  let remaining = (last_tick +. frametime -. (Unix.gettimeofday())) /. 1000.0 in
  if (remaining > 0.) then (Thread.delay remaining);
  main (Unix.gettimeofday())

let () =
  open_graph (Printf.sprintf " %dx%d" win_w win_h);
  set_font "9x15bold";
  auto_synchronize false;
  startPause();
  display_init ();
  synchronize();
  let thread_action = Thread.create action () in
  let _ = Thread.create tran (Unix.gettimeofday()) in
  let _ = Thread.create main (Unix.gettimeofday()) in
  Thread.join thread_action;
  close_graph ()
