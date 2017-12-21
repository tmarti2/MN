open Types
open Graphics
open Options

let add_if_uniq v l = List.mem v l
    
let remove_dup l =
  let res = ref [] in
  List.iter (fun e -> if List.mem e !res then () else res := !res@[e]) l;
  !res
    
let displayA v =
  try
    let s = !algo.output v in
    s,snd (List.find (fun (st,c) ->
      comp v st
    ) !algo.listStateColor)
  with
    Not_found -> assert false
      
let legR = ref 40
let legMargin = ref 90
let space = 4
let bot = 150

let init_stats () =
  (* séparation légende *)
  set_color black;
  set_line_width 5;
  moveto 0 bot;
  lineto 277 bot;
  set_line_width 1;
  moveto 10 (bot-30);
  draw_string"Stats :";
  let ts = get_time() in
  let time = Printf.sprintf "   Temps      : %.3fs"  ts  in
  let nbC =  Printf.sprintf "   Nb com'    : %d"  !countCalc in
  let ts =
    Printf.sprintf "   Nb com/sec : %d" (if ts <> 0.0 then truncate ((float_of_int !countCalc)/.ts) else 0)
  in
  moveto 10 (bot-60);
  draw_string time;
  moveto 10 (bot-80);
  draw_string nbC;
  moveto 10 (bot-100);
  draw_string ts
    

  
let display_stats () =
  set_color white;
  fill_rect  143 0 120 (bot-10);
  set_color black;
  let ts = get_time() in
  let time = Printf.sprintf "                %.3fs"  ts  in
  let nbC =  Printf.sprintf "                %d"  !countCalc in
  let ts =
    Printf.sprintf "                %d" (if ts <> 0.0 then truncate ((float_of_int !countCalc)/.ts) else 0)
  in
  moveto 10 (bot-60);
  draw_string time;
  moveto 10 (bot-80);
  draw_string nbC;
  moveto 10 (bot-100);
  draw_string ts
  
  
let display_leg () =
  (* séparation légende *)
  set_color black;
  set_line_width 5;
  moveto 277 768;
  lineto 277 0;
  set_line_width 1;
  moveto 10 740;
  draw_string "Légendes :";
  
  let listLeg = List.map (fun (s,c) -> (!algo.output s),c) !algo.listStateColor in
  let n = ref (2 * !legMargin + ((List.length listLeg)-1) * (!legR*2 + space) + !legR*2) in
  while !n > (win_h-bot) do
    legR := !legR - 1;
    legMargin := !legR + 50;
    n := (2 * !legMargin + ((List.length listLeg)-1) * (!legR*2 + space ) + !legR*2);
  done;
  let draw_el i s c =
    let x,y,r = 50, (win_h - i * (!legR*2 + space) - !legMargin), !legR in
    set_color c;
    fill_circle x y r;
    set_color black;
    draw_circle x y r;
    moveto  (x+50) (y-5);
    draw_string (" : "^s)
  in
  List.iteri (fun i (s,c) -> draw_el i s c) listLeg

let print_obj () =
  let s = "Objectif  : " ^ (!algo.objectif ()) in
  set_color white;
  fill_rect 290 745 500 20;
  moveto 290 750;
  set_color black;
  draw_string s
    
let getDim len =
  truncate (ceil (sqrt (float len)))
       
let sizeAR = ref 80
let margeOffset = 70
let margeW = 80
let margeH = 20
  
let prev1 = ref (-1)
let prev2 = ref (-1)

let pi = atan(1.0) *. 4.0

let offsetW = ref 0
let offsetH = ref 0
let n = ref (getDim (Array.length !conf))
let angle = ref (360.0 /. (float_of_int (Array.length !conf)))
let ra = 250.0
  
let calc_pos i =
  match !algo.render with
  | Classic ->
     let col = i mod !n in
     let lig = i / !n in
     let x = 282 + !offsetW + col * (!sizeAR + space) + margeW in
     let y = win_h - !offsetH - lig * (!sizeAR + space) - margeH - margeOffset - !sizeAR in
     Array.set !pos i (x,y)
  | Ring ->
     let a = (!angle *. (float_of_int i) +. 180.0) *. pi /. 180.0 in
     let x = truncate((float_of_int (win_w-(win_h/2))) +. ra *. cos(a)) in
     let y = truncate((float_of_int (win_h/2)) -. ra *. sin(a)) - margeOffset/2  in
     Array.set !pos i (x,y)
       
let init_render () =
  sizeAR  := 80;
  offsetW := 0;
  offsetH := 0;
  n := getDim (Array.length !conf);
  angle := 360.0 /. (float_of_int (Array.length !conf));
   begin
    match !algo.render with
    | Classic ->
       let dimW = ref (2 * margeW + (!n-1) * (!sizeAR + space) + !sizeAR) in
       let dimH = ref (2 * margeH + margeOffset + (!n-1) * (!sizeAR + space) + !sizeAR) in
       while !dimH > win_h || !dimW > win_h do
	 sizeAR := !sizeAR - 1;
         dimW := (2 * margeW + (!n-1) * (!sizeAR + space) + !sizeAR);
	 dimH := (2 * margeH + margeOffset + (!n-1) * (!sizeAR + space) + !sizeAR);
       done;
       offsetW := (win_h - !dimW)/2;
       offsetH := (win_h - !dimH)/2 
    | Ring ->
       let t = ref ((Array.length !conf) * (2 * !sizeAR + space)) in
       let p = truncate (2.0 *. pi *. 250.0) in
       while !t > p do
	 sizeAR := !sizeAR - 1;
	 t :=  (Array.length !conf) * (2 * !sizeAR + space)
       done;
   end;
   Array.iteri (fun i v -> calc_pos i) !pos
       
let draw_info_ring st i =
  let x,y = Array.get !pos i in
  let v =
    match st with
    | Token(_,_,v) ->
       v
    | Election(_, _, v, _, _) ->
     let i2 = (i+1) mod !miniNbA in
     let i3 = (if i-1 = -1 then !miniNbA-1 else (i-1 mod !miniNbA)) in
     let i2 =
       match Array.get !conf i2, Array.get !conf i3 with
       | Election(_,_,v2,_,_), Election(_,_,v3,_,_) ->
	  if v = v2 then
	    i2
	  else begin
	    if v = v3 then
	      i3
	    else
	      -1
	  end
       | _ -> assert false
     in
     if i2 != -1 then begin
       let x2,y2 = Array.get !pos i2 in
       let sr  = min ((!sizeAR/4)+1) 4 in
       set_line_width sr;
       draw_circle x y  (!sizeAR-(sr/2));
       draw_circle x2 y2  (!sizeAR-(sr/2));
       set_line_width 1;
     end;
     v
    | _ -> assert false
  in
  let sv = if v then "1" else "0" in
  let w,h = text_size sv in
  moveto (x-w/2) (y-h/2);
  draw_string sv
       
let draw_select i =
  set_color black;
  match !algo.render with
  | Classic ->
     set_line_width 2;
     let x,y = Array.get !pos i in
     let r = !sizeAR in
     moveto (x+1) (y+1);
     lineto (x-1+r) (y+r-1);
     moveto (x+1) (y-1+r);
     lineto (x+r-1) (y+1);
     set_line_width 1
  | Ring ->
     let ra = (ra -. (float_of_int (!sizeAR)))-.4. in
     let a = (!angle *. (float_of_int i) +. 180.0) *. pi /. 180.0 in
     let x = truncate((float_of_int (win_w-(win_h/2))) +.  ra *. cos(a)) in
     let y = truncate((float_of_int (win_h/2)) -. ra *. sin(a)) - margeOffset/2  in
     let cx = truncate((float_of_int (win_w-(win_h/2))) +. 13. *. cos(a)) in
     let cy = truncate((float_of_int (win_h/2)) -. 13. *. sin(a)) - margeOffset/2  in
     moveto cx cy;
     lineto x y
         
let draw_agent i =
  let st = Array.get !conf i in 
  let _,c = displayA st in
  let r = !sizeAR in
  set_color c;
  let x,y = Array.get !pos i in
  begin
    match !algo.render with
    | Classic ->
       fill_rect x y r r;
      set_color black;
      draw_rect x y r r;
    | Ring   ->
       fill_circle x y !sizeAR;
      set_color black;
      draw_circle x y  !sizeAR;
      draw_info_ring st i
  end;
  match !selected with
  | None -> ()
  | Some(si) ->
     if i = si then
       draw_select i
       
let draw_unselect i =
  set_color white;
  match !algo.render with
  | Classic ->
     draw_agent i
  | Ring -> 
     let ra = (ra -. (float_of_int (!sizeAR))) -. 4. in
     let a = (!angle *. (float_of_int i) +. 180.0) *. pi /. 180.0 in
     let x = truncate((float_of_int (win_w-(win_h/2))) +.  ra *. cos(a)) in
     let y = truncate((float_of_int (win_h/2)) -. ra *. sin(a)) - margeOffset/2  in
     let cx = truncate((float_of_int (win_w-(win_h/2))) +. 13. *. cos(a)) in
     let cy = truncate((float_of_int (win_h/2)) -. 13. *. sin(a)) - margeOffset/2  in
     set_line_width 3;
     moveto cx cy;
     lineto x y;
     set_line_width 1
       
let draw_link i1 i2 =
  set_color black;
  let r = !sizeAR/3 in
  let x1,y1 = Array.get !pos i1 in
  let x2,y2 = Array.get !pos i2 in
  match !algo.render with
  | Classic ->
     fill_rect (x1+r) (y1+r) r r;
    draw_rect (x2+r) (y2+r) r r
  | Ring ->
     fill_circle x1 y1 (!sizeAR/3);
    draw_circle x2 y2 (!sizeAR/3)
  
let display_conf v =
  begin
    match v with
    | None ->
       set_color white;
      fill_rect (282 + margeW) (margeH) (win_w - 2 * margeW - 282) (win_h - margeOffset - 2 * margeH);
      set_color black;
      if !algo.render = Ring then begin
	let x = truncate(float_of_int (win_w-(win_h/2))) in
	let y = truncate(float_of_int (win_h/2)) - margeOffset/2  in
	fill_circle x y 10;
      end;
      Array.iteri (fun i s -> draw_agent i) !conf;
      prev1 := -1;
      prev2 := -1;
    | Some(-1,i) ->
       draw_agent i
    | Some(i1,i2) ->
       draw_agent i1;
      draw_agent i2;
      if !link then
	begin
	  if !prev1 != -1 && !prev2 != -1 then
	    begin
	      draw_agent !prev1;
	      draw_agent !prev2
	    end;
	  prev1 := i1;
	  prev2 := i2;
	  draw_link i1 i2
	end;
  end;
  print_obj ()
       
let print_pause () =
  set_color white;
  fill_rect 290 725 300 20;
  moveto 290 730;
  set_color black;
  if not !pause then
    draw_string ("Pause     : OFF")
  else
    draw_string ("Pause     : ON")

let print_stats () =
  let ts = get_time() in
  Printf.printf "Temps (sec) : %f\n" ts;
  Printf.printf "Nombre de transitions : %d\n" !countCalc;
  let ts =
    if ts <> 0.0 then
      (float_of_int !countCalc)/.ts
    else
      0.0
  in
  Printf.printf "Nombre de transitions/sec : %.3f\n" ts

let print_link () =
  set_color white;
  fill_rect 290 705 325 25;
  moveto 290 710;
  set_color black;
  if not !link then
    draw_string ("Link      : OFF")
  else
    draw_string ("Link      : ON")

let print_fps () =
  set_color white;
  fill_rect (win_w - 235) 750 235 15;
  moveto (win_w - 235) 750;
  set_color black;
  if not !unlimited then
    draw_string ("Transitions/sec : " ^ (string_of_int !fps))
  else
    draw_string "Transitions/sec : nolimit"

let print_algo () =
  moveto 290 690;
  draw_string ("0|1|2|3|4 : changer d'algorithme")
      
let print_unlim () =
  moveto (win_w - 235) 750;
  set_color black;
  draw_string "U pour ON/OFF nolimit";
  moveto (win_w - 235) 730;
  draw_string "+/- pour changer les fps"

let display_text () =
  set_line_width 5;
  moveto 280 (win_h - margeOffset);
  lineto win_w (win_h - margeOffset);
  set_line_width 1;
    
  print_obj ();
  print_pause ();
  print_fps ();
  print_link()
  (*print_unlim ();
    print_algo ()*)
    
let display_init () =
  clear_graph ();
  display_leg ();
  display_text ();
  init_stats ();
  init_render ();
  display_conf None
