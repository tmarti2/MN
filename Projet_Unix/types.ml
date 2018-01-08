open Graphics
open Options

type algo = {
  initConf : unit -> unit;
  comp : genStates -> genStates -> bool;
  select : unit -> (int*int);
  render : render;
  step : genStates*genStates -> genStates*genStates;
  listStateColor : (genStates*color*string) list;
  objectif : unit -> string
}
  

(*************Types Generiques***************)
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

let miniNbA = ref 0

let algo = ref {
  initConf = (fun () -> ());
  comp = (fun a b -> true);
  select = (fun () -> 0,0);
  render = Classic;
  step = (fun (a,b) -> a,b);
  listStateColor = [];
  objectif = (fun () -> "")
}

let conf = ref [||]
let pos = ref [||]
  
(**************Méthodes***************)

let comp a b =
  a = b
  
let rec randArray len v =
  let n = Random.int len in
  if n != v then
    n
  else
    randArray len v

let selectRand () =
  let i1 = randArray !miniNbA (-1) in
  let i2 = randArray !miniNbA i1 in
  i1,i2


let selectRing () =
  let i1 = randArray !miniNbA (-1) in
  let i2 = (i1+1) mod !miniNbA in
  i1,i2
    
(*************************************)
(***********Plus de Leader?***********)
(*************************************)

let accL, accF = ref 0, ref 0
  
let initLeader () =
  accL := 0;
  accF := 0;
  let affect x =
    if Random.int 100 < 50 then  (incr accL; L) else (incr accF; F);
  in
  conf := Array.init !miniNbA affect

let leaderOutput = function
  | Bool(true) -> "1"
  | Bool(false) -> "0"
  | L -> "L"
  | F -> "F"
  | _ -> assert false
     
let leaderConv = function
  | L, F                   -> decr accL; decr accF; Bool(false), Bool(false)
  | L, Bool(false)         -> L, Bool(true)
  | F, Bool(true)          -> F, Bool(false)
  | Bool(false), Bool(true)-> Bool(false), Bool(false)
  | s1, s2                 -> s1, s2

let leaderStateColor = [(L,rgb 0 204 0,"L");(F,rgb 204 0 0,"F");
			(Bool(true),rgb 153 255 153,"1");
			(Bool(false),rgb 255 153 153,"0")]

let leaderObj () =
  ("Plus de Learders? : L " ^ (string_of_int !accL) ^ " | F " ^ (string_of_int !accF))
    
let testLeader = {
  initConf = initLeader;
  comp = comp;
  select = selectRand;
  step = leaderConv;
  render = Classic;
  listStateColor = leaderStateColor;
  objectif = leaderObj
}

  
(*************************************)
(*************Count mod 4*************)
(*************************************)

let accC = ref 0
  
let initCount () =
  accC := 0;
  let affect x =
    let n = Random.int 4 in
    accC := (!accC + n) mod 4;
    E(n)
  in
  conf := Array.init !miniNbA affect     

let count4Conv = function
  | E(x), E(y) -> E((x+y) mod 4),Q((x+y) mod 4)
  | E(x), Q(y) -> E(x), Q(x)
  | s1, s2 -> s1, s2

let count4StateColor = [(E(0),rgb 0 204 0,"0");
		        (E(1),rgb 0 102 204,"1");
			(E(2),rgb 204 102 0,"3");
			(E(3),rgb 102 0 204,"3");
			(Q(0),rgb 153 255 153,"Q0");
		        (Q(1),rgb 153 255 255,"Q1");
			(Q(2),rgb 255 204 153,"Q2");
			(Q(3),rgb 204 153 255,"Q3")]
let count4Obj () =
  ("Somme mod 4 = " ^ (string_of_int !accC))

    
let testCount4 = {
  initConf = initCount;
  comp = comp;
  select = selectRand;
  step = count4Conv;
  render = Classic;
  listStateColor = count4StateColor;
  objectif = count4Obj
}


(*************************************)
(*************OU Logique**************)
(*************************************)
let nbT = ref 1

let initOr () =
  nbT := 1;
  let n = Random.int !miniNbA in
  let affect x =
    if n = x then
      Q(1)
    else
      Q(0)
  in
  conf := Array.init !miniNbA affect

let orConv = function
  | Q(x),Q(y) when x = 1 && y != 1 || x != 1 && y = 1 -> incr nbT; Q(1),Q(1)
  | s1, s2 -> s1,s2

let orStateColor = [(Q(1),rgb 0 204 0,"1");
		    (Q(0),rgb 204 102 0,"0")]
let orObj () =
  if !nbT = !miniNbA then
    startPause();
  ("Ou Logique = " ^ (string_of_int !nbT) ^ " / " ^ (string_of_int !miniNbA))


let testOr = {
  initConf = initOr;
  comp = comp;
  select = selectRand;
  step = orConv;
  render = Classic;
  listStateColor = orStateColor;
  objectif = orObj
}

(*************************************)
(********Space-Optimal Counting*******)
(************2 states + BST***********)
(*************************************)
let initBST () =
  let affect x =
    if x = 0 then
      Base(0,0)
    else
      Q(Random.int 2)
  in
  conf := Array.init !miniNbA affect
     
let bstConv = function
  | Base(n0,n1), Q(0) ->
     let n0 = if n0 > 0 then n0-1 else n0 in
    Base(n0,n1+1), Q(1)
  | Base(n0,n1), Q(1) ->
     let n1 = if n1 > 0 then n1-1 else n1 in
    Base(n0+1,n1), Q(0)
  | _ -> assert false


let bstStateColor = [(Base(0,0),rgb 204 102 0,"BST");
		     (Q(1),rgb 0 204 0,"1");
		     (Q(0),rgb 0 102 204,"0")]
let bstObj () =
  match Array.get !conf 0 with
  | Base (n0,n1) ->
    ("Space-optimal counting = " ^ (string_of_int (n0 + n1)) ^ " / " ^ (string_of_int (!miniNbA-1)))
  | _ -> assert false

let selectBst () =
  let i2 = (Random.int (!miniNbA-1))+1 in
  0,i2

  
let compBST s1 s2 =
  match s1,s2 with
  | Base _, Base _ -> true
  | _ -> s1 = s2
    
let testBST = {
  initConf = initBST;
  comp = compBST;
  select = selectBst;
  step = bstConv;
  render = Classic;
  listStateColor = bstStateColor;
  objectif = bstObj
}

(*************************************)
(**************Token ring*************)
(*************************************)

let initToken () =
  let affect x =
    if x = 0 then
      Token(true, Random.bool (), Random.bool ())
    else
      Token(false, Random.bool (), Random.bool ())
  in
  conf := Array.init !miniNbA affect
     
let tokenConv = function
  | Token(false, _, b1), Token(true,  _, b2) when b1 = b2 ->
     Token(false, false, b1), Token(true, true, not b1)
  | Token(s1, _, b1), Token(false,  _, b2) when b1 <> b2 ->
     Token(s1, false, b1), Token(false, true, b1)
  | s1, s2 -> s1,s2


let tokenStateColor = [(Token(false,true,false),rgb 255 0 0,"Token");
		       (Token(true,false,false),rgb 0 204 0,"L");
		       (Token(false,false,false),rgb 215 215 215,"NL")]
		       
let tokenObj () =
  "Token ring"

let compToken s1 s2 =
  match s1,s2 with
  | Token(s1,t1,_), Token(s2,t2,_) ->
     t1 && t2 || (not t1 && not t2 && s1 = s2)
  | _ -> assert false
    
let testToken = {
  initConf = initToken;
  comp = compToken;
  select = selectRing;
  step = tokenConv;
  render = Ring;
  listStateColor = tokenStateColor;
  objectif = tokenObj
}

(*************************************)
(**************Token ring*************)
(*************************************)
let initElection () =
  let affect x =
    Election(false, Random.bool (), Random.bool (),false, false)
  in
  conf := Array.init !miniNbA affect     
     
let electionConv = function
  | Election(bulletU, leaderU, labelU, probeU, phaseU), Election(bulletV, leaderV, labelV, probeV, phaseV) ->
    let nbU, nlU, nllU, npU, nphU = ref bulletU, ref leaderU, ref labelU, ref probeU, ref phaseU in
    let nbV, nlV, nllV, npV, nphV = ref bulletV, ref leaderV, ref labelV, ref probeV, ref phaseV in
    if !nllU = !nllV then begin
      if !npU then begin
	nlU := true;
	npU := false;
      end;
      if not !nphU then begin
	nphU := true;
	if not !nlV then
	  npV := true;
      end
      else if not !npV then begin
	nllV := not !nllV;
	nphV := false;
	nbV := false;
      end
    end
    else if !nlV then begin
      if !nbV then
	nlV := false
      else begin
	nbU := true;
	npU := false
      end
    end
    else begin
      if !nbV then begin
	nbV := false;
	nbU := true
      end;
      if !npU then begin
	npU := false;
	npV := true
      end
    end;
    Election(!nbU, !nlU, !nllU, !npU, !nphU), Election(!nbV, !nlV, !nllV, !npV, !nphV)
  | _ -> assert false
	

let electionStateColor =
  [(Election(false ,true, false, false, false),rgb 0 204 0,"L");
   (Election(true, false, false, false, false),rgb 255 255 0,"NL Bullet");
   (Election(false, false, false, true, false),rgb 0 102 204,"NL Probe");
   (Election(false, false, false, false, false),rgb 215 215 215,"NL Empty")]
		       
let electionObj () =
  "Leader Election in a odd size Ring"

let compElection s1 s2 =
  match s1, s2 with
  | Election(b1,l1,_,p1,_), Election(b2,l2,_,p2,_) ->
     if l1 && l2 then
       true
     else if not l1 && not l2 then begin
       if not b1 && not b2 && p1 && p2 then
	 true
       else if b1 && b2 && not p1 && not p2 then
	 true
       else if b1 = p1 && b2 = p2 then
	 true
       else
	 false
     end
     else
       false
  | _ -> assert false
    
let testElection = {
  initConf = initElection;
  comp = compElection;
  select = selectRing;
  step = electionConv;
  render = Ring;
  listStateColor = electionStateColor;
  objectif = electionObj
}


(*************************************)
(***********Token + Election**********)
(*************************************)
  
let initET () =
  let affect x =
    ET(Random.bool (), Random.bool (), false, Random.bool (), Random.bool (),false, false)
  in
  conf := Array.init !miniNbA affect  
     
let etConv = function
  | ET(tokenU, bitU, bulletU, leaderU, labelU, probeU, phaseU), ET(tokenV, bitV, bulletV, leaderV, labelV, probeV, phaseV) ->
     let ntU, nBitU, nbU, nlU, nllU, npU, nphU = ref tokenU, ref bitU, ref bulletU, ref leaderU, ref labelU, ref probeU, ref phaseU in
     let ntV, nBitV, nbV, nlV, nllV, npV, nphV = ref tokenV, ref bitV, ref bulletV, ref leaderV, ref labelV, ref probeV, ref phaseV in
     if !nllU = !nllV then begin
       if !npU then begin
	 nlU := true;
	 npU := false;
       end;
       if not !nphU then begin
	 nphU := true;
	 if not !nlV then
	   npV := true;
       end
       else if not !npV then begin
	 nllV := not !nllV;
	 nphV := false;
	 nbV := false;
       end
     end
     else if !nlV then begin
       if !nbV then
	 nlV := false
       else begin
	 nbU := true;
	 npU := false
       end
     end
     else begin
       if !nbV then begin
	 nbV := false;
	 nbU := true
       end;
       if !npU then begin
	 npU := false;
	 npV := true
       end
     end;
     begin
       match !nlU, !ntU, !nBitU, !nlV, !ntV, !nBitV with
       | false, _, b1, true, _, b2 when b1 = b2 ->
	  ntU := false;
	 ntV := true;
	 nBitV := not b1
       | _, _, b1, false, _, b2 when b1 <> b2 ->
	  ntU := false;
	 ntV := true;
	 nBitV := b1
       | _ -> ()
     end;
     ET(!ntU, !nBitU, !nbU, !nlU, !nllU, !npU, !nphU), ET(!ntV, !nBitV, !nbV, !nlV, !nllV, !npV, !nphV)
  | _ -> assert false
      
let etStateColor = 
  [ (ET(true , false, false, true , false, false, false) , rgb 255 0 0,"Token");
    (ET(false, false, false, true , false, false, false) , rgb 0 204 0,"Leader");
    (ET(false, false, true , false, false, false, false) , rgb 255 255 0,"NL Bullet");
    (ET(false, false, false, false, false, true , false) , rgb 0 102 204,"NL Probe");
    (ET(false, false, false, false, false, false, false) , rgb 215 215 215,"NL")]
		       
let etObj () =
  "Token Ring + Election Leader in a odd size Ring"

let compET s1 s2 =
  match s1, s2 with
  | ET(t1, _, b1, l1, _, p1, _), ET(t2, _, b2, l2, _, p2, _) ->
     if t1 && t2 then
       true
     else if not t1 && not t2 && l1 && l2 then
       true
     else if not t1 && not t2 && not l1 && not l2 then begin
       if b1 && b2 then
	 true
       else if p1 && p2 then
	 true
       else if b1 = b2 && p1 = p2 then
	 true
       else
	 false
     end
     else
       false
  | _ -> assert false
    
let testET = {
  initConf = initET;
  comp = compET;
  select = selectRing;
  step = etConv;
  render = Ring;
  listStateColor = etStateColor;
  objectif = etObj
}


  
  
(********************************)
(**************Init**************)
(********************************)
  
let minimize () =
  miniNbA :=
    match !algo.render with
    | Classic -> min nbA 4096
    | Ring ->
       let tmp = min nbA 64 in
       if !id = 1 || !id = 2 then begin
	 if tmp mod 2 = 0 then tmp - 1 else tmp
       end
       else
	 tmp
	   
let algoList : algo list = [testToken; testElection; testET; testLeader; testCount4; testOr; testBST]
let () =
  algo :=  List.nth algoList !id;
  minimize ();
  !algo.initConf ();
  pos := Array.make !miniNbA (-1,-1); 
