open Graphics
open Options

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
    

(*************Types Generiques***************)
and genEntrees = Int of int | L | F
and genStates = E of genEntrees | Q of int | Bool of bool | Base of int*int | State of genEntrees * bool * bool | State2 of bool*bool*bool*bool*bool
and render = Classic | Ring

let miniNbA = ref 0

let algo = ref {
  entrees = [];
  initConf = (fun () -> ());
  input = (fun a -> Q(0));
  output = (fun s -> "");
  select = (fun i -> i,i);
  render = Classic;
  step = (fun (a,b) -> a,b);
  listStateColor = [];
  objectif = (fun () -> "")
}

let conf = ref [||]
  
(**************Méthodes***************)

let rec randArray len v =
  let n = Random.int len in
  if n != v then
    n
  else
    randArray len v

let selectRand len =
  let i1 = randArray len (-1) in
  let i2 = randArray len i1 in
  i1,i2
    
(*************************************)
(***********Plus de Leader?***********)
(*************************************)

let accL, accF = ref 0, ref 0
  
let initLeader () =
  accL := 0;
  accF := 0;
  let affect x =
    let n = Random.int (List.length !algo.entrees) in
    !algo.input (List.nth !algo.entrees n)
  in
  conf := Array.init !miniNbA affect

    
let leaderInput = function
  | L -> incr accL; E(L)
  | F -> incr accF; E(F)
  | _ -> assert false

let leaderOutput = function
  | Bool(true) -> "1"
  | Bool(false) -> "0"
  | E(L) -> "L"
  | E(F) -> "F"
  | _ -> assert false
     
let leaderConv = function
  | E(L), E(F)                    -> decr accL; decr accF; Bool(false), Bool(false)
  | E(L), Bool(false)             -> E(L), Bool(true)
  | E(F), Bool(true)              -> E(F), Bool(false)
  | Bool(false), Bool(true)       -> Bool(false), Bool(false)
  | s1, s2                        -> s1, s2

let leaderStateColor = [(E(L),rgb 0 204 0);(E(F),rgb 204 0 0);(Bool(true),rgb 153 255 153);(Bool(false),rgb 255 153 153)]

let leaderObj () =
  ("Plus de Learders? : L " ^ (string_of_int !accL) ^ " | F " ^ (string_of_int !accF))
    
let testLeader = {
  entrees = [L;F];
  initConf = initLeader;
  input = leaderInput;
  output = leaderOutput;
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
    let n = Random.int (List.length !algo.entrees) in
    accC := (!accC + n) mod 4;
    !algo.input (List.nth !algo.entrees n)
  in
  conf := Array.init !miniNbA affect

let count4Input = function
  | Int(0) -> E(Int(0))
  | Int(1) -> E(Int(1))
  | Int(2) -> E(Int(2))
  | Int(3) -> E(Int(3))
  | _ -> assert false
     

let count4Output = function
  | E(Int(0)) -> "0"
  | E(Int(1)) -> "1"
  | E(Int(2)) -> "2"
  | E(Int(3)) -> "3"
  | Q(0) -> "Q0"
  | Q(1) -> "Q1"
  | Q(2) -> "Q2"
  | Q(3) -> "Q3"
  | _ -> assert false

let count4Conv = function
  | E(Int(x)), E(Int(y)) -> E(Int((x+y) mod 4)),Q((x+y) mod 4)
  | E(Int(x)), Q(y) -> E(Int(x)), Q(x)
  | s1, s2 -> s1, s2

let count4StateColor = [(E(Int(0)),rgb 0 204 0);
		        (E(Int(1)),rgb 0 102 204);
			(E(Int(2)),rgb 204 102 0);
			(E(Int(3)),rgb 102 0 204);
			(Q(0),rgb 153 255 153);
		        (Q(1),rgb 153 255 255);
			(Q(2),rgb 255 204 153);
			(Q(3),rgb 204 153 255)]
let count4Obj () =
  ("Somme mod 4 = " ^ (string_of_int !accC))
    
let testCount4 = {
  entrees = [Int(0);Int(1);Int(2);Int(3)];
  initConf = initCount;
  input = count4Input;
  output = count4Output;
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

let orInput = function
  | Int(0) -> Q(0)
  | Int(1) -> Q(1)
  | _ -> assert false
     

let orOutput = function
  | Q(0) -> "Q0"
  | Q(1) -> "Q1"
  | _ -> assert false

let orConv = function
  | Q(x),Q(y) when x = 1 && y != 1 || x != 1 && y = 1 -> incr nbT; Q(1),Q(1)
  | s1, s2 -> s1,s2

let orStateColor = [(Q(1),rgb 0 204 0);
			(Q(0),rgb 204 102 0)]
let orObj () =
  ("Ou Logique = " ^ (string_of_int !nbT) ^ " / " ^ (string_of_int !miniNbA))


let initOr () =
  nbT := 1;
  let n = Random.int !miniNbA in
  let affect x =
    if n = x then
      !algo.input (List.nth !algo.entrees 1)
    else
      !algo.input (List.nth !algo.entrees 0)
  in
  conf := Array.init !miniNbA affect

let testOr = {
  entrees = [Int(0);Int(1)];
  initConf = initOr;
  input = orInput;
  output = orOutput;
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
let bstInput = function
  | Int(0) -> Q(0)
  | Int(1) -> Q(1)
  | L -> Base(0,0)
  | _ -> assert false
     

let bstOutput = function
  | Q(0) -> "0"
  | Q(1) -> "1"
  | Base _ -> "BST"
  | _ -> assert false
     
let bstConv = function
  | Base(n0,n1), Q(0) ->
     let n0 = if n0 > 0 then n0-1 else n0 in
    Base(n0,n1+1), Q(1)
  | Base(n0,n1), Q(1) ->
     let n1 = if n1 > 0 then n1-1 else n1 in
    Base(n0+1,n1), Q(0)
  | _ -> assert false


let bstStateColor = [(Base(0,0),rgb 204 102 0);
		     (Q(1),rgb 0 204 0);
		    (Q(0),rgb 0 102 204)]
let bstObj () =
  match Array.get !conf 0 with
  | Base (n0,n1) ->
    ("Space-optimal counting = " ^ (string_of_int (n0 + n1)) ^ " / " ^ (string_of_int (!miniNbA-1)))
  | _ -> assert false

let initBST () =
  let affect x =
    if x = 0 then
      !algo.input (List.nth !algo.entrees 0)
    else
      let n = (Random.int ((List.length !algo.entrees)-1)) +1 in
      !algo.input (List.nth !algo.entrees n)
  in
  conf := Array.init !miniNbA affect

let selectBst len =
  let i2 = (Random.int (len-1))+1 in
  0,i2
    
let testBST = {
  entrees = [L;Int(0);Int(1)];
  initConf = initBST;
  input = bstInput;
  output = bstOutput;
  select = selectBst;
  step = bstConv;
  render = Classic;
  listStateColor = bstStateColor;
  objectif = bstObj
}

(*************************************)
(**************Token ring*************)
(*************************************)
let tokenInput = function
  | L -> State(L,Random.bool (), Random.bool ())
  | F -> State(F,Random.bool (), Random.bool ())
  | _ -> assert false
     

let tokenOutput = function
  | State(L, true, _) -> "L Token"
  | State(F, true, _) -> "F Token"
  | State(L, false, _) -> "L Empty"
  | State(F, false, _) -> "F Empty"
  | _ -> assert false
     
let tokenConv = function
  | State(F, _, b1), State(L,  _, b2) when b1 = b2 ->
     State(F, false, b1), State(L, true, not b1)
  | State(s1, _, b1), State(F,  _, b2) when b1 <> b2 ->
     State(s1, false, b1), State(F, true, b1)
  | s1, s2 -> s1,s2


let tokenStateColor = [(State(L,true,true),rgb 0 204 0);
		       (State(F,true,true),rgb 153 255 153);
		       (State(L,false,true),rgb 204 0 0);
		       (State(F,false,true),rgb 255 153 153)]
		       
let tokenObj () =
  "Token ring de taille " ^ (string_of_int !miniNbA)

let initToken () =
  let affect x =
    if x = 0 then
      !algo.input (List.nth !algo.entrees 0)
    else
      !algo.input (List.nth !algo.entrees 1)
  in
  conf := Array.init !miniNbA affect

let selectToken len =
  let i1 = randArray len (-1) in
  let i2 = (i1+1) mod len in
  i1,i2
    
let testToken = {
  entrees = [L;F];
  initConf = initToken;
  input = tokenInput;
  output = tokenOutput;
  select = selectToken;
  step = tokenConv;
  render = Ring;
  listStateColor = tokenStateColor;
  objectif = tokenObj
}

(*************************************)
(**************Token ring*************)
(*************************************)
let electionInput = function
  | F -> State2(false, Random.bool (), Random.bool (),false, false)
  | _ -> assert false
     

let electionOutput = function
  | State2(_, true, _, _, _) -> "Leader"
  | State2(true, _, _, false, _) -> "NL Bullet"
  | State2(false, _, _, true, _) -> "NL Shield"
  | _ -> "NL Empty"
     
let electionConv = function
  | State2(bulletU, leaderU, labelU, probeU, phaseU), State2(bulletV, leaderV, labelV, probeV, phaseV) ->
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
    State2(!nbU, !nlU, !nllU, !npU, !nphU), State2(!nbV, !nlV, !nllV, !npV, !nphV)
  | _ -> assert false
	

let electionStateColor =
  [(State2(false ,true, false, false, false),rgb 0 204 0);
   (State2(false, false, false, true, false),rgb 0 102 204);
   (State2(true, false, false, false, false),rgb 204 0 0);
   (State2(false, false, false, false, false),rgb 255 153 153)]
		       
let electionObj () =
  "Election de Leader avec ring impaire (" ^ (string_of_int !miniNbA) ^ ")"

let initElection () =
  let affect x =
    !algo.input (List.nth !algo.entrees 0)
  in
  conf := Array.init !miniNbA affect

let selectElection len =
  let i1 = randArray len (-1) in
  let i2 = (i1+1) mod len in
  i1,i2
    
let testElection = {
  entrees = [F];
  initConf = initElection;
  input = electionInput;
  output = electionOutput;
  select = selectElection;
  step = electionConv;
  render = Ring;
  listStateColor = electionStateColor;
  objectif = electionObj
}
  
(********************************)
(**************Init**************)
(********************************)
  
let minimize id =
  miniNbA :=
    match !algo.render with
    | Classic -> min nbA 2000
    | Ring ->
       
       let tmp = min nbA 64 in
       if id = 5 then begin
	 if tmp mod 2 = 0 then tmp - 1 else tmp
       end
       else
	 tmp
	   
let algoList : algo list = [testLeader; testCount4; testOr; testBST; testToken; testElection]

let () =
  id := if !id >= List.length algoList then 0 else !id;
  algo :=  List.nth algoList !id;
  minimize !id;
  !algo.initConf ()
