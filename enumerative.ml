
type exp =
    | Zero
    | One
    | In
    | Negate: exp -> exp
    | Plus: exp * exp -> exp
    | Mult: exp * exp -> exp
    | IfElse: exp * exp * exp -> exp

exception ProgramNotFound
exception TimeOut

let rec show exp = match exp with
  | Zero -> "0"
  | One -> "1"
  | In -> " in "
  | Negate x -> " - " ^ (show x)
  | Plus (a, b) -> show a ^ " + " ^ show b
  | Mult (a, b) -> show a ^ " * " ^ show b
  | IfElse (a, b, c) -> "IfNonNeg " ^ show a ^ " then " ^ show b ^ " else " ^ show c

let rec evaluate exp input = match exp with
  | Zero -> 0
  | One -> 1
  | In -> input
  | Negate x -> 0 - evaluate x input
  | Plus (a, b) -> evaluate a input + evaluate b input
  | Mult (a, b) -> evaluate a input * evaluate b input
  | IfElse (a, b, c) -> if (evaluate a input > 0) then evaluate b input else evaluate c input

let rec evaluateAll p inputs = List.map (evaluate p) inputs

let accountedFor pList p inputs =
  let pOutputs = evaluateAll p inputs in
    let rec acc_helper pList = match pList with
      | d::ds -> if pOutputs = (evaluateAll d inputs) then true else acc_helper ds
      | [] -> false
  in acc_helper pList

let grow pList =
  let expansion = [] in
  let nList = List.map (fun p -> Negate p) pList in
  let plList = List.map2 (fun a b -> Plus (a, b)) pList pList in
  let mlList = List.map2 (fun a b -> Mult (a, b)) pList pList in
  let ifnList = List.flatten (List.map (fun x -> List.map2 (fun a b -> IfElse (x, a, b)) pList pList) pList) in
  List.concat (expansion::nList::plList::mlList::ifnList::[])

let eliminateEquivalents pList inputs =
  let prunedList = [] in
  let elimeq_h pList = match pList with
    | d::ds -> if accountedFor prunedList d inputs then prunedList else d::prunedList
    | [] -> prunedList in
  elimeq_h pList

let isCorrect p inputs outputs = (evaluateAll p inputs = outputs)

let synthesize inputs outputs =
  let pList = [Zero; One; In] in
  let synth_h loops =
    if loops = 0 then raise TimeOut else
    let pList = grow pList in
    let pList = eliminateEquivalents pList inputs in
    let rec check pList =
      match pList with
      | d::ds -> if isCorrect d inputs outputs then d else check ds
      | [] -> raise ProgramNotFound in
    check pList in
  synth_h 5

