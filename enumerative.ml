
type exp =
    | Zero
    | One
    | In
    | Negate: exp -> exp
    | Plus: exp * exp -> exp
    | Mult: exp * exp -> exp
    | IfNonNeg: exp * exp * exp -> exp

exception TimeOut

let rec show exp = match exp with
  | Zero -> "(0)"
  | One -> "(1)"
  | In -> "(Input)"
  | Negate x -> " - " ^ (show x)
  | Plus (a, b) -> show a ^ " + " ^ show b
  | Mult (a, b) -> show a ^ " * " ^ show b
  | IfNonNeg (a, b, c) -> "IfNonNeg " ^ show a ^ " then " ^ show b ^ " else " ^ show c

let rec evaluate exp input = match exp with
  | Zero -> 0
  | One -> 1
  | In -> input
  | Negate x -> 0 - evaluate x input
  | Plus (a, b) -> evaluate a input + evaluate b input
  | Mult (a, b) -> evaluate a input * evaluate b input
  | IfNonNeg (a, b, c) -> if (evaluate a input > 0) then evaluate b input else evaluate c input

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
  let plList = List.flatten (List.map (fun a -> List.map (fun b -> Plus (a, b)) pList) pList) in
  let mlList = List.flatten (List.map (fun a -> List.map (fun b -> Mult (a, b)) pList) pList) in
  let ifnList = List.flatten (List.map (fun x ->
      List.flatten (List.map (fun a -> List.map (fun b -> IfNonNeg (x, a, b)) pList) pList)) pList) in
  List.concat (expansion::nList::plList::mlList::ifnList::[pList])

let eliminateEquivalents pList inputs =
  let prunedList = [] in
  let rec elimeq_h pList prunedList = match pList with
    | d::ds -> if accountedFor prunedList d inputs then elimeq_h ds prunedList else d::(elimeq_h ds prunedList)
    | [] -> prunedList in
  elimeq_h pList prunedList

let isCorrect inputs outputs p = (evaluateAll p inputs = outputs)

let rec synth_h loops pList inputs outputs =
  if loops = 0 then raise TimeOut else
    begin
      let pList = eliminateEquivalents (grow pList) inputs in
      let candList = List.filter (isCorrect inputs outputs) pList in
      if candList = [] then synth_h (loops - 1) pList inputs outputs else List.hd candList
    end

let synthesize inputs outputs = synth_h 10 [Zero; One; In] inputs outputs

let _ = (Printf.printf "%s\n") ((show) (synthesize [1;2;3;-1;-2;-3] [1;4;9;2;2;2]))
