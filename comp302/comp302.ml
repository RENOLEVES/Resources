(* how to define a type of a function *)
let double x = 2 * x
let app f x = f x
let test x = app app double
let test2 x = app double 
    
(* type check: parameter -> ouput *)
(* If output is a function then ??? -> ???, partial application *)
(* If output is a value then ??? *)
(* double : int -> int *)
(* app: ('a -> 'b) -> 'a -> 'b *)
(* app(double): 'a -> int -> int *)
(* app app double: 'a -> int -> int *)

let rec func f arg = 
  match arg with
  | [] -> []
  | x :: xs -> 
      match x with
      | Some x -> f x :: func f xs
      | Node -> func f xs
                  
(* type check :
   
parameter: f and arg
f is a function takes an element and output an element
arg is a list

parameter: ('a -> 'b) and ('a option list)

return type: f x :: func f xs
which is a list contains all (f x) whose type is 'b
   
return type: 'b list
   
fial type: ('a -> 'b) -> ('a option list) -> ('b list)
  
*)

(* Uppercase for first letter is required in Constructors *)
(* variable starts with lowercase, constructor starts with uppercase*)
type hand = Rock | Paper | Scissors 

let my_hand = Rock
let your_hand = Paper
  
let beats (h1 : hand) (h2 : hand) : bool = (* return type here is optional *)
  match h1 with
  | Rock -> h2 = Scissors
  | Paper -> h2 = Rock
  | Scissors -> h2 = Paper

type outcome = Win | Lose | Draw
  
let play h1 h2 : outcome = 
  match h1 with 
  | Rock ->
      (match h2 with
       | Rock -> Draw
       | Paper -> Lose
       | Scissors -> Win)
  | Paper -> 
      (match h2 with
       | Rock -> Win
       | Paper -> Draw
       | Scissors -> Lose)
  | Scissors ->
      (match h2 with
       | Rock -> Lose
       | Paper -> Win
       | Scissors -> Draw
(*use of wildcard as else case*)
       | _ -> failwith "boom")
      
let play h1 h2 : outcome = 
  if h1 = h2 then Draw else
    match (h1, h2) with 
    | (Rock, Scissors) -> Win
    | (Scissors, Paper) -> Win
    | (Paper, Rock) -> Win
    | _ -> Lose
      
let play h1 h2 : outcome = 
  if h1 = h2 then Draw else
  if beats h1 h2 then Win else
    Lose