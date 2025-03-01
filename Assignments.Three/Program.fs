﻿type aExp =
    | N of int              // Integer value
    | V of string           // Variable 
    | WL                    // Word Length
    | PV of aExp            // Point value of character at specfic word index
    | Add of aExp * aExp    // Addition
    | Sub of aExp * aExp    // Subtraction
    | Mul of aExp * aExp;;  // Multiplication

let (.+.) a b = Add (a, b);;
let (.-.) a b = Sub (a, b);;
let (.*.) a b = Mul (a, b);;

// Exercise 3.1
let rec arithEvalSimple = function
    | N n -> n
    | Add (aExp', bExp') -> (arithEvalSimple aExp') + (arithEvalSimple bExp')
    | Sub (aExp', bExp') -> (arithEvalSimple aExp') - (arithEvalSimple bExp')
    | Mul (aExp', bExp') -> (arithEvalSimple aExp') * (arithEvalSimple bExp');;

// let a7 = N 4 .+. (V "y" .-. V "z");;

// Exercise 3.2 
(*
    Credit to the lecture 3 about arithmetic expressions. 
    This is heavily inspired by the slides from that lecture
*)
let binop f x y s = f (x s) (y s);;
let rec arithEvalState = function
    | N n -> fun _ -> n
    | V n -> Map.tryFind n >> Option.defaultValue 0 
    | Add (aExp', bExp') -> binop (+) (arithEvalState aExp') (arithEvalState bExp')
    | Sub (aExp', bExp') -> binop (-) (arithEvalState aExp') (arithEvalState bExp')
    | Mul (aExp', bExp') -> binop (*) (arithEvalState aExp') (arithEvalState bExp');;

// Exercise 3.3
let arithSingleLetterScore = PV (V "_pos_") .+. (V "_acc_");;
let arithDoubleLetterScore = ((N 2) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithTripleLetterScore = ((N 3) .*. PV (V "_pos_")) .+. (V "_acc_");;
let arithDoubleWordScore = N 2 .*. V "_acc_";;
let arithTripleWordScore = N 3 .*. V "_acc_";;

type word = (char * int) list;;

let hello = [('H', 4); ('E', 1); ('L', 1); ('L', 1); ('O', 1); ];;

let rec arithEval a (w:word) = 
    match a with
    | N n -> fun _ -> n
    | V n -> Map.tryFind n >> Option.defaultValue 0 
    | WL -> fun _ -> w.Length
    | PV n -> fun q -> List.item (arithEval n w q) w  |> fun (_,b) -> b
    | Add (aExp', bExp') -> binop (+) (arithEval aExp' w) (arithEval bExp' w)
    | Sub (aExp', bExp') -> binop (-) (arithEval aExp' w) (arithEval bExp' w)
    | Mul (aExp', bExp') -> binop (*) (arithEval aExp' w) (arithEval bExp' w);;


// Exercise 3.4
type cExp =
    | C of char (* Character value *)
    | ToUpper of cExp (* Converts lower case to upper case character, non-letters are unchanged *)
    | ToLower of cExp (* Converts upper case to lower case character, non-letters are unchanged *)
    | CV of aExp;; (* Character lookup at word index *)

let rec charEval ch wd s = 
    match ch with
    | C c       -> c
    | ToUpper c -> charEval c wd s |> System.Char.ToUpper 
    | ToLower c -> charEval c wd s |> System.Char.ToLower
    | CV a      -> List.item (arithEval a wd s) wd |> fun (b, _) -> b;;

// Exercise 3.5 
type bExp =
    | TT (* true *)
    | FF (* false *)
    | AEq of aExp * aExp (* numeric equality *)
    | ALt of aExp * aExp (* numeric less than *)
    | Not of bExp (* boolean not *)
    | Conj of bExp * bExp (* boolean conjunction *)
    | IsDigit of cExp (* check for digit *)
    | IsLetter of cExp (* check for letter *)
    | IsVowel of cExp (* check for vowel *)
    | IsEven of aExp (* check if the result of arithmic exp is even *)

let (~~) b = Not b
let (.&&.) b1 b2 = Conj (b1, b2)
let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2) (* boolean disjunction *)

let (.=.) a b = AEq (a, b)
let (.<.) a b = ALt (a, b)
let (.<>.) a b = ~~(a .=. b) (* numeric inequality *)
let (.<=.) a b = a .<. b .||. ~~(a .<>. b) (* numeric less than or equal to *)
let (.>=.) a b = ~~(a .<. b) (* numeric greater than or equal to *)
let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)

let isVowel c = 
    match System.Char.ToUpper c with 
    | 'A' | 'E' | 'I' | 'O' | 'U' -> true
    | _ -> false;;

let rec boolEval exp wd = 
    match exp with
    | TT            -> fun _ -> true
    | FF            -> fun _ -> false
    | AEq (a ,b)    -> binop (=) (arithEval a wd) (arithEval b wd)
    | ALt (a, b)    -> binop (<) (arithEval a wd) (arithEval b wd)
    | Not a         -> boolEval a wd >> not
    | Conj (a, b)   -> binop (&&) (boolEval a wd) (boolEval b wd)
    | IsDigit a     -> charEval a wd >> System.Char.IsDigit
    | IsLetter a    -> charEval a wd >> System.Char.IsLetter
    | IsVowel a     -> charEval a wd >> isVowel
    | IsEven a      -> arithEval a wd >> (%) 2 >> (=) 0

// Exercise 3.6
let isConsonant c = Not (IsVowel (c));;

// Exercise 3.7
type stmnt =
    | Skip                        (* does nothing *)
    | Ass of string * aExp        (* variable assignment *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)    
    | While of bExp * stmnt;;     (* while statement *)

let rec evalStmnt s wd st = 
    match s with
    | Skip            -> st
    | Ass (a, b)      -> Map.add a (arithEval b wd st) st
    | Seq (a, b)      -> evalStmnt a wd st |> evalStmnt b wd
    | ITE (a, b, c)   -> if boolEval a wd st then evalStmnt b wd st else evalStmnt c wd st
    | While (a, b)    -> if boolEval a wd st then evalStmnt b wd st |> evalStmnt s wd else st;;

// Exercise 3.8
type squareFun = word -> int -> int -> int;;

let stmntToSquareFun stmnt: squareFun = fun wd (pos:int) (acc:int) -> evalStmnt stmnt wd (Map.ofList [("_pos_", pos); ("_acc_", acc)]) |> Map.find "_result_";;

// Exercise 3.9
let oddConsonants = (Seq ((While ((V "I" .<. WL),  
                        ITE (isConsonant (CV (V "i")), 
                            Ass("numC", (V "numC" .+. N 1)), 
                            Skip))),
                        ITE (~~ (IsEven (V "numC")), 
                            Ass("_result_", V "_result_" .-. (V "_result_" .*. N 2)),
                            Skip)));;

// Exercise 3.10
type square = (int * squareFun) list;;
type square2 = (int * stmnt) list;;
