// Exercise 5.1
let sum = 
    let rec sumAcc acc a = function 
        | 0 -> a+acc
        | x -> sumAcc (acc + a + x) a (x-1)

    sumAcc 0;;

// Exercise 5.2
let length lst = 
    let rec lengthAcc acc = function 
        | [] -> acc
        | x::xs -> lengthAcc (acc+1) xs

    lengthAcc 0 lst;;

// Exercise 5.3
let foldBack f lst acc = 
    let rec aux f lst c =
        match lst with
        | [] -> c acc
        | x::xs -> aux f xs (fun bla -> c(f x bla))

    aux f lst id;;

// Exercise 5.4
let factC = 
    let rec aux c = function
        | 0 -> c 1
        | x -> aux (fun a -> c (a*x)) (x-1)

    aux id;;

// Exercise 5.5 
let fibA a = 
    let rec aux n acc1 acc2 = 
        if n < a then 
            aux (n+1) (acc2) (acc1+acc2)
        else
            acc1
    
    aux 0 0 1;;

let fibC = 
    let rec aux c = function
        | 0 -> c 0
        | 1 -> c 1
        | x -> aux (fun a -> c a) (x-1) + aux (fun a -> c a) (x-2)

    aux id;;

// Exercise 5.6
(* The issue here was the the continuation function did not have the append evaluation inside as a parameter but was rather a
 part of the parameter: 
 Before: fun res -> 1 :: c res)
 Now   : fun res -> c (1 :: res))
 *)

let rec bigListK c = function
    | 0 -> c []
    | n -> bigListK (fun res -> c (1 :: res)) (n-1);;


