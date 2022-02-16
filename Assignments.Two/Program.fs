// Exercise 2.1
let downto1 n = 
    if n = 0 then []
    else [ n .. -1 .. 1 ];;
    
let rec downto2 = function
    | n when n < 1 -> []
    | n -> n::downto2(n-1);;

// Exercise 2.2
let rec removeOddIdx = function
    | x::_::xs -> x :: removeOddIdx xs
    | x::xs -> x :: removeOddIdx xs
    | [] -> [];;

// Exercise 2.3
let rec combinePair = function 
    | [] -> []
    | _::[] -> []
    | x::y::xs -> (x, y) :: combinePair xs;;

// Exercise 2.4
type complex = float * float;;

let mkComplex x y = complex (x, y);;

let complexToPair (x:complex): float * float = x;;

let (|+|) ((x1, y1): complex) ((x2, y2): complex): complex = ((x1+x2), (y1+y2));;

let (|*|) ((x1, y1): complex) ((x2, y2): complex) : complex = complex(x1*x2 - y1*y2, y1*x2 + y2*x1);;

let (|-|) ((x1, y1): complex) ((x2, y2): complex): complex = complex(x1-x2, y1-y2);;

let (|/|) (x: complex) ((x2, y2): complex): complex = 
    (x |*| (x2/(x2**2.0+y2**2.0), -y2/(x2**2.0+y2**2.0)));;

// Exercise 2.5
let explode1 (a: string) = List.ofArray(a.ToCharArray());;

let rec explode2 = function
    | "" -> []
    | a -> a.[0] :: explode2 (a.Remove(0,1));;

// Exercise 2.6
let implode (a: char list) = List.foldBack (fun s acc -> string s + acc ) a "";;

let implodeRev (a: char list) = List.fold (fun acc s -> string s + acc) "" a;;

// Exercise 2.7
let toUpper = explode2 >> List.map (fun c -> System.Char.ToUpper(c)) >> implode;;

// Exercise 2.8
let rec ack = function
    | (0, n) -> n+1
    | (m, n) when m > 0 && n = 0 -> ack(m-1, 1)
    | (m, n) when m > 0 && n > 0 -> ack(m-1, ack(m, n-1));;

// Exercise 2.9
 
let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start);;

let timeArg1 f a = 
    time (fun () -> f a);;

// Exercise 2.10 (Credit til min egen borup dreng)
let rec downto3 f n e =
    if n > 0 then 
        let rec evalf f n e = function
            | i when i < n -> (evalf f n e (i+1)) |> f i
            | _ -> f n e

        evalf f n e 1
    else 
        e

let fac n = downto3 (*) n 1;;
let range a n = downto3 (fun b c -> a b::c) n [];;
