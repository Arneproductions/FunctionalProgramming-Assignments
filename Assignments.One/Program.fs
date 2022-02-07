// Exercise 1.1
let sqr x = x*x;;

// Exercise 1.2
let pow x n = System.Math.Pow(x, n);;

// Exercise 1.3
let rec sum = function
    | 0 -> 0
    | n -> sum (n-1) + n;;

// Exercise 1.4
let rec fib = function
    | 0 -> 0
    | 1 -> 1
    | n -> fib (n-1) + fib(n-2);;

// Exercise 1.5
let dup value:string = value+value;;

// Exercise 1.6
let rec dupn (value:string) times = 
    if times = 1 then value
    else value + dupn value (times-1);;

// Exercise 1.7
let rec bin = function
    | n, 0 -> 1
    | n, k when n = k -> 1
    | n, k -> bin(n-1, k-1)+bin(n-1, k);;

// Exercise 1.8
let timediff (h1, m1) (h2, m2) = (h2*60+m2)-(h1*60+m1);;

// Exercise 1.9
let minutes time = timediff (0, 0) time;;

// Exercise 1.10
let curry f x y = f(x, y);;

let uncurry f (x,y) = f x y;;

// 1.11
let empty (letter:char, pointValue:int) = fun (pos:int) -> (letter, pointValue);;
// let letterA = empty('A', 1);;  

// 1.12
let add (newPos:int) (cv:(char * int)) word = fun pos -> if pos = newPos then cv else word newPos;;