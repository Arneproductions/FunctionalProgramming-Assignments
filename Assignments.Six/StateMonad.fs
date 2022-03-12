module StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun _ -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> = S(fun s -> Success((), {s with vars = List.removeAt 0 s.vars}))     

    let wordLength : SM<int> = S(fun s -> Success(s.word.Length, s))      

    let characterPair (pos: int) = 
        wordLength >>= (fun a -> if a > pos && pos >= 0 then S(fun s -> Success(s.word.[pos], s))
                                 else fail (IndexOutOfBounds pos))

    let characterValue (pos: int) : SM<char> = 
        characterPair pos >>= (fun a -> ret (fst a))
        

    let pointValue (pos : int) : SM<int> = 
        characterPair pos >>= (fun a -> ret (snd a))      

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> = 
        let isDeclared (s: State) = Set.contains var s.reserved

        S(fun s -> if not(isDeclared s) then 
                        if not(s.vars.Head.ContainsKey var) then Success((), {s with vars = List.updateAt 0 (Map.add var 0 s.vars.Head) s.vars})
                        else Failure (VarExists var)
                   else Failure (ReservedName var))


    let update (var : string) (value : int) : SM<unit> = 
        S (fun t -> 
              match List.tryFindIndex (fun x -> Map.containsKey var x) t.vars with
              | Some v -> Success ((), {t with vars = List.updateAt v (Map.add var value t.vars.[v]) t.vars})
              | None   -> Failure (VarNotFound var))      



              

    