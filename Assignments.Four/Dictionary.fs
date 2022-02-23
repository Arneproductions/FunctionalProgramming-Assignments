module Dictionary
    type Dict = 
            | Leaf of bool
            | Node of (Map<char, Dict> * bool)

    let empty () = Leaf false

    let rec insert (str:string) = function
        | Leaf _ when System.String.IsNullOrEmpty str      -> Leaf true
        | Leaf a                                           -> Node (Map.ofList [(str.[0], insert (str.Substring 1) (empty ()))], a)
        | Node (a, _) when System.String.IsNullOrEmpty str -> Node (a, true)
        | Node (a, b) when a.ContainsKey str.[0]           -> Node (a.Add (str.[0], Map.find str.[0] a |> insert (str.Substring 1) ), b)
        | Node (a, b)                                      -> Node (a.Add (str.[0], insert (str.Substring 1) (empty ())), b)

    let rec lookup str = function
        | Leaf _ when not (System.String.IsNullOrEmpty str)      -> false
        | Leaf a                                                 -> a
        | Node (a, b) when not (System.String.IsNullOrEmpty str) -> if a.ContainsKey str.[0] then 
                                                                        Map.find str.[0] a |> lookup (str.Substring 1) 
                                                                    else false
        | Node (a, b) -> b

    let step ch = function
        | Leaf _                                    -> Option.None
        | Node (a, b) when not (a.ContainsKey ch)   -> Option.None
        | Node (a, b)                               -> Map.find ch a |> function
                                                                        | Leaf x      -> Option.Some (x, Leaf x)
                                                                        | Node (y, x) -> Option.Some (x, Node (y,x))