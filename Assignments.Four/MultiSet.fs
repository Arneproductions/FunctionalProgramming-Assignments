module MultiSet
    type MultiSet<'a when 'a : comparison> = MS of Map<'a, uint32> 

    let empty = MS Map.empty<'a, uint32>

    let isEmpty = function  
        | MS a -> a.IsEmpty

    let size (MS a) = Map.fold (fun acc _ value -> acc + value) 0u a

    let contains s (MS a) = a.ContainsKey s
        
    let numItems s (MS a) = a.TryFind s |> Option.defaultValue 0u 

    let add key (value:uint32) (MS set) = MS (Map.add key ((numItems key (MS set)) + value) set)

    let addSingle key set = add key 1u set

    let remove key value set =
        let numLeft = numItems key set
        match set with
        | MS s -> if numLeft <= value then MS (s.Remove key) else MS (Map.add key (numLeft-value) s)

    let removeSingle key set = remove key 1u set

    let fold f acc (MS s) =  Map.fold f acc s

    let foldBack f (MS s) acc =  Map.foldBack f s acc