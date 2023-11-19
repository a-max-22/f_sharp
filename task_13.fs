exception ListsSizesDiffer

// 39.1
let rec rmodd l = 
    match l with
        | []  -> []
        | [h] -> [] 
        | o :: h :: t -> [h] @ rmodd t


// 39.2
let rec del_even l = 
    match l with
        | h :: t when h % 2 > 0 -> [h] @ del_even t
        | h :: t when h % 2 = 0 -> del_even t
        | _ -> []

// 39.3
let rec multiplicity x xs = 
    match xs with
        | h :: t when h = x -> 1 + multiplicity x t
        | h :: t when not (h = x) -> multiplicity x t
        | _ -> 0

// 39.4
let rec split l = 
    match l with
        | non_odd :: odd :: tail -> 
            let non_odd_tail, odd_tail = split tail 
            ([non_odd] @ non_odd_tail, [odd] @ odd_tail)
        | non_odd :: odd -> ([non_odd] , odd)
        | _ -> ([], [])

// 39.5
let rec zip = function
    | h1 :: t1, h2 :: t2 -> [(h1, h2)] @ zip (t1, t2)
    | [], [] -> []
    | _ -> raise ListsSizesDiffer 
