// 49.5.1
let even_seq = Seq.initInfinite (fun i -> 2* (i + 1))

let rec fact = function 
        | 0 -> 1
        | n when n > 0 ->  n * (fact (n-1))
        | _ -> 0
        
// 49.5.2
let fac_seq = Seq.initInfinite(fun i -> fact i)

// 49.5.3
let seq_seq = Seq.initInfinite(fun i -> ((i / 2) + (i % 2)) * (pown (-1) i) )
