// 34.1
let rec upto n =
    let rec pass (n,i) = 
        match i with
            | 0 -> []
            | _ -> n - i + 1 :: pass(n, i-1)
    pass (n,n)

// 34.2
let rec dnto n = 
    match n with
        | 0 -> []
        | _ -> n :: dnto(n-1)
// 34.3
let rec evenn n = 
    let rec pass1 (n,i) =
        match i with
            | 0 -> []
            | i -> (n - i) :: pass1(n, i - 2)
    pass1 (2*n, 2*n)
