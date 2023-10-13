// 7.1.1
let rec fibo = function 
    | 0 -> 0
    | 1 -> 1
    | n when n > 0 -> fibo(n-1) + fibo(n-2)
    | _ -> 0

// 7.1.2
let rec sum = function
    | 0 -> 0
    | (n : int) when n > 0  -> sum(n-1) + n
    | _ -> 0

// 7.1.3
let rec sum2 = function 
    | (m,0) -> m 
    | (m,n) when m >= 0 && n >= 0 -> m + n + sum2(m, n - 1)
    | _ -> 0
    
