// 47.4.1
let f n =
    let mutable i = 1
    let mutable res = 1
    while i < n do
        i <- i + 1
        res <- res * i
    res

// 47.4.2
let fibo n =
    if n = 0 then 0
    else
        let mutable curr = 1
        let mutable prev = 0
        let mutable i = 0
        while i < n - 1 do
            let tmp = prev + curr
            prev <- curr
            curr <- tmp
            i <- i + 1
        curr
