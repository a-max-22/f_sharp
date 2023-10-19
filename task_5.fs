// 16.1
let notDivisible (n,m) = ( (m % n) = 0 )  

let checkAllDivisors (n, currentDivisor) = 
    match currentDivisor with
        | 2 -> notDivisible (2, n)
        | currentDivisor -> notDivisible (currentDivisor, n) || notDivisible (currentDivisor - 1, n)      

// 16.2
let  prime n = function
    | 1 -> false
    | 2 -> true
    | n -> checkAllDivisors(n-1, n) 
