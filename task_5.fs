// 16.1
let notDivisible (n,m) =  ( (m % n) = 0 )  

let divisible (n,m) = not ( (m % n) = 0 )  

let rec checkAllDivisors (currentDivisor, n) =  
    match currentDivisor with
        | 2 -> divisible (2, n)
        | currentDivisor -> divisible (currentDivisor, n) && checkAllDivisors(currentDivisor - 1, n)      

// 16.2
let  prime n = 
    match n with
        | 1 -> false
        | 2 -> true
        | n when n > 2 -> checkAllDivisors(n-1, n) 
        | n  -> false 
