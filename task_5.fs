// 16.1
let notDivisible (n,m) = not ( (m % n) = 0 )  

let rec checkAllDivisors (currentDivisor, n) =  
    match currentDivisor with
        | 2 -> notDivisible (2, n)
        | currentDivisor -> notDivisible (currentDivisor, n) && checkAllDivisors(currentDivisor - 1, n)      

// 16.2
let  prime n = 
    match n with
        | 1 -> false
        | 2 -> true
        | n when n > 2 -> checkAllDivisors(n-1, n) 
        | n  -> false 
