// 20.3.1
let vat n = fun (x:float) -> x * ( 1.0 + (float n) / 100.0 )

// 20.3.2
let unvat n = fun (x:float) -> x / (1.0 + (float n) / 100.0) 


let rec testmin (f, startValue) = 
    if f(startValue) = 0 then
        startValue
    else
        testmin (f, startValue + 1) 

// 20.3.3
let rec min f = testmin (f,0)
