
let format_money_value x = 
  let max_coppers = 12
  let max_silvers = 20
  let (golds, silvers, coppers) = x
  let coppers_formatted = coppers % max_coppers
  let silvers_formatted = silvers + (coppers / max_coppers)
  let golds_formatted = golds + (silvers_formatted / max_silvers)
  (golds_formatted, silvers_formatted % max_silvers, coppers_formatted)
  
// 23.4.1
let (.+.) x y = 
    let (g1, s1, m1) = x
    let (g2, s2, m2) = y
    format_money_value (g1 + g2, s1 + s2, m1 + m2)
    
let (.-.) x y = 
    let (g1, s1, m1) = x
    let (g2, s2, m2) = y
    format_money_value (g1 - g2, s1 - s2, m1 - m2)

// 23.4.2
let (.+) x y = 
  let (a, b) = x
  let (c, d) = y
  (a + c, b + d)

let (.-) x y =
  let (a, b) = y
  x .+ (-a, -b)

let (.*) x y =
  let (a, b) = x
  let (c, d) = y
  (a*c - b*d, b*c + a*d)

let (./) x y =
  let (a, b) = y
  x .* (a / (a*a + b*b), -b / (a*a + b*b))
