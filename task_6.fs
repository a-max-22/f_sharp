// 17.1
let rec pow (string, n) = 
    match n with 
        | 1 -> string
        | n when n > 1 ->   string + pow(string, n-1)
        | _ -> ""

// 17.2
let rec isIthChar(str, index, char) = 
    if ((0 <= index) && ( index < (String.length str))) then 
        str.[index] = char
    else
        false

// 17.3
let rec occFromIth(str, index, char) = 
    match (String.length str - index) with
        | 0 -> 0
        | n when (n > 0) && (n < String.length str) -> System.Convert.ToInt32(str.[index] = char) + occFromIth(str, index + 1, char)
        | _ -> 0
