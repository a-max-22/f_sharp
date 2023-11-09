exception TimeExc
type F = 
  | AM
  | PM
  
type TimeOfDay = { hours: int; minutes: int; f: F }

let normalize_time (time:TimeOfDay) = 
    match time.f with
        | AM -> (time.hours, time.minutes)
        | PM -> (time.hours + 12, time.minutes)
        | _ -> raise TimeExc

let (.>.) x y = 
    normalize_time x > normalize_time y
