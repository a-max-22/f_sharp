// 41.4.1
let list_filter f xs = List.foldBack (fun elem rem_lst -> if f elem then elem :: rem_lst else rem_lst) xs []

// 41.4.2
let sum (p, xs) = List.fold (fun rem_lst x -> if p x then rem_lst + x else rem_lst) 0 xs

// 41.4.3
let revrev xxs =
    let rev rem_lst lst = (List.fold (fun head tail -> tail::head) [] lst) :: rem_lst
    List.fold rev [] xxs
