// 40.1
let rec sum (p, xs) =
    match xs with
        | [] -> 0
        | head::tail -> 
            if p(head) then
                head + sum (p, tail)
            else
                sum (p, tail)

// 40.2.1
let rec count (xs, n) = 
    match xs with 
        | head::tail when head = n -> 1 + count (tail, n)
        | head::tail when head < n  -> count (tail, n)
        | _ -> 0 
        

// 40.2.2
let rec insert (xs, n) = 
    match xs with 
        | head::tail when head < n -> head::insert (tail,n)
        | _ -> n::xs   

// 40.2.3
let rec intersect (xs1, xs2) = 
    match (xs1,xs2) with
        | head1::tail1, head2::tail2 when head1 = head2 -> head1::intersect(tail1,tail2)
        | head1::tail1, head2::tail2 when head1 > head2 -> intersect(xs1, tail2)
        | head1::tail1, head2::tail2 when head1 < head2 -> intersect(tail1, xs2)        
        | _ -> []
        
// 40.2.4
let rec plus (xs1, xs2) = 
    match (xs1,xs2) with
        | head1::tail1, head2::tail2 -> if head1 <= head2 then head1::plus(tail1, xs2) else head2::plus(xs1,tail2)
        | [], head::tail -> xs2
        | head::tail, [] -> xs1
        | _ -> []

// 40.2.5
let rec minus (xs1, xs2) = 
    match (xs1, xs2) with 
        | head1::tail1, head2::tail2 when head1 = head2 -> minus(tail1,tail2)
        | head1::tail1, head2::tail2 -> if head1 < head2 then head1::minus(tail1,xs2) else minus(xs1,tail2)
        | head::tail, [] -> xs1
        | _ -> []


// 40.3.1
let rec smallest1 xs =
    let rec get_smallest (list, cur_smallest) = 
        match list with 
            | head::tail -> if (Option.get cur_smallest ) > head then get_smallest(tail, Some head) else get_smallest(tail, cur_smallest)  
            | [] -> cur_smallest
                    
    match xs with
        | head::tail -> get_smallest(tail, Some head) 
        | [] -> None
    
// 40.3.2
let rec delete (n, xs) = 
  match xs with
    | head::tail when head = n -> tail
    | head::tail -> head::delete(n, tail)
    | _ -> []

// 40.3.3
let rec sort xs =
    let rec sub_sort (xs,sorted) =
        match xs with
            | head::tail ->                    
                    let cur_smallest = smallest1 xs
                    if cur_smallest.IsSome then 
                        sub_sort(delete (cur_smallest.Value, xs), sorted @ [cur_smallest.Value])
                    else
                        sorted
            | [] -> sorted
    sub_sort(xs, [])
    
// 40.4
let rec revrev xxs = 
    match xxs with 
      | head::tail -> revrev(tail) @ ([List.rev head])
      | _ -> []
