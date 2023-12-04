exception InvalidParam

// 42.3
let rec allSubsets n k =
    match k with
        | 0 -> set [Set.empty]
        | k when k < 0 || k > n -> raise InvalidParam
        | k when k = n -> set [set [1 .. n]]
        | k  -> Set.union ( Set.map (Set.add n) (allSubsets (n-1) (k-1))) (allSubsets (n-1) k)
