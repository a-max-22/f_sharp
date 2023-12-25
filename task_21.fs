// 50.2.1
let fac_seq n = seq {
    for i in 1..n do
        yield fact i
}

// 50.2.2
let seq_seq n = seq {
    for i in 0..n do
        yield ((i / 2) + (i % 2)) * (pown (-1) i)
}
