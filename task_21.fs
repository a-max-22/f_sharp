// 50.2.1
let fac_seq = seq {
    for i in Seq.initInfinite  id do
        yield fact i
}

// 50.2.2
let seq_seq  = seq {
    for i in Seq.initInfinite  id do
        yield ((i / 2) + (i % 2)) * (pown (-1) i)
}
