type 'a t = (int * 'a) Heap.t

let make size_max default =
  Heap.make size_max (0, default)

let length q = Heap.length q
let add x p q = Heap.add (p, x) q
let take_full q = Heap.take q
let take q = snd (take_full q)
let peek q = snd (Heap.peek q)
