type 'a t = {
  elts : 'a array;
  mutable length : int
}

let make size_max default = 
  { 
    elts = Array.make size_max default; 
    length = 0 
  }

let length heap = heap.length

let father a =
  if a = 0
  then failwith "root"
  else (a+1)/2 - 1

let left a = 2*(a+1) - 1
let right a = 2*(a+1)

let nth heap i = heap.elts.(i)

let swap v x y =
  let t = v.(x) in
  v.(x) <- v.(y);
  v.(y) <- t

exception Overflow
let add x heap =
  if heap.length = Array.length heap.elts
  then raise Overflow;
  let pos = ref heap.length in
  heap.elts.(heap.length) <- x;
  heap.length <- heap.length + 1;
  while !pos > 0 && nth heap (father !pos) < nth heap !pos do
    let p = father !pos in
    swap heap.elts p !pos;
    pos := p
  done

let max_with_sons heap a =
  let current_max = ref a in
  if heap.length > left a 
  && nth heap (left a) > nth heap !current_max
  then current_max := left a;
  if heap.length > right a 
  && nth heap (right a) > nth heap !current_max
  then current_max := right a;
  !current_max

exception Empty
let take heap =
  if heap.length = 0
  then raise Empty;
  heap.length <- heap.length - 1;
  let x = heap.elts.(0) in
  if heap.length > 0
  then begin
    let pos = ref 0 in
    heap.elts.(0) <- heap.elts.(heap.length);
    while max_with_sons heap !pos <> !pos do
      let max_sons = max_with_sons heap !pos in
      swap heap.elts !pos max_sons;
      pos := max_sons
    done
  end;
  x

let peek heap =
  if heap.length = 0
  then raise Empty;
  heap.elts.(0)
