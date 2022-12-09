type tree = Char of int | Node of tree * tree

let build_tree f =
  let forest = PriorityQueue.make 256 (Char 0) in
  for i = 0 to 255 do
    if f.(i) > 0
    then PriorityQueue.add (Char i) (-f.(i)) forest
  done;
  while PriorityQueue.length forest > 1 do
    let f_x, x = PriorityQueue.take_full forest in
    let f_y, y = PriorityQueue.take_full forest in
    PriorityQueue.add (Node(x, y)) (f_x+f_y) forest
  done;
  PriorityQueue.take forest

let codes t =
  let t_codes = Array.make 256 [] in
  let rec aux t acc =
    match t with
    | Char c -> t_codes.(c) <- List.rev acc
    | Node(g,d) -> aux g (false :: acc); aux d (true :: acc)
  in aux t [];
  t_codes

let rec output_tree f a =
  match a with
  | Node(x, y) -> begin
      FileIO.output_byte f 0;
      output_tree f x;
      output_tree f y
    end
  | Char c -> begin
      FileIO.output_byte f 1;
      FileIO.output_byte f c
    end

let rec input_tree f =
  let code = FileIO.input_byte f in
  if code = 0
  then begin
    let x = input_tree f in
    let y = input_tree f in
    Node(x, y)
  end else Char (FileIO.input_byte f)


