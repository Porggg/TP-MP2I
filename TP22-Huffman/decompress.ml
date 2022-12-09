let rec input_code fin t = 
  match t with
  | Huffman.Char i -> i
  | Huffman.Node(x, y) ->
    let b = FileIO.input_bit fin in
    if b 
    then input_code fin y 
    else input_code fin x

let decompress fn =
  let fn_base = String.sub fn 0 (String.length fn - 7) in
  let fin = FileIO.open_in_bits fn in
  let fout = open_out_bin fn_base in
  let t = Huffman.input_tree fin in
  try
    while true do
      let c = input_code fin t in
      output_byte fout c
    done
  with End_of_file -> ();
    FileIO.close_in_bits fin;
    close_out fout

