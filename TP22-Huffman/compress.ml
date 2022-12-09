let rec output_code f l =
  match l with
  | [] -> ()
  | b :: q -> FileIO.output_bit f b; output_code f q

let stats fn =
  let occ = Array.make 256 0 in
  FileIO.input_byte_iter 
    (fun i -> occ.(i) <- occ.(i) + 1) fn;
  occ

let compress fn =
  let fout = FileIO.open_out_bits (fn ^ ".mp2zip") in
  let occ = stats fn in
  let t = Huffman.build_tree occ in
  Huffman.output_tree fout t;
  let codes = Huffman.codes t in
  FileIO.input_byte_iter
    (fun i -> output_code fout codes.(i)) fn;
  FileIO.close_out_bits fout
