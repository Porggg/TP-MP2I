type in_channel_bits = {
  i_file : in_channel;
  mutable i_acc : int;
  mutable i_acc_bits : int;
  i_length : int
}

let open_in_bits fn =
  let file = open_in_bin fn in
  { i_file = file; 
    i_acc = 0; i_acc_bits = 0; i_length = in_channel_length file }

let input_bit f =
  if f.i_acc_bits = 0
  then begin
    f.i_acc <- input_byte f.i_file;
    f.i_acc_bits <- 8;
    if pos_in f.i_file = f.i_length - 1
    then begin
      let pad = input_byte f.i_file in
      f.i_acc_bits <- f.i_acc_bits - pad
    end
  end;
  let bit = f.i_acc mod 2 = 1 in
  f.i_acc <- f.i_acc / 2;
  f.i_acc_bits <- f.i_acc_bits - 1;
  bit

let close_in_bits f = close_in f.i_file

let input_byte_iter f fn =
  let f_in = open_in_bin fn in
  begin
    try
      while true do
        let c = input_byte f_in in
        f c
      done
    with End_of_file -> ()
  end;
  close_in f_in

type out_channel_bits = {
  o_file : out_channel;
  mutable o_acc : int;
  mutable o_acc_bits : int
}

let open_out_bits fn =
  { o_file = open_out_bin fn; o_acc = 0; o_acc_bits = 0 }

let output_bit f b =
  if f.o_acc_bits = 8
  then begin
    output_byte f.o_file f.o_acc;
    f.o_acc <- 0;
    f.o_acc_bits <- 0
  end;
  if b
  then f.o_acc <- f.o_acc + 1 lsl f.o_acc_bits;
  f.o_acc_bits <- 1 + f.o_acc_bits

let close_out_bits f =
  if f.o_acc_bits = 0
  then output_byte f.o_file 0
  else begin
    let padding = 8 - f.o_acc_bits in
    output_byte f.o_file (Int.shift_left f.o_acc padding);
    output_byte f.o_file padding
  end;
  close_out f.o_file

(* Attention au masquage potentiel *)
let input_byte f = input_byte f.i_file
let output_byte f = output_byte f.o_file
