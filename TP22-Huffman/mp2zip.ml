let main =
  if Array.length Sys.argv < 3 
  || (Sys.argv.(1) <> "comp" && Sys.argv.(1) <> "decomp")
  then Printf.printf "Usage: mp2zip comp/decomp file\n"
  else if Sys.argv.(1) = "comp"
  then Compress.compress Sys.argv.(2)
  else Decompress.decompress Sys.argv.(2)
