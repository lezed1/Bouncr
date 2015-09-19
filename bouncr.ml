open Printf
module T = ANSITerminal

let minisleep (seconds:float) =
  ignore (Unix.select [] [] [] seconds)

let x : int ref = ref 120
let y : int ref = ref 1

let x' : int ref = ref (1)
let y' : int ref = ref (-1)
		      
let moveit (s:string) =
  let str_len : int = String.length s in
  
  while true do
    let x_size, y_size = T.size () in

    if (!x + str_len - 1) >= x_size then (x' := -1; x := x_size - str_len + 1);
    if !y >= y_size then (y' := -1; y := y_size);
    
    if !x <= 1 then (x' := 1; x := 1);
    if !y <= 1 then (y' := 1; y := 1);

    x := !x + !x';
    y := !y + !y';
    T.erase T.Screen;
    T.set_cursor !x !y;
    print_string s;
    flush stdout;
    minisleep 0.1;
  done

let x = moveit "Happy Birthday Renda";;
