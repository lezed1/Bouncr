open Printf
module T = ANSITerminal

(* http://stackoverflow.com/a/10789674 *)
let minisleep (seconds:float) =
  ignore (Unix.select [] [] [] seconds)

let x : int ref = ref 120
let y : int ref = ref 1

let x' : int ref = ref (1)
let y' : int ref = ref (-1)


		      
let moveit (s:string) =
  let str_len : int = String.length s in

  let all_colors = [T.Black; T.Red; T.Green; T.Yellow;
		    T.Blue; T.Magenta; T.Cyan; T.White; T.Default] in

  let all_styles = [T.Bold; T.Underlined; T.Blink] in
  
  let color _ : T.color =
    let n = Random.int (List.length all_colors) in
    List.nth all_colors n in

  let last_height = ref 0 in
  
  while true do
    let x_size, y_size = T.size () in

    (* You can uncomment the next line to not overwrite the current screen. 
     * But why would you want to? *)
    
    (* T.scroll (y_size - !last_height); *)
    
    last_height := y_size;
    
    if (!x + str_len - 1) >= x_size then (x' := -1; x := x_size - str_len + 1);
    if !y >= y_size then (y' := -1; y := y_size);
    
    if !x <= 1 then (x' := 1; x := 1);
    if !y <= 1 then (y' := 1; y := 1);

    x := !x + !x';
    y := !y + !y';

    let style = ref [T.Foreground (color ()); T.Background (color ())] in

    style := List.rev_append
	       !style
	       (List.filter (fun _ -> Random.bool ()) all_styles);
    
    T.set_cursor !x !y;

    T.print_string !style s;

    flush stdout;
    minisleep 0.1;
  done

let x = moveit "Happy Birthday Renda";;
