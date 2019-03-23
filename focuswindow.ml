open Core

let command_to_string command =
  Stdio__In_channel.input_all (Core__Core_unix.open_process_in command)

let get_window_focus () =
  String.strip @@ command_to_string "xdotool getwindowfocus"

let get_desktop () =
  Int.of_string @@ String.strip @@ command_to_string "xdotool get_desktop"

(* let getwindows (current_window) = *)
(* tmpws=$(xdotool search --onlyvisible --desktop $CURRENT_DESKTOP --classname "^.+$") *)
let get_windows desktop =
  let result = command_to_string @@ "xdotool search --onlyvisible --desktop " ^ (Int.to_string desktop) ^ " --classname \"^.+$\"" in
  String.split_lines result

type windowsize = {
    x: int;
    y: int;
    w: int;
    h: int;
}

exception Command_err

let get_windowsize window =
  let result = String.strip @@ command_to_string @@ "eval $(xdotool getwindowgeometry --shell "
                                                    ^ window
                                                    ^ " ); echo $X $Y $WIDTH $HEIGHT" in
  match (String.split result ~on:' ') with
    [x; y; w; h] -> {x=(Int.of_string x);
                     y=(Int.of_string y);
                     w=(Int.of_string w);
                     h=(Int.of_string h)}
  | _ -> raise Command_err

(* ２つのwindowの角度 *)
let theta_of_windows w1 w2 =
  let w1 = get_windowsize w1 in
  let w2 = get_windowsize w2 in
  let center1_x = w1.x + (w1.w / 2) in
  let center1_y = w1.y + (w1.h / 2) in
  let center2_x = w2.x + (w2.w / 2) in
  let center2_y = w2.y + (w2.h / 2) in
  Float.atan2 (Int.to_float (center2_y - center1_y))
              (Int.to_float (center2_x - center1_x))

let square_distance_of_windows w1 w2 =
  let w1 = get_windowsize w1 in
  let w2 = get_windowsize w2 in
  let center1_x = w1.x + (w1.w / 2) in
  let center1_y = w1.y + (w1.h / 2) in
  let center2_x = w2.x + (w2.w / 2) in
  let center2_y = w2.y + (w2.h / 2) in
  ((center1_x - center2_x) * (center1_x - center2_x))
  +
  ((center1_y - center2_y) * (center1_y - center2_y))
    
  

let r2d r =
  let d = r *. (180.0 /. Float.pi) in
  ((Float.to_int d) + 360) % 360

type direction = Up
               | Down
               | Left
               | Right

let degree_to_direction d =
  if 45 <= d && d < 135 then
    Down
  else if 135 <= d && d < 225 then
    Left
  else if 225 <= d && d < 315 then
    Up
  else
    Right

let show d =
  match d with
    Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"

let check_args () =
  if (Array.length Sys.argv) = 1 then
    Up
  else
    let d = Sys.argv.(1) in
    let to_d = fun _d -> match _d with
                           "up" -> Up
                         | "down" -> Down
                         | "left" -> Left
                         | "right" -> Right
                         | _ -> Up in
    to_d d

let () =
  let d = check_args () in
  let focusw = get_window_focus () in
  let others = List.filter (get_windows (get_desktop ())) ~f:(fun (w) ->
                           if w = focusw then false else true) in
  let directed_windows = List.filter others ~f:(fun (w) ->
                                     let wd = degree_to_direction @@ r2d @@ theta_of_windows focusw w in
                                     wd = d) in
  let sorted = List.sort ~compare:(fun w1 w2 ->
                           let w1_distance = square_distance_of_windows focusw w1 in
                           let w2_distance = square_distance_of_windows focusw w2 in
                           if w1_distance = w2_distance then
                             0
                           else if w1_distance > w2_distance then
                             1
                           else
                             -1
                         ) directed_windows in
  begin
    match sorted with
      w :: _ ->
       let _ = command_to_string @@ "xdotool windowactivate " ^ w ^ " windowraise " ^ w in
       ()
    | _ -> ()
  end
  
