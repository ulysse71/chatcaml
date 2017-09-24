
type processT = Server of int | Client of (string * int)

let warning str =
  print_endline str;
  flush_all ()

(*****************************************)
(** graphical part **)
let affString ?(color=Graphics.black) x y str =
  Graphics.moveto x y;
  Graphics.set_color color;
  Graphics.draw_string str

let rec affLines l i j dj n =
  match l with
  | str :: l ->
    affString i (j - dj) str;
    if pred n > 1 then affLines l i (j - dj) dj (pred n)
  | [] -> ()

let affChat lu ld maxy nl =
  Graphics.clear_graph ();
  affLines lu 4 (maxy - 10) (maxy / nl) 10;
  affLines ld 4 (maxy / 2 - 10) (maxy / nl) 10;
  Graphics.synchronize ()

let rec readString x y str =
  affString x y str;
  let ch = Graphics.read_key () in
  if ch = '\r' || ch = '\n' then str
  else if ch = '\b' then readString x y (String.sub str 0 (max (String.length str - 1) 0))
  else readString x y (str ^ String.make 1 ch)

let getLocal x y =
  if Graphics.key_pressed () then (
    let str = readString x y "" in
    Some str
  )
  else None
 
(*****************************************)
(** networking part **)

let initSocketServer port =
  Graphics.set_window_title "server";
  Printf.ksprintf warning "initializing server socket on port %d" port;
  let sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let inet_addr = (Unix.gethostbyname (Unix.gethostname ())).Unix.h_addr_list.(0) in
  Unix.bind sock (Unix.ADDR_INET (inet_addr, port));
  Unix.listen sock 2;
  let fd0, _ = Unix.accept sock in
  let fd1, _ = Unix.accept sock in
  fd0, fd1

let initSocketClient server port =
  Printf.ksprintf Graphics.set_window_title "client to %s" server;
  Printf.ksprintf warning "initializing client socket for machine %s on port %d" server port;
  let fd0 = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let fd1 = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let inetaddr = (Unix.gethostbyname server).Unix.h_addr_list.(0) in
  Unix.connect fd0 (Unix.ADDR_INET (inetaddr, port));
  Unix.connect fd1 (Unix.ADDR_INET (inetaddr, port));
  fd0, fd1

let getDistant (process, (fd0, fd1)) =
  let ns = 256 in
  let str = String.make ns ' ' in
  let fdi = match process with | Server _ -> fd0 | Client _ -> fd1 in
  let fl = try Unix.select [fdi] [] [] 0.001 with _ -> [], [], [] in
  match fl with
  | (fdi :: _), _, _ ->
    let retv = try Unix.recv fdi str 0 ns [] with _ -> 0 in
    if retv > 0 then Some str
    else None
  | [], _, _ -> None

let putDistant (process, (fd0, fd1)) str =
  let fdo = match process with | Server _ -> fd1 | Client _ -> fd0 in
  if Unix.send fdo str 0 (String.length str) [] < 0 then Printf.printf "send failed\n"
  

(*****************************************)
(** help **)

let help () =
  Printf.printf "
#grepman
# usage:
#   chat client <servername> <port>
#     start a client connecting to <servername>:<port>
#   chat server <port>
#     start a server on local host on port <port>
#
";
  exit 255

(*****************************************)
(** main loop **)

let _ =
  let dx = 800 and dy = 600 in
  let count = 1 in
  let process =
    if Array.length Sys.argv > 1 then
      match Sys.argv.(count) with
      | "client" -> Client (Sys.argv.(count+1), int_of_string Sys.argv.(count+2))
      | "server" -> Server (int_of_string Sys.argv.(count+1))
      | _ -> help ()
    else Server 1025 in

  let sock =
    match process with
    | Server port -> process, initSocketServer port
    | Client (server, port) -> process, initSocketClient server port in

  ignore (Printf.ksprintf Graphics.open_graph " %dx%d" dx (dy + 32));
  let rec loop lstLoc lstDst =
    (* asymmetric communication for begining, change *)
    match getLocal 4 (dy - 20) with
    | Some str ->
      if str <> "quit" && str <> "exit" then (
	putDistant sock str;
	let lstLoc = str :: lstLoc in
	affChat lstLoc lstDst dy 20;
	loop lstLoc lstDst 
      )
    | None -> (
      match getDistant sock with
      | Some str ->
        let lstDst = str :: lstDst in
        affChat lstLoc lstDst dy 20;
	loop lstLoc lstDst
      | None -> loop lstLoc lstDst
    ) in

  loop [] []

