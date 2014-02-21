open Unix
open Printf

type message = {
  message_type: string;
  body: string }

type client_message_type =
  | LISP
  | LISP_MV
  | JSON
  | JSON_MV

type acl2_bridge = {
  ready: unit -> bool;
  address: sockaddr;
  socket: file_descr;
  receive: unit -> message;
  receive_raw: unit -> string;
  send: client_message_type -> string -> unit;
  send_raw: string -> unit;
  close: unit -> unit }

let make_message (message_type: string) (body: string) =
  let length = string_of_int (String.length body) in
  sprintf "%s %s\n%s\n" message_type length body

let init (address: sockaddr)  =
  let ready = ref false in
  let socket = socket PF_UNIX SOCK_STREAM 0 in
  connect socket address;
  let receive () =
    if !ready then
      failwith "Cannot receive when the server is in ready state." 
    else
      let buffer = ref (String.create 1) in
      let next () =
	if recv socket !buffer 0 1 [] = 0 then
	  failwith "Something happened"
	else
	  !buffer in
      let receive_header (): string * int =
	let (typ, length) = (ref "", ref "") in
	while next () <> " " do
	  typ := !typ ^ !buffer
	done;
	while next () <> "\n" do
	  length := !length ^ !buffer
	done;
	(!typ, int_of_string !length) in
      let receive_body (message_type, length) =
	let body = (String.create length) in
	ignore (recv socket body 0 length []);
	ignore (next ());
	body in
      let (message_type, length) = receive_header () in
      let body = receive_body (message_type, length) in
      if message_type = "READY" then ready := true;
      { message_type = message_type;
	body = body } in
  let receive_raw () =
    let message = receive () in
    make_message message.message_type message.body in
  let send_raw message =
    while not !ready do
      ignore (receive ())
    done;
    ignore (send socket message 0 (String.length message) []);
    ready := false in
  let send typ body =
    let type_string = match typ with
      | LISP -> "LISP"
      | LISP_MV -> "LISP_MV"
      | JSON -> "JSON"
      | JSON_MV -> "JSON_MV" in
    let message = make_message type_string body in
    send_raw message in
  { ready = (fun () -> !ready);
    address = address;
    socket = socket;
    receive = receive;
    receive_raw = receive_raw;
    send = send;
    send_raw = send_raw;
    close = (fun () -> close socket) }
