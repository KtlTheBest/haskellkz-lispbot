module type StringT = sig val x : string end
module type TokenT = sig val token : string end

module Token(Tok: StringT) : TokenT = struct
  let token = Tok.x
end

module LwtHttpBot(Token: TokenT) = struct
  open Lwt
  open Cohttp
  open Cohttp_lwt_unix

  type debug = Debug | NoDebug
  let debug_mode = ref NoDebug
  let set_debug m =
    debug_mode := m

  let switch_debug_on () = set_debug Debug
  let switch_debug_off () = debug_mode := NoDebug

  let with_forced_debug f =
    let prev_debug_mode = !debug_mode in
    switch_debug_on ();
    f ();
    set_debug prev_debug_mode

  type msg_contents =
    | Text of string
    | UnknownContents
  [@@deriving show]

  type message_upd = {
    update_id: int;
    message_id: int;
    chat_id: int;
    from_user_id: int;
    from_user_first_name: string;
    from_user_last_name: string option;
    contents: msg_contents;
  } [@@deriving show]

  type update =
    | Message of message_upd
    | Unknown of string
  [@@deriving show]

  let try_construct_message_update j =
    let open BatPervasives in
    let open Yojson.Basic.Util in
    let ok = j |> member "ok" |> to_bool in
    (match ok with
    | false -> [Unknown (to_string j)]
    | true ->
      let result = j |> member "result" |> to_list in
      let parse_result_item j =
        Printf.printf "%s\n" (Yojson.Basic.pretty_to_string j);
        let update_id = j |> member "update_id" |> to_int in
        let message = j |> member "message" in
        let chat = message |> member "chat" in
        let chat_id = chat |> member "id" |> to_int in
        let message_id = message |> member "message_id" |> to_int in
        let from = message |> member "from" in
        let id = from |> member "id" |> to_int in
        let first_name = from |> member "first_name" |> to_string in
        let last_name = from |> member "last_name" |> to_string_option in
        let contents =
          let text = message |> member "text" |> to_string_option in
          (match text with
          | None -> UnknownContents
          | Some(s) -> Text(s))
        in

        Message({ update_id = update_id;
                  message_id = message_id;
                  chat_id = chat_id;
                  from_user_id = id;
                  from_user_first_name = first_name;
                  from_user_last_name = last_name;
                  contents = contents
                })
      in
      let results = List.map parse_result_item result in
      print_endline @@ String.concat "\n" (List.map show_update results);
      results
    )

  let make_get_basic s =
    Client.get (Uri.of_string s) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (match !debug_mode with
    | Debug -> begin
        Printf.printf "Response code: %d\n" code;
        Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
        Printf.printf "Body: %s\n" body
      end
    | NoDebug -> ());
    body

  let merge_headers ha hb =
    Cohttp.Header.fold 
      (fun a b acc -> Cohttp.Header.add acc a b) 
      ha 
      hb

  let json_header = Cohttp.Header.of_list ["Content-type", "application/json"]
  let token = Token.token
  let api_prefix = Printf.sprintf "https://api.telegram.org/bot%s" token
  let api_method s = Printf.sprintf "%s/%s" api_prefix s
  let api_method_with_args s args =
    Printf.sprintf "%s/%s?%s" api_prefix s
    (String.concat "&" (List.map (fun (a, b) -> a ^ "=" ^ b) args))

  let getMe =
    let getMe_string = Printf.sprintf "https://api.telegram.org/bot%s/getMe\n" token in
    make_get_basic getMe_string

  let updates_raw_body_to_yojson body =
    Yojson.Basic.from_string body

  let peekUpdates =
    let peekUpdates_string = api_method "getUpdates" in
    let open BatPervasives in
    let open Lwt in
    make_get_basic peekUpdates_string >>= (updates_raw_body_to_yojson %> Lwt.return)
  
  let sendMessageToUser chat_id msg =
    let sendMessageToUser_string = api_method "sendMessage" in
    let open BatPervasives in
    let open Lwt in
    let post_body =
      Cohttp_lwt.Body.of_string (
        let open Yojson.Basic in
        let v = to_string (
          `Assoc [
            ("text", `String msg);
            ("chat_id", `Int chat_id);
          ]
        ) in
        Printf.printf "DEBUG: sending message: %s\n" v;
        v
      )
    in
    Client.post (Uri.of_string sendMessageToUser_string) 
      ~headers:json_header
      ~body:post_body >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    (match !debug_mode with
    | Debug -> begin
        Printf.printf "Response code: %d\n" code;
        Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
        Printf.printf "Body: %s\n" body
      end
    | NoDebug -> ());
    body
  
end