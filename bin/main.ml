open List_bot_dune

let () = 
  let token = Utils.get_token_or_ask_user_for_one () in
  let open Lwt in
  let open Telegram in
  let module BotToken = Token(struct 
      let x = token 
    end) 
  in
  let module Bot = LwtHttpBot(BotToken) in
  Bot.switch_debug_on ();
  let tasks = 
    Bot.getMe >>= fun _ ->
    Bot.peekUpdates >>= fun s ->
    let upd = Bot.try_construct_message_update s in
    let f (x : Bot.update) =
      (match x with
      | Message(msg_upd) ->
        let chat_id = msg_upd.chat_id in
        let contents = msg_upd.contents in
        (match contents with
        | Text(s) ->
          Bot.sendMessageToUser chat_id s >>= fun res ->
          Printf.printf "%s\n" res;
          return ()
        | UnknownContents -> return ())
      | Unknown(_) ->
        Printf.printf "%s\n" (Bot.show_update x); return ())
    in
    Lwt.join (List.map f upd)
  in
  Lwt_main.run tasks |> ignore;
  ()
