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
  let echo_f upd =
    let open Bot in
    match upd with
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
      Printf.printf "%s\n" (Bot.show_update upd); return ()
  in
  Bot.poll echo_f
