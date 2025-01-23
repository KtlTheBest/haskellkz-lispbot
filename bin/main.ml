open List_bot_dune

let () = 
  let token = Utils.get_token_or_ask_user_for_one () in
  let open Telegram in
  let module BotToken = Token(struct 
      let x = token 
    end) 
  in
  let module Bot = LwtHttpBot(BotToken) in
  Lwt_main.run Bot.getMe
