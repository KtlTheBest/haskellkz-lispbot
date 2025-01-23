module type StringT = sig val x : string end
module type TokenT = sig val token : string end

module Token(Tok: StringT) : TokenT = struct
  let token = Tok.x
end

module LwtHttpBot(Token: TokenT) = struct
  open Lwt
  open Cohttp
  open Cohttp_lwt_unix

  let token = Token.token
  let getMe =
    let getMe_string = Printf.sprintf "https://api.telegram.org/bot%s/getMe\n" token in
    Printf.printf "DEBUG: URL method: %s" getMe_string;
    Client.get (Uri.of_string getMe_string) >>= fun (resp, body) ->
    let code = resp |> Response.status |> Code.code_of_status in
    Printf.printf "Response code: %d\n" code;
    Printf.printf "Headers: %s\n" (resp |> Response.headers |> Header.to_string);
    body |> Cohttp_lwt.Body.to_string >|= fun body ->
    Printf.printf "Body: %s\n" body
end