# TeleLisp Bot

TeleLisp is an imaginary (and arguably not well-defined) lisp dialect that is currently WIP.
The goal is to combine lisp interpreter with Telegram Bot API to make some dialect of lisp
accessible from within Telegram.

## Why are you doing this?

For fun.
Wouldn't it be also cool to have a lisp inside your telegram client?
You could functional programming while you're talking about functional programming :D

## How it works?
It doesn't work yet.
But the way I see it, is that for each group the bot exists in,
and for every user, the bot keeps track of some state and definitions.

* When user sends a lisp code, the bot will parse it and evaluate.
* If user sends several messages with lisp codes, then the bot
will evaluate the messages sequentially, as if it was one big file.
* If the user edits a lisp code, then bot should reevaluate the
code after that, for consistency.
* Users cannot contaminate the state of the other users.
* Users, however, can export their function definitions for others to reuse.
* If the exported definition is modified later, then it will affect the code
that depends on that function.
Which is a mess.
* Bot should provide some capabilities to combine the code into libraries, for
the benefit of everyone.
* Bot should store the code written by users for persistence (especially while the bot
is being developed and will crash often).
* I am not sure what kind of capabilities the Lisp should have, and whether there is a need
for a full-blown IO, but we'll see.

Those are just some ideas and my understanding of the bot.
If you have any suggestions or comments, please let me know.

## How it is implemented?
It is implemented in OCaml.
There will be two main components:
* Lisp interpreter
* Telegram Bot API library
For communication with Telegram servers, I'll use cohttp-lwt, unless better alternatives are found.

And I'm using batteries, because I don't have much free time to redefine everything from scratch.
