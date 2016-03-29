elm-gogui
==========

A webapp for playing Go, in [Elm](http://elm-lang.org).
The backend part is done by [gnugo-as-a-service](https://github.com/didmar/gnugo-as-a-service).

This project is designed as a fun experimentation with Elm.
I used a mix [Elm's core](https://github.com/elm-lang/core) (for the board) with [elm-html](https://github.com/evancz/elm-html) (for the rest of the UI).

How to build and run
--------------------

1. Install and run [gnugo-as-a-service](https://github.com/didmar/gnugo-as-a-service)

2. Run this:

    ```
    npm install -g elm
    elm-package install
    elm-reactor
    x-www-browser http://0.0.0.0:8000/go.elm
    ```

3. Alternatively, use Docker (build gnugo-as-a-service Docker first):

    ```
    docker build -t elm-gogui .
    docker-compose up
    ```
