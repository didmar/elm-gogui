elm-gogui
==========

A webapp for playing Go, in Elm.
The backend is done by [gnugo-as-a-service](https://github.com/didmar/gnugo-as-a-service)

How to build and run
--------------------

1. Install and run [gnugo-as-a-service](https://github.com/didmar/gnugo-as-a-service)

2. Run this:

    npm install -g elm
    elm-package install
    elm-reactor
    x-www-browser http://0.0.0.0:8000/go.elm
