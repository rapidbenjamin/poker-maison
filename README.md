# Haskell Poker

Multiplayer poker site built with Haskell and React.


![screenshot](https://i.imgur.com/lO9B6dB.png)


To run the app locally carry out the following steps.

System dependencies you need are nodejs, ghc, stack, docker and libpq (c bindings to postgres)

Start Postgres DB

`docker-compose up`

Alternatively you can skip docker and just connect to a postgres server you have running.

Start redis server

`redis-server`

Start server

`dbConnStr='host=0.0.0.0 port=5432 user=postgres dbname=postgres password=postgres' secret="secret" stack run`


Run local server to serve static assets

`cd client/static && static-server`

Start front end

`cd client && yarn install && yarn start`


If you are interested in contributing have a look at the issues.


## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
