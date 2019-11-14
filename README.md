# Haskell Poker

Multiplayer poker site built with Haskell and React.

[![Netlify Status](https://api.netlify.com/api/v1/badges/c21ef5e4-5227-4a24-87a8-b71961650fd5/deploy-status)](https://app.netlify.com/sites/tenpoker/deploys)


# Server

To run the server locally carry out the following steps.

System dependencies you need are nodejs, ghc, stack, docker and libpq (c bindings to postgres)

Start Postgres DB

`docker-compose up`

Alternatively you can skip docker and just connect to a postgres server you have running.

Start redis server

`redis-server`

Start server

`dbConnStr='host=0.0.0.0 port=5432 user=postgres dbname=postgres password=postgres' secret="secret" stack run`


# Client

Install system dependency needed for node-sass 
`sudo apt-get install libpng-dev`

Run a little local server to serve static assets
Enter this from project root.
`npm i -g static-server && cd client/static && static-server`

Start front end

  dbConnStr='host=0.0.0.0 port=5432 user=tom dbname=poker2 password=tom' secret="wwaaifidsa9109f0dasdakjdm,4jhkbnsdv768tkjhbnsfda-=2-13" stack run


`cd client && yarn install && yarn start`


If you are interested in contributing have a look at the issues.


## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
