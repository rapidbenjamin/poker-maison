# Haskell Poker

Start dev DB
`docker-compose up`

Start server

`dbConnStr='host=0.0.0.0 port=5432 user=postgres dbname=poker2 password=postgres' secret="secret" stack run`

run local server to serve static assets (this should be incorporated into servant server)
``cd client/static && static-server`

Run front end

`cd client && yarn start`

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
