![alt text](https://i.imgur.com/big5Pqa.png "Ten Poker")

## A multiplayer app crafted with Haskell, React and ❤️

> “The guy who invented poker was bright, but the guy who invented the chip was a genius.” ~ Julius Weintraub


[![Netlify Status](https://api.netlify.com/api/v1/badges/c21ef5e4-5227-4a24-87a8-b71961650fd5/deploy-status)](https://app.netlify.com/sites/tenpoker/deploys)

<br />

## How to get everything working on your local machine.

## Back End

Firstly make sure you have ghc and stack installed in order to compile the back end written in Haskell.
If you need to install the Haskell platform then run
```bash
curl -sSL https://get.haskellstack.org/ | sh
```

Secondly install libpq (c bindings to postgres)
```bash
sudo apt-get install libpq-dev
```

Next install redis.
```bash
sudo apt-get install redis
```

Navigate to the server/ directory.
```bash
cd server
```

Compile the back end poker server.
```bash
stack build
```

## Now we need to set some config.

Ensure postgresql 10 is installed and running.

Set the env var so that the server has the postgresql connection string.
Of course you will need to change the db connection parameters below to match your local database.
```bash
export dbConnStr='host=0.0.0.0 port=5432 user=tom dbname=defaultdb password=pass'
```

Set env variable with the secret key for generating auth tokens.
```bash
export secret="your-super-secret"
```

Lastly ensure redis-server is running in the background on default port 
```bash
redis-server
```

Now run the server locally. The default user API port is 8000 and websocket port is 5000. 
```bash
stack run
```


## Front End

Install a required system dependency for node-sass .
```bash
sudo apt-get install libpng-dev
```

Navigate to the client/ directory with
```bash
cd client
```

Then just run.
```bash
yarn start
```

Now you are ready to play poker! 

### Simulating a multiplayer game locally

You may want to play against yourself when you are developing locally so just 
run the clients on two separate ports.

In your first terminal run
```
PORT=8001 yarn start
```

Then open another terminal and run
```
PORT=8002 yarn start
```

Now just open two tabs in your browser navigating to 
```
localhost:8001
```
and 
```
localhost:8002
```


## Contributions Welcome

Have a look at the issues if you want some starting ideas on how to get involved.

Feel free to open any issues with potential enhancements or bugs you have found.

## License

This is free and unencumbered software released into the public domain.  
For more information, please refer to the `UNLICENSE` file or [unlicense.org](http://unlicense.org).
