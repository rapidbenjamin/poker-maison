#!/usr/bin/env bash
set -o errexit
set -o pipefail
set -o nounset

# Location of executable
BUILD_DIR="./build"
BINARY_PATH=$BUILD_DIR"/poker-server-exe"

# Where we deploy
HOST="34.244.29.59"
REMOTE="ubuntu@"$HOST

RUN="sudo /opt/server/server-exe"


serverHealthCheck(){
    local url="https://tenpoker.co.uk/lobby"
    local statusCode=`echo $(curl -s -o /dev/null -w "%{http_code}" $url)`

    if [[ "$statusCode" != 2* ]] && [[ "$statusCode" != 0* ]]; then
        echo "Error: Server responded with http status: $statusCode"  # if the content of statusCode isn't a "2xx" print the error.
    fi

    if [[ "$statusCode" = 0* ]]; then
        echo "Error: Server unreachable: $statusCode" # connection refused
    fi

     if [[ "$statusCode" = 2* ]]; then
        echo "Success: Server responded with http status: $statusCode"
    fi
}

# compile binrary
stack build --copy-bins --local-bin-path $BUILD_DIR


ssh -i ~/.ssh/id_rsa $REMOTE sudo "systemctl stop server.service" 


# copy server binary to remote
scp -i ~/.ssh/id_rsa $BINARY_PATH $REMOTE:/opt/server

# restart server using systemd
ssh -i ~/.ssh/id_rsa $REMOTE sudo "systemctl start server.service"

serverHealthCheck