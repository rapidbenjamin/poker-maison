#!/usr/bin/env bash

set -o errexit
set -o pipefail
set -o nounset

REMOTE="ubuntu@34.244.29.59"
INSTALL_SYS_DEPENDENCIES="sudo apt-get update && sudo apt-get install -yy build-essential lzma-dev libpq-dev"


ssh -i ~/Downloads/tenprod.pem $REMOTE sudo $INSTALL_SYS_DEPENDENCIES

# create release dir and give ubuntu user permissions
ssh -i ~/.ssh/id_rsa $REMOTE sudo "sudo mkdir -pv /opt/server && sudo chown ubuntu /opt/server" -v

# give ubuntu user ownership of systemd service file dir
ssh -i ~/.ssh/id_rsa $REMOTE sudo "sudo chown ubuntu /etc/systemd/system" -v

# copy systemd service conf file to remote
scp -i ~/.ssh/id_rsa "server.service" $REMOTE:/etc/systemd/system

ssh -i ~/.ssh/id_rsa $REMOTE "sudo systemctl enable server.service"

scp -i ~/.ssh/id_rsa ".prod.env" $REMOTE:/etc/systemd/system/prod.env