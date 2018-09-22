#!/bin/bash
## First arg to the bashfile must be the path to the key file to SSH into the EC2 host(s)
##
## Build and push new docker image to AWS ECR then pull and run new image in EC2 instance.
## Ensure AWS credentials are set in environment or the ansible playbook will fail to login to AWS

if [ $# -eq 0 ]
  then
    echo "You forgot to speciffy the path to the key file to authenticate the SSH connection"
fi

sudo -H pip install pip==18.0.0 \
    && sudo -H pip uninstall --yes setuptools \
    && sudo -H pip install 'setuptools<20.2' --ignore-installed \
    && sudo -H pip install 'requests[security]' --ignore-installed \
    && sudo -H pip install boto awscli ansible docker-py --ignore-installed \
    && ANSIBLE_CONFIG=ansible/ansible.cfg ansible-playbook ansible/push-new-image.yml ansible/deploy-image.yml --key-file=$1 -vvvv