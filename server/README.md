# poker-server

For development install docker and docker-machine.

Then use docker-compose up to run the redis and postgres containers.

When connecting to the db remember to use the docker-machine host ip address and not local host to connect to the db container (usually 192.168.99.100)


##Deployment

### Dynamic EC2 Inventory Management

EC2 hostnames vary over time we don't want to manually keep track of say which host is our production environment.

So instead of hardcoding them for deployment we use a script for dynamic AWS EC2 inventory management which retrieves the hosts of
EC2 instances automatically.

Since we have outsourced the management of host names to this script
ansible can just deploy our code to the AWS instance with the tag name - Production.

### Continous Integration

Travis is used for Continous integration to run tests and linting on any push to Github.

### Continous Delivery using CI Webhooks

On a successful build on the Travis CI server we automatically
deploy the front end to an S3 bucket and the backend to the EC2 instance which is tagged as our Production environment. Provisioning
of the EC2 instance is automated using ansible.

The steps for provisioning the server are for the backend:

1. On successful build of master on Travis CI we build the Docker Image.

2. Run the Ansible Playbook which does the following

1 - Push the Docker Image to a Private Repo on Docker Hub

2 - SSHes into our production EC2 instance

3 - Installs the necessary dependencies

4- Spins up our Docker container.

## Generate Reason Types, Decoders and API Queries with

The reason-servant package allows us to generate front end ReasonML code from our Servant API specification and our haskell types.

To do so run:

`stack runHaskell GenerateReason`

Then the generated Reason code will be in client/app/Api.re
