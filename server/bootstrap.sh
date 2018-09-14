# install virtual env and python

# activate python virtual env
virtualenv venv
~ source venv/bin/activate

# install ansible
sudo dnf install ansible

pip install docker-py boto

# install aws-cli

# Give executable permissions to the
# script which dynamically retrieves 
# AWS EC2 instance inventory 
chmod +x ansible/inventory/ec2.py

# Environment variables to the AWS EC2 inventory management script 
#
#
# Our EC2 dynamic inventory script has the file name ec2.py
#
# This variable tells Ansible to use the dynamic 
# EC2 script instead of a static /etc/ansible/hosts file.
export EC2_INI_PATH=./ansible/inventory/ec2.ini
# This variable tells ec2.py where the ec2.ini config file is located.
export ANSIBLE_INVENTORY=./ansible/inventory/ec2.py