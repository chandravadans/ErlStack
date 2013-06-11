#!/bin/bash
#
# Refer to 'man yaws' to see various options that can be given to yaws on command line

#yaws --daemon --runmod start_applications --sname yaws --heart --conf ./yaws.conf --id application
#yaws --daemon --sname cloud_server --runmod management_process --heart  --id cloud_server
yaws  --name 'manager1@cloudmanager.virtual-labs.ac.in' --runmod management_process --heart --conf ./yaws.conf --id cloud_manager  --setcookie iiit123

exit 0

