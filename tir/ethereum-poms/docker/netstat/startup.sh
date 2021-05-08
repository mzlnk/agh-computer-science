#!/bin/bash
sh /home/ethnetintel/startscript.sh
cd /opt/netstat-ui/eth-netstats
grunt
WS_SECRET=secretsecret npm start
