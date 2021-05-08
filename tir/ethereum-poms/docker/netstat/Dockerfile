FROM ubuntu:xenial

RUN apt-get update &&\
    apt-get install -y curl git-core &&\
    apt-get update

RUN curl -sL https://deb.nodesource.com/setup_14.x | bash - &&\
    apt-get install -y nodejs

RUN node -v
RUN npm -v


RUN apt-get update &&\
    apt-get install -y build-essential

RUN adduser ethnetintel

RUN cd /home/ethnetintel &&\
    git clone https://github.com/cubedro/eth-net-intelligence-api &&\
    cd eth-net-intelligence-api &&\
    npm install &&\
    npm install -g pm2

RUN echo '#!/bin/bash\nset -e\n\ncd /home/ethnetintel/eth-net-intelligence-api\n/usr/bin/pm2 start ./app.json\ntail -f \
    /home/ethnetintel/.pm2/logs/node-app-out-0.log' > /home/ethnetintel/startscript.sh

RUN chmod +x /home/ethnetintel/startscript.sh &&\
    chown -R ethnetintel. /home/ethnetintel

RUN node -v
RUN pm2 -v

RUN mkdir /opt/netstat-ui

RUN cd /opt/netstat-ui &&\
    git clone https://github.com/cubedro/eth-netstats &&\
    cd eth-netstats &&\
    npm install &&\
    npm install grunt --save-dev

RUN npm install -g grunt-cli
RUN npm install grunt

RUN node -v
RUN npm -v

COPY ./startup.sh /opt/netstat-ui
RUN ls -alh /opt/netstat-ui

RUN apt-get update && \
    apt-get install -y wget software-properties-common && \
    apt-get install -y libusb-1.0-0 && \
    add-apt-repository -y ppa:ethereum/ethereum && \
    apt-get update && \
    apt-get install -y ethereum

CMD exec sh /opt/netstat-ui/startup.sh
