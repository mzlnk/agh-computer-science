FROM ubuntu:xenial

RUN apt-get update && \
    apt-get install -y wget software-properties-common && \
    apt-get install -y libusb-1.0-0 && \
    add-apt-repository -y ppa:ethereum/ethereum && \
    apt-get update && \
    apt-get install -y ethereum

RUN apt-get install -y screen

RUN mkdir /opt/geth
RUN mkdir /opt/geth/nodes
RUN mkdir /opt/geth/utils
RUN mkdir /opt/geth/nodes/bootnode

COPY ./startup.sh /opt/geth/utils

CMD exec sh /opt/geth/utils/startup.sh
