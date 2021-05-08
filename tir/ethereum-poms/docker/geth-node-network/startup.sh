#!/bin/sh
echo "Initializing bootnode..."
screen -S BOOTNODE -d -m bootnode -nodekey /opt/geth/nodes/bootnode/boot.key -verbosity 7 -addr "127.0.0.1:5000"

echo "Initializing nodes..."
for i in `seq 0 $((5))`
do
  echo "Initializing node $i..."
  mkdir /opt/geth/nodes/node-$((i))
  cp /opt/geth/utils/genesis.json /opt/geth/nodes/node-$((i))
  geth --nousb init /opt/geth/nodes/node-$((i))/genesis.json --datadir /opt/geth/nodes/node-$((i))

  echo "abc123" > password.txt
  account=`geth account new --password password.txt --keystore . | grep -e "^Public" | awk '{print $6}'`

  screen -S NODE-$((i)) -d -m geth --networkid $NETWORK_ID --nousb --syncmode full --allow-insecure-unlock --rpc --rpcport $((RPC_PORT_FIRST + i)) --rpcaddr "0.0.0.0" --port $((HOST_PORT_FIRST + i)) --rpccorsdomain="*" --rpcapi web3,eth,debug,personal,net,miner --allow-insecure-unlock --datadir /opt/geth/nodes/node-$((i)) --bootnodes $ENODE --keystore . --unlock $account --password password.txt --mine
  echo "Node $i initialized"
  screen -ls
done

echo "Restoring bootnode console..."
screen -r BOOTNODE
