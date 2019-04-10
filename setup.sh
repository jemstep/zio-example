#!/bin/bash

NAME=$1
DEVUSER=$USER

docker build -t "$NAME:latest" ./docker/.
docker run --rm -it \
    --name=$NAME \
    --volume=$(pwd):/code/$NAME \
    -v /home/${DEVUSER}/.ivy2:/root/.ivy2 \
    $NAME