#!/bin/bash

docker_tag=plt/comma
docker_work_dir=/home/comma

docker_cmd_override=$*

docker build -t $docker_tag .

if [ $? -ne 0 ]; then
    echo "docker build failed"
else
    set -e
fi

docker run -it -v "$(pwd)":"$docker_work_dir" -w="$docker_work_dir" $docker_tag $docker_cmd_override
