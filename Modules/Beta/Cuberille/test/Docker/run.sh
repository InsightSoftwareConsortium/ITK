#!/bin/sh

script_dir="`cd $(dirname $0); pwd`"

docker run \
  --rm \
  -v $script_dir/../..:/usr/src/ITKCuberille \
    insighttoolkit/cuberille-test \
      /usr/src/ITKCuberille/test/Docker/test.sh
