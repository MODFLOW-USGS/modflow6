#!/bin/sh

echo "Current working directory: $PWD"
cd ./pymake

# build with pymake
echo "Running command: python $1 $2 $3 $4 $5"
python $1 $2 $3 $4 $5

# ... and back again (BB)
cd ..
