#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

function transform {
  # TODO: check bug
  if [ $1 = "wasmer" ]; then
    cp $2 "$2-tmp"
    # sed -i '' 's/extern/func/g' $2
  fi
}

function backup {
  if [ $1 = "wasmer" ]; then
    mv "$2-tmp" $2
  fi
}

function run {
  printf " - $1: ${ORANGE}Running..${NC}\r"
  printf "${RED}"

  # transform wast
  transform $1 $3

  # run wast
  if grep -q "assert_exhaustion" $3; then
    timeout 10 $2 $3 &> /dev/null
    res=$?
    # either success or infinite loop
    res=$((res*(res-124)))
  else
    $2 $3 &> /dev/null
    res=$?
  fi

  # print result
  printf "${NC}"
  if [[ $res = 0 ]]; then
    printf " - $1: ${GREEN}Success${NC}  \n"
  else
    printf " - $1: ${RED}Fail${NC}     \n"
  fi

  backup $1 $3

}

# build reference interpreter
(cd ../interpreter && make) &&
# build west
make && (
  if [ ! -d out ]; then
    mkdir out
  fi
  for (( i=0; ; i++ ))
  do  
    filename="out/$i.wast"
    if [ ! -f $filename ]; then
      # Gen test
      printf "${ORANGE}Generating $i.wast..${NC}\r"
      ./watsup spec/wasm-2.0/*.watsup --test --test-seed $i
    fi
    printf "${CYAN}[$i.wast]           ${NC}\n"

    # Run
    run "reference interpreter" "../interpreter/wasm" $filename
    run "wasmer" "wasmer wast" $filename
    run "wasmtime" "wasmtime wast" $filename

    printf "\n"
  done

)

