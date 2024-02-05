#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

function run {
  printf " - $1: ${ORANGE}Running..${NC}\r"
  printf "${RED}"

  # run wast
  if grep -q "assert_exhaustion" $3; then
    timeout 10 $2 $3
    res=$?
    # either success or infinite loop
    res=$((res*(res-124)))
  else
    $2 $3
    res=$?
  fi

  # print result
  printf "${NC}"
  if [[ $res = 0 ]]; then
    printf " - $1: ${GREEN}Success${NC}  \n\n"
  else
    printf " - $1: ${RED}Fail${NC}  \n\n"
  fi

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
    if [ ! -f "out/$i.wast" ]; then
      # Gen test
      printf "${ORANGE}Generating $i.wast..${NC}\r"
      ./watsup spec/wasm-2.0/*.watsup --test --test-seed $i
    fi
    printf "${CYAN}[$i.wast]           ${NC}\n"

    # Run
    run "Reference Interpreter" "../interpreter/wasm" "out/$i.wast"
  done

)

