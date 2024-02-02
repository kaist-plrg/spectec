RED='\033[0;31m'
GREEN='\033[0;32m'
ORANGE='\033[0;33m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

function run {
  printf " - $1: ${ORANGE}Running..${NC}\r"
  printf "${RED}"
  $2 $3
  res=$?
  printf "${NC}"
  if [[ $res = 0 ]]; then
    printf " - $1: ${GREEN}Success${NC}  \n"
  fi
}

make && (
  if [ ! -d out ]; then
    mkdir out
  fi
  for (( i=0; ; i++ ))
  do  
    # Gen test
    printf "\n"
    printf "${ORANGE}Generating $i.wast..${NC}\r"
    # ./watsup spec/wasm-2.0/*.watsup --test --test-seed $i
    printf "${CYAN}[$i.wast]           ${NC}\n"

    # Run
    run "Reference Interpreter" "../interpreter/wasm" "out/${i}.wast"
  done

)

