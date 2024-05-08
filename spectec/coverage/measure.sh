# Input: directory name containing 0.wast ~ n.wast + time.txt) (n)
# TODO: Make this into Makefile?

trap cleanup INT
cleanup() {
  rm -f *.profraw *.profdata
  exit 130
}

if [ -z "$1" ]; then
  echo "Usage: $0 <dir> <n> <stride> <graph-out>"
  exit 1
fi

tests=$1
n=${2:-100}
stride=${3:-100}
groups=$(($n/stride))
exe="./wasmtime"
graph=${4:-"graph.txt"}

rm -f $graph
touch $graph

for i in `seq 0 $(($groups-1))`; do
  for j in `seq 0 $(($stride-1))`; do
    k=$(($stride*$i+$j))
    printf "\r%d" $k
    if [ $k -ge $n ]; then
      break
    fi
    LLVM_PROFILE_FILE="cov$j.profraw" $exe wast -C cache=n $tests/$k.wast
  done

  llvm-profdata merge --sparse cov*.profraw -o cov-stride.profdata
  rm cov*.profraw
  
  if [ $i -eq 0 ]; then
    mv cov-stride.profdata cov.profdata
  else
    llvm-profdata merge --sparse cov-stride.profdata cov.profdata -o cov.profdata
    rm cov-stride.profdata
  fi
  
  llvm-cov report $exe --instr-profile=cov.profdata | tail -n 1 | sed 's/^..... *//' >> $graph
done
printf "\r"

header="
  Time
  Regions
  MissedRegions
  Cover
  Functions
  MissedFunctions
  Executed
  Lines
  MissedLines
  Cover
  Branches
  MissedBranches
  Cover
"
echo $header > time-$graph
awk "NR % $stride == 0" $tests/time.txt | paste -d ' ' - $graph >> time-$graph
