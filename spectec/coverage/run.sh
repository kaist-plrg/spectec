set -e

(
  ./measure.sh west-out 100000 100 graph.txt
  ./measure.sh smith-out 100000 100 graph-smith.txt
  ./measure.sh ../out-swarm 100000 100 graph-swarm.txt
) 2> err
