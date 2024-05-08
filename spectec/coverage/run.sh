set -e

(
  ./measure.sh west-out 10000 100 graph.txt
  ./measure.sh smith-out 10000 100 graph-smith.txt
) 2> err
