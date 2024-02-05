#!/bin/bash

o=false
w=false
v=false

while getopts "ow:v" option; do
   case $option in
      o) # preserve output
         o=true;;
      w) # word
         w=true
         WORD=$OPTARG;;
      v) # verbose
         v=true
   esac
done
shift $[ $OPTIND - 1 ]
TEST_PATH=$1

JSON_PATH=./temp/test
OUTPUT_PATH=./temp/output
mkdir -p $JSON_PATH
mkdir -p $OUTPUT_PATH

I=0
J=0

run_test() {
   let I=I+1
   echo "==== $I.$FILE ====="
   filename=${FILE/%.wast/}
   ./wast2json $TEST_PATH/$FILE -o $JSON_PATH/$filename.json

   # print in file
   ./run-spec-test.py $JSON_PATH/$filename.json >$OUTPUT_PATH/$filename.txt
   ./print-result.py $OUTPUT_PATH/$filename.txt $v
   if [[ $? == 1 ]]; then 
         echo "test failed."
         let J=J+1
   fi
}

if [[ "$TEST_PATH" =~ ".wast" ]]; then
   FILE=$TEST_PATH
   run_test
else
   if [[ $w == false ]]; then
      for FILE in `ls $TEST_PATH`
      do
         echo "$FILE"
         if [[ "$FILE" =~ ".wast" ]]; then
            run_test
         fi
      done
   else
      for FILE in `ls $TEST_PATH`
      do
         if [[ "$FILE" =~ ".wast" && "$FILE" =~ "$WORD" ]]; then
            run_test
         fi
      done
   fi
   echo "$J/$I tests failed."
fi


if [[ "$o" = false ]]; then
   rm -r ./temp
fi
