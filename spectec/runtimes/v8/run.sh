#!/bin/bash

o=false
w=false
v=false

D8_PATH="/root/v8/v8/out/x64.release/d8"

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
if [ $# -ne 1 ] ; then
    echo "Error: 1 argument expected, got $#"
    exit 0
fi

JS_PATH=./temp/test
OUTPUT_PATH=./temp/output
LOG_PATH=./temp/log
mkdir -p $JS_PATH $OUTPUT_PATH $LOG_PATH

I=0
J=0
K=0
# interpreters='wasm wasm-exception-handling wasm-tail-call wasm-memory64 wasm-extended-const wasm-multi-memory wasm-function-references wasm-gc'
interpreters='wasm wasm-tail-call wasm-extended-const wasm-function-references wasm-gc wasm-multi-memory wasm-relaxed-simd'

run_test() {
   let I=I+1
   filename=${FILE/%.wast/}
   echo "==== $I.$FILE ====="
   compiled=false
   for interpreter in ${interpreters}; do
      ./interpreter/${interpreter} -d $TEST_PATH/$FILE -o $JS_PATH/$filename.js 2> /dev/null
      if [[ $? == 0 ]]; then
            echo "compilation success with ${interpreter}."
            compiled=true
            break
      fi
   done
   if [ $compiled = false ]; then
   # if [ true ]; then
      echo "compilation failed."
      :> $LOG_PATH/$filename
      for interpreter in ${interpreters}; do
         echo "${interpreter}> " >> $LOG_PATH/$filename
         ./interpreter/${interpreter} -d $TEST_PATH/$FILE -o $JS_PATH/$filename.js 2>> $LOG_PATH/$filename
      done
      let K=K+1
   else
      # print in file
      /root/v8/v8/out/x64.release/d8 $JS_PATH/$filename.js >$OUTPUT_PATH/$filename
      # ./print-result.py $OUTPUT_PATH/$filename.txt $v
      if [[ $? == 1 ]]; then 
            echo "test failed."
            let J=J+1
      fi
   fi
}

if [[ "$TEST_PATH" =~ ".wast" ]]; then
   FILE=${TEST_PATH##*/}
   TEST_PATH=${TEST_PATH%/*.wast}
   run_test
   echo "$(($I-$K))/$I tests compiled."
   echo "$(($I-$K-$J))/$(($I-$K)) tests passed."
else
   if [[ $w == false ]]; then
      for FILE in `ls $TEST_PATH`
      do
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
   echo "Compiled $(($I-$K))/$I tests."
   echo "Passed $(($I-$K-$J))/$(($I-$K)) compiled tests."
fi


if [[ "$o" = false ]]; then
   rm -r ./temp
fi
