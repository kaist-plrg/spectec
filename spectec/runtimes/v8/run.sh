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

RESULT_PATH=./result

if [[ "$o" = false ]]; then
   TEMP_PATH=$(mktemp -d)
else 
   TEMP_PATH=$RESULT_PATH
fi

JS_PATH=$TEMP_PATH/test
OUTPUT_PATH=$TEMP_PATH/output
FAIL_PATH=$RESULT_PATH/failed
COMPILE_FAIL_PATH=$FAIL_PATH/compile
TEST_FAIL_PATH=$FAIL_PATH/test
mkdir -p $JS_PATH $OUTPUT_PATH $FAIL_PATH $COMPILE_FAIL_PATH $TEST_FAIL_PATH

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
      :> $COMPILE_FAIL_PATH/$filename
      for interpreter in ${interpreters}; do
         echo "${interpreter}> " >> $COMPILE_FAIL_PATH/$filename
         ./interpreter/${interpreter} -d $TEST_PATH/$FILE -o $JS_PATH/$filename.js 2>> $COMPILE_FAIL_PATH/$filename
      done
      cp $TEST_PATH/$FILE $COMPILE_FAIL_PATH
      let K=K+1
   else
      # print in file
      /root/v8/v8/out/x64.release/d8 $JS_PATH/$filename.js >$OUTPUT_PATH/$filename
      # ./print-result.py $OUTPUT_PATH/$filename.txt $v
      if [[ $? == 1 ]]; then 
            cp $OUTPUT_PATH/$filename $TEST_FAIL_PATH
            cp $JS_PATH/$filename.js $TEST_FAIL_PATH
            cp $TEST_PATH/$filename.wast $TEST_FAIL_PATH
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
   rm -r $TEMP_PATH
fi
