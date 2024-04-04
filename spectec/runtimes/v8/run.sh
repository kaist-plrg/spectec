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
mkdir -p $JS_PATH
mkdir -p $OUTPUT_PATH

I=0
J=0

run_test() {
   let I=I+1
   filename=${FILE/%.wast/}
   echo "==== $I.$FILE ====="
   ./wasm -d $TEST_PATH/$FILE -o $JS_PATH/$filename.js
   # ./exception-handling/interpreter/wasm -d $TEST_PATH/$FILE -o $JS_PATH/$filename.js

   # print in file
   /root/v8/v8/out/x64.release/d8 $JS_PATH/$filename.js >$OUTPUT_PATH/$filename.txt
   # ./print-result.py $OUTPUT_PATH/$filename.txt $v
   if [[ $? == 1 ]]; then 
         echo "test failed."
         let J=J+1
   fi
}

if [[ "$TEST_PATH" =~ ".wast" ]]; then
   FILE=${TEST_PATH##*/}
   TEST_PATH=${TEST_PATH%/*.wast}
   run_test
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
   echo "$(($I-$J))/$I tests passed."
fi


if [[ "$o" = false ]]; then
   rm -r ./temp
fi
