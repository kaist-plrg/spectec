make && (
  for (( i=0; ; i++ ))
  do  
    echo gen $i th module
    ./watsup spec/wasm-2.0/*.watsup --test --test-seed $i
    ../interpreter/wasm out/"$i".wast
    if [[ $? = 0 ]]; then
      echo [Reference interpreter] Success
    fi
  done

)

