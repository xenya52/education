cobc -x \
  -o build/main \
  src/main.cbl

if [ $? -eq 0 ]; then
  ./build/main
else
  echo "Compilation failed"
  exit 1
fi
