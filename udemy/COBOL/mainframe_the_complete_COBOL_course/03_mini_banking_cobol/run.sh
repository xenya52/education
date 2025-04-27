cobc -x \
  -o build/main \
  src/main.cbl

# Überprüfen, ob die Kompilierung erfolgreich war
if [ $? -eq 0 ]; then
  echo "Compilation successful. Running the program..."
  ./build/main
else
  echo "Compilation failed"
  exit 1
fi
