#!/bin/bash

FILE=$1

echo "got here: $FILE";

echo "====== node ======"
node "$FILE";

echo "====== scala ====="
scala -cp target/scala-2.11/classes HW2 "$FILE";

