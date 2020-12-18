#! /bin/bash

cd $(dirname "$0")

echo "generating basic test output..."
turnt basic/*.txt --save > /dev/null
echo "generating optimized test output..."
turnt optimize/*.txt --save > /dev/null

for f in basic/*.out; do
    echo "diff for $(basename $f)"
    diff $f optimize/$(basename $f)
    echo ""
done
