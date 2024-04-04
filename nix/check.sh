#!/bin/sh

# Check indentation

set -eu

ret=0

echo "Check indent"
for f in src/*.ml src/*.mli utils/*.ml utils/*.mli
do
    ocp-indent $f -o $f.lint
    if diff -q $f $f.lint ;
    then
        rm -f $f.lint
    else
        echo "File $f not correctly indented"
        ret=1
    fi
done

# Check headers

echo "Check headers"
for f in src/*.ml src/*.mli src/*.mll utils/*.ml utils/*.mli
do
    cp $f $f.head
    headache -h HEADER $f
    if diff -q $f $f.head ;
    then
        rm -f $f.head
    else
        echo "File $f has incorrect header"
        ret=1
    fi
done

if [ $ret -eq 0 ]; then
    echo "Done."
else
    echo "Errors."
fi

exit $ret
