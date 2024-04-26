#!/bin/sh

# Check indentation

echo "Check indent"
for f in src/*.ml src/*.mli utils/*.ml utils/*.mli ppx/*.ml ppx/*.mli
do
    ocp-indent $f -o $f.lint
    if diff -q $f $f.lint ;
    then
        rm -f $f.lint
    else
        echo "File $f not correctly indented"
        exit 1
    fi
done

# Check headers

echo "Check headers"
for f in src/*.ml src/*.mli src/*.mll utils/*.ml utils/*.mli ppx/*.ml ppx/*.mli
do
    cp $f $f.head
    headache -h HEADER $f
    if diff -q $f $f.head ;
    then
        rm -f $f.head
    else
        echo "File $f has incorrect header"
        exit 1
    fi
done

# Done
echo "Done."
