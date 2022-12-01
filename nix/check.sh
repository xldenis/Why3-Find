#!/bin/sh

# Check linting

for f in src/*.ml src/*.mli
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
