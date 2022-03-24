#!/bin/sh

D=$(dirname $0)

if [ ! -f Makefile ]
then
    cp $D/Makefile.template Makefile
fi

if [ ! -f .gitignore ]
then
    cp $D/gitignore.template .gitignore
fi
