#!/bin/sh

if [ ! -f Makefile ]
then
    cp $0/../Makefile.template Makefile
fi

if [ ! -f .gitignore ]
then
    cp $0/../gitignore.template .gitignore
fi
