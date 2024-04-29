#!/bin/sh

if git merge-base --is-ancestor 58b14d60 HEAD \
        || git merge-base --is-ancestor c42fce63 HEAD; then
    echo "This commit reintroduce old history, please rebase on new history."
    exit 1
fi

exit 0
