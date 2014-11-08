#!/bin/sh
# Archive a private file that no longer needs to be private by moving it
# to the public repository.

if [ -h $1 ]
then
  TARGET=`readlink $1`
  rm $1
  cp -r $TARGET .
  git add --all $1
  cd `dirname $TARGET`
  git rm -r `basename $TARGET`
  cd -
else
  echo 'failed: not a symlink'
fi
