#!/bin/bash
DESTINATION=`realpath $1`
KEEP=( 
       'hw'
       'exams'
       'project'
     )
# have to deMorgan this to avoid intercalating -o
for x in `find . \! \( ${KEEP[@]/#/-not -name } \) | cut -c3-`
do
  TO_ADD=$DESTINATION/$x
  if [ -f $TO_ADD ]
  then
    echo 'Skipped existing file' $TO_ADD
  fi
  if [ ! -h $TO_ADD ] 
  then
    echo `readlink -f $x`
    ln -s `readlink -f $x` $TO_ADD
    echo $TO_ADD
  fi
done
