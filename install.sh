#!/bin/bash
# Create symbolic links from many directories into the current directory,
# which should have the same internal directory structure. This was created
# to allow my public schoolwork repository to contain notes etc, while still
# referring to sensitive material such as exam solutions in a private 
# repo which lives in a submodule.
DESTINATION=`realpath $1 --relative-to=.`
KEEP=( 
       'hw'
       'exams'
       'project'
     )
# have to deMorgan this to avoid intercalating -o
for x in `find . \! \( ${KEEP[@]/#/-not -name } \) | cut -c3-`
do
  TO_ADD=$DESTINATION/$x
  if [ -e $TO_ADD ]
  then
    echo 'Skipped existing file' $TO_ADD
    continue
  fi
  if [ ! -h $TO_ADD ] 
  then
    LINKPOINT=`dirname $TO_ADD`
    echo 'linkpoint' $LINKPOINT
    ln -s `realpath $x --relative-to=$LINKPOINT` $TO_ADD
    echo $TO_ADD
  fi
done
