#!/bin/bash
WORK_DIR=`pwd`

if [[ ! -z "$DEXY_ROOT" ]]
then
  cd $DEXY_ROOT
else
  # if we're not in dexy, work in target/docs, which can be regenerated anyway
  WORK_DIR=target/docs
fi

sbt -Dsbt.log.noformat=true doc

for VERSION_DIR in target/scala-*/api
do
  SCALA_DIR=`echo $VERSION_DIR | sed -e "s/^.*\(scala-[^/]*\).*$/\1/"`

  mkdir -p $WORK_DIR/$SCALA_DIR/api
  cp -r $VERSION_DIR/* $WORK_DIR/$SCALA_DIR/api
done
