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

cp target/api $WORK_DIR/api
