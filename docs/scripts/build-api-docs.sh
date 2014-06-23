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

DOCS_DIR=$WORK_DIR
until [ "$(basename $DOCS_DIR)" == "docs" ]
do
  DOCS_DIR=$(dirname $DOCS_DIR)
done

for VERSION_DIR in target/scala-*/api
do
  SCALA_DIR=`echo $VERSION_DIR | sed -e "s/^.*\(scala-[^/]*\).*$/\1/"`

  mkdir -p $DOCS_DIR/api/$SCALA_DIR
  cp -r $VERSION_DIR/* $DOCS_DIR/api/$SCALA_DIR

  sbt -Dsbt.log.noformat=true "project lift-documentation-helpers" \
    "run-main net.liftweb.documentation.AddSearchToApiDocs \"$DOCS_DIR/api/$SCALA_DIR\""
done
