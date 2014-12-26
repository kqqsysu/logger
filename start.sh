#! /bin/bash

ROOT=`cd $(dirname $0); pwd`
CONFDIR=$ROOT/config
ERL=/usr/local/bin/erl

DATETIME=`date "+%Y%m%d-%H%M%S"`

export ERL_CRASH_DUMP=$ROOT/erl_crash_$DATETIME.dump
export HOME=$ROOT

$ERL -sname log_server@localhost \
      -pa ebin \
      -s logger_app
