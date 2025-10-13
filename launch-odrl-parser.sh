#!/bin/bash

# Make sure we are in the app folder to launch the REPL
cd /app

if [ "$MODE" == "development" ]
then
    exec watchexec \
         --project-origin="/" \
         --watch="/app" \
         --stop-timeout="60s" \
         --stop-signal="SIGUSR1" \
         --exts="lisp,asd,lock" \
         --shell=none \
         --wrap-process=none \
         --restart \
         /usr/src/run-development.sh
else
    # Adapted from base image's startup.sh
    CMD_OPTS=""

    if [[ $LISP_DYNAMIC_SPACE_SIZE ]]
    then
        CMD_OPTS="$CMD_OPTS --dynamic-space-size $LISP_DYNAMIC_SPACE_SIZE"
    fi

    if [[ $LISP_DYNAMIC_STACK_SIZE ]]
    then
        CMD_OPTS="$CMD_OPTS --control-stack-size $LISP_DYNAMIC_STACK_SIZE"
    fi

    if [[ $LOG_LISP_LAUNCH_COMMAND ]]
    then
        echo "sbcl $CMD_OPTS --load /usr/src/startup.lisp"
    fi

    exec qlot exec sbcl $CMD_OPTS --load /usr/src/startup.lisp
fi
