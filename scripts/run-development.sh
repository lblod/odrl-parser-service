#!/usr/bin/env bash

# Adapted from the `run-development.sh` script in the mu-javascript-template:
# <https://github.com/mu-semtech/mu-javascript-template/blob/master/run-development.sh>

# Make sure file exists to avoid useless error message in logs
touch /tmp/service-status.lock

SERVICE_STATUS=$(cat /tmp/service-status.lock)
if [ "$SERVICE_STATUS" == "exit_after_startup" ]
then
    echo "More file changes detected during startup. Continue starting up and then schedule another start up."
    exit 0
fi

function handle_sigusr()
{
    SERVICE_STATUS=$(cat /tmp/service-status.lock)
    if [ $SERVICE_STATUS == "starting" ];
    then
        echo "exit_after_startup" > /tmp/service-status.lock
    fi
}

# watchexec may send SIGUSR1 during starting when more files have changed.  This
# requires to finish starting up so no files are lingering about, but stop the
# script before starting node.  watchexec will then start the script again.
trap handle_sigusr SIGUSR1

SERVICE_STATUS=$(cat /tmp/service-status.lock)
if [ "$SERVICE_STATUS" == "exit_after_startup" ]
then
    echo "" > /tmp/service-status.lock
    echo " >> Exiting without launching REPL due to EXIT_AFTER_STARTUP status"
    exit 0
else
    # (Re)Launch REPL
    echo " >> Start launching REPL"
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
        echo "qlot exec sbcl $CMD_OPTS --load /usr/src/startup.lisp"
    fi

    # exec will replace this script's process with the qlot/sbcl process,
    # allowing the latter to receive the SIGUSR1 signal to exit
    exec qlot exec sbcl $CMD_OPTS --load /usr/src/startup.lisp
fi
