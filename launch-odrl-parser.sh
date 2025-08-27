#!/bin/bash

mkdir -p /app/config
cp /config/*.ttl /app/config/

# TODO add setup.lisp loading?
exec /usr/src/startup.sh
