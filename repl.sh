#!/bin/bash

cd "$( dirname "${BASH_SOURCE[0]}" )"

clj -Adev -m nrepl.cmdline $@
