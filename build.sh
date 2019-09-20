#!/bin/bash

cd "$( dirname "${BASH_SOURCE[0]}" )"

mvn -Dmaven.test.skip=true -pl '!clojure' install
