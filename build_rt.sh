#!/bin/bash

cd "$( dirname "${BASH_SOURCE[0]}" )"


javac -cp ~/.m2/repository/org/clojure/clojure/1.11.0-master-SNAPSHOT/clojure-1.11.0-master-SNAPSHOT.jar src/main/java/subclojure/*.java
