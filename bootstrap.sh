#!/bin/sh

ant bootstrap
unzip lib/jacoco.zip lib/jacocoant.jar lib/jacocoagent.jar
cp lib/jacocoant.jar ${HOME}/.ant/lib
