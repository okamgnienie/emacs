#!/bin/bash

# Colors
PURPLE='\033[1;35m'
BLUE='\033[1;34m'
GREEN='\033[1;32m'
RESET='\033[0m'

# Paths
DIR="./configs/.emacs.d/elpa/yasnippet-*/snippets"

printf "\n${BLUE}| Script will remove now unnecessary snippets. ${RESET}\n"
sleep 3

printf "\n${PURPLE}>_ Entering directory... ${RESET}\n"
cd $DIR
echo "cd `pwd`"

printf "\n${PURPLE}>_ Cleaning... ${RESET}\n"
rm -rf -v js*
rm -rf -v web-mode

printf "\n${GREEN}| Done. ${RESET}\n\n"
