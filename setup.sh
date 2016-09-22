#!/bin/bash

# Colors
PURPLE='\033[1;35m'
BLUE='\033[1;34m'
GREEN='\033[1;32m'
RED='\033[1;31m'
RESET='\033[0m'
BOLD='\033[1;37m'

# Paths
SRC="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
DEST=${HOME}

printf "\n${BLUE}| Script will create now symlinks for the configuration. ${RESET}\n"
sleep 3

printf "\n${PURPLE}>_ Creating symlink for .emacs${RESET}\n"
ln -s $SRC/configs/.emacs $DEST/.emacs

printf "\n${PURPLE}>_ Creating symlink for .emacs.d${RESET}\n"
ln -s $SRC/configs/.emacs.d $DEST/.emacs.d

printf "\n${GREEN}| Done.${RESET}\n\n"
