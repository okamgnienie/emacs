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

printf "\n${BLUE}| Welcome to setup script. ${RESET}\n"
sleep 3

printf "\n${PURPLE}>_ Installing npm packages required for ESLint${RESET}\n"
npm install -g eslint babel-eslint eslint-plugin-react

printf "\n${PURPLE}>_ Creating symlink for .emacs${RESET}\n"
ln -s $SRC/configs/.emacs $DEST/.emacs

printf "\n${PURPLE}>_ Creating symlink for .emacs.d${RESET}\n"
ln -s $SRC/configs/.emacs.d $DEST/.emacs.d

printf "\n${PURPLE}>_ Creating symlink for .eslintrc.js${RESET}\n"
ln -s $SRC/configs/.eslintrc.json $DEST/.eslintrc.json

printf "\n${GREEN}| Done.${RESET}\n\n"
