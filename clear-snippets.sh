#!/bin/bash

dir="./configs/.emacs.d/elpa/yasnippet-*/snippets"

echo -e "\nEntering directory...\n"
cd $dir

echo -e "\nCleaning...\n"
rm -rf js*
rm -rf web-mode

echo -e "\nDone.\n"
