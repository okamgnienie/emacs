#!/bin/bash

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Description: Installation script for phardyn emacs config.                  #
# Author:      Przemyslaw Hardyn                                              #
# GitHub:      github.com/phardyn                                             #
# Website:     przemyslawhardyn.com                                           #
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - #
#                            Created: Poland, 2015                            #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Save location:
location=$(pwd)

# Colors and writing functions ------------------------------------------------
defaultColor=$(printf "\e[m")
infoColor=$(printf "\e[44m")
successColor=$(printf "\e[42m")
warningColor=$(printf "\e[48;5;202m")
errorColor=$(printf "\e[41m")

# Show success text:
function writeSuccess () {
    echo ${successColor}$1${defaultColor}
}

# Show neutral text:
function writeInfo () {
    echo -e ${infoColor}$1${defaultColor}
}

# Show warning text:
function writeWarning () {
    echo -e ${warningColor}$1${defaultColor}
}

# Show error text:
function writeError () {
    echo -e ${errorColor}$1${defaultColor}
}

# Clear console:
function clearConsole () {
    printf "\033c"
}

# Components installation  ---------------------------------------

# Install Emacs config files:
function installEmacsConfig () {
    writeInfo "Installing Emacs configuration..."

    # Delete current config:
    rm -rf ~/.emacs.d
    rm -f ~/.emacs
    
    # Copy new config:
    cp -rf ./configs/.emacs.d ~/.emacs.d
    cp -f ./configs/.emacs ~/.emacs

    if [ -d ~/.emacs.d ] && [ -f ~/.emacs ]; then
        writeSuccess "Installation completed."
    else
	writeError "Error while installing Emacs config."
    fi
}

# Create backup for the current config:
function backupEmacs () {
    writeInfo "Creating backup for the current emacs setup..."

    if [ -d ~/.emacs.d ]; then
	cp -rf ~/.emacs.d ./BACKUP
	if [ -d ./BACKUP/.emacs.d ]; then
	    writeSuccess "Creating backup of ~/.emacs.d finished."
	else
            writeError "Error while creating backup of ~/.emacs.d."
	fi
    else
        writeInfo "No ~/.emacs.d. to backup."
    fi

    if [ -f ~/.emacs ]; then
	cp -rf ~/.emacs ./BACKUP
	if [ -f ./BACKUP/.emacs ]; then
	    writeSuccess "Creating backup of ~/.emacs finished."
	else
            writeError"Error while creating backup of ~/.emacs."
	fi
    else
        writeInfo "No ~/.emacs to backup."
    fi

    if [ -d ./BACKUP/.emacs.d ] && [ -f ./BACKUP/.emacs ]; then
        installConfigs
    else
        writeInfo "Error while creating backup, \
or there are no files. Do you want to continue? (y/n)."
        read decision
        if [ $decision == 'y' ]; then
            installConfigs
        fi
    fi
}

# Backup configs:
function createBackups () {
    backupEmacs
}

# Install configs:
function installConfigs () {
    installEmacsConfig
}

# -------------------------------- INSTALLATION -------------------------------
# Welcome info and script startup:
clearConsole
writeInfo "This script will backup your current setup \
           and install emacs config saved in: \n\
           $location/configs/ \n\n\
           Do you want to continue? (y/n)"

read start
if [ "$start" == "y" ]; then
    # Start installation function:
    createBackups
else
    clearConsole
fi

exit 0
