#!/bin/bash

# electron.sh - Takes in an expert system name relative to the /logic directory
# in the file-system 
# 
# It allows for names like you can find in plan9 so you can use the file-system
# hierarchy to name expert systems. So if you had
# $ElectronFSRoot/logic/do/something.clp then it would be
# 
# electron do/something 

$ElectronFSRoot/sys/bin/electron -f2 $ElectronFSRoot/lib/sys/fs.clp -f2 $ElectronFSRoot/logic/$1.clp -f2 $ElectronFSRoot/lib/sys/chunks/reset-run-exit.clp
