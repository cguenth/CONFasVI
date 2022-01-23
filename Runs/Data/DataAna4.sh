#!/bin/bash

#if [ ! -d  "$3" ]
#then
#	mkdir "$3"
#fi

cd "$1"


#WolframScript -script /home/carla/Documents/DebSim/DataAna4.sh "/home/carla/Documents/DebSim/" ""$1"/"$m"/Mod_c/Ratios/" ""$2"/"$m""

WolframScript -script DataAna4.m "$1" "$2" "$3" "$4" "$5"> "Ratio_grid.out"

mv "Ratio_grid.out" "$3"
