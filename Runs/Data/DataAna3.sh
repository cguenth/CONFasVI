#!/bin/bash

## if [ ! -d  "$3" ]
## then
## 	mkdir "$3"
## fi

cd "$1"


#WolframScript -script DataAna3.m  /home/carla/Documents/DebSim/DataAna3.sh "/home/carla/Documents/DebSim/" ""$1"/"$m"/Mod_"$l"/Texts/" ""$2"/"$m"" "$l" "$m"

WolframScript -script DataAna3.m "$1" "$2" "$3" "$4" "$5"> "AB_grid_"$4"_"$5".out"

mv "AB_grid_"$4"_"$5".out" "$3"
