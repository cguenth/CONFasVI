#!/bin/bash

## usage : 
## ./jobrunAna3.sh /home/carla/Documents/DebSim/Data/Genese_09_2017/Ana2/14_09_2017 /home/carla/Documents/DebSim/Data/Genese_09_2017/Ana3/14_09_2017



cd /home/carla/Documents/DebSim/

ANZ_MIT=$(find "$1" -type d -name Mit | wc -l)
ANZ_OHNE=$(find "$1" -type d -name Ohne | wc -l)

if [[ "$ANZ_MIT" -ne "0" && "$ANZ_OHNE" -ne "0" ]] ; then
    LIST_ANA_MOD=$(find "$1"/Mit -type d -name Mod_* | sed 's/.*\(.\)/\1/')
    cd "$2"
    for m in {"Mit","Ohne"}; do
        mkdir "$m"
        cd "$m"
        for l in ${LIST_ANA_MOD}; do
            /home/carla/Documents/DebSim/DataAna3.sh "/home/carla/Documents/DebSim/" ""$1"/"$m"/Mod_"$l"/Texts/" ""$2"/"$m"" "$l" "$m"
        done
        cd ..
    done
elif [[ "$ANZ_MIT" -ne "0" && "$ANZ_OHNE" -eq "0" ]] ; then
    LIST_ANA_MOD=$(find "$1"/Mit -type d -name Mod_* | sed 's/.*\(.\)/\1/')
    cd "$2"
    mkdir Mit
    cd Mit
    for l in ${LIST_ANA_MOD}; do
        /home/carla/Documents/DebSim/DataAna3.sh "/home/carla/Documents/DebSim/" ""$1"/Mit/Mod_"$l"/Texts/" ""$2"/Mit" "$l" "Mit"
    done
else
    LIST_ANA_MOD=$(find "$1"/Mit -type d -name Mod_* | sed 's/.*\(.\)/\1/')
    cd "$2"
    mkdir Ohne
    cd Ohne
    for l in ${LIST_ANA_MOD}; do
        /home/carla/Documents/DebSim/DataAna3.sh "/home/carla/Documents/DebSim/" ""$1"/Ohne/Mod_"$l"/Texts/" ""$2"/Ohne" "$l" "Ohne"
    done

fi
    

