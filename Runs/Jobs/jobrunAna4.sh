#!/bin/bash

## usage : 
## ./jobrunAna4.sh /home/carla/Documents/DebSim/Data/Genese_09_2017/Ana2/14_09_2017/ /home/carla/Documents/DebSim/Data/Genese_09_2017/Ana4/14_09_2017


cd /home/carla/Documents/DebSim/

ANZ_MIT=$(find "$1" -type d -name Mit | wc -l)
ANZ_OHNE=$(find "$1" -type d -name Ohne | wc -l)

if [[ "$ANZ_MIT" -ne "0" && "$ANZ_OHNE" -ne "0" ]] ; then
    cd "$2"
    for m in {"Mit","Ohne"}; do
        mkdir "$m"
        cd "$m"
        /home/carla/Documents/DebSim/DataAna4.sh "/home/carla/Documents/DebSim/" ""$1"/"$m"/Mod_c/Ratios/" ""$2"/"$m"" "2,5,10" "0.15,0.3,0.45"
        cd ..
    done
elif [[ "$ANZ_MIT" -ne "0" && "$ANZ_OHNE" -eq "0" ]] ; then
    cd "$2"
    mkdir Mit
    cd Mit
    /home/carla/Documents/DebSim/DataAna4.sh "/home/carla/Documents/DebSim/" ""$1"/Mit/Mod_c/Ratios/" ""$2"/Mit" "2,5,10" "0.15,0.3,0.45"
else
    cd "$2"
    mkdir Ohne
    cd Ohne
    /home/carla/Documents/DebSim/DataAna4.sh "/home/carla/Documents/DebSim/" ""$1"/Ohne/Mod_c/Ratios/" ""$2"/Ohne" "2,5,10" "0.15,0.3,0.45"
fi
    

