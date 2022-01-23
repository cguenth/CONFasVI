#!/bin/bash

## usage : 
## ./jobrunAna2.sh /home/carla/Documents/DebSim/Data/Genese_09_2017/Ana1/14_09_2017 /home/carla/Documents/DebSim/Data/Genese_09_2017/Ana2/14_09_2017


cd /home/carla/Documents/DebSim/

ANZ_MIT=$(find "$1"/Data -type d -name Mit | wc -l)
ANZ_OHNE=$(find "$1"/Data -type d -name Ohne | wc -l)



if [[ "$ANZ_MIT" -ne "0" && "$ANZ_OHNE" -ne "0" ]] ; then
    LIST_ANA_MOD=$(find "$1"/Data/Mit -type d -name Mod_* | sed 's/.*\(.\)/\1/')
    cd "$2"
    for m in {"Mit","Ohne"}; do
        mkdir "$m"
        cd "$m"
        for l in ${LIST_ANA_MOD}; do
            mkdir "Mod_"$l""
            cd "Mod_"$l""
            mkdir Histos Outs Texts
            for j in {"doj","z","f"}; do
                for k in {"1.","0.8","0.6"}; do
                    /home/carla/Documents/DebSim/DataAna2.sh "/home/carla/Documents/DebSim/" ""$1"/Data/"$m"/Mod_"$l"/" "DataToPlot_"$m"_ExDOJ_"$j"_"$l"_"$k".txt" ""$2"/"$m"/Mod_"$l"/" "$l" "$j" "Eigen" "Ana2_"$m"_ExDOJ_"$j"_"$l"_"$k".out"
                done
            done
            mv *.out Outs
            mv Dreierles*.txt Texts
            mv *.jpeg Histos
            if [ "$l" == "c" ] ; then
                mkdir Ratios
                mv Ratio*.txt Ratios
            fi
            cd ..
        done
        cd ..
    done

elif [[ "$ANZ_MIT" -ne "0" && "$ANZ_OHNE" -eq "0" ]] ; then
    LIST_ANA_MOD=$(find "$1"/Data/Mit -type d -name Mod_* | sed 's/.*\(.\)/\1/')
    cd "$2"
    mkdir Mit
    cd Mit
    for l in ${LIST_ANA_MOD}; do
        mkdir "Mod_"$l""
        cd "Mod_"$l""
        mkdir Histos Outs Texts
        for j in {"doj","z","f"}; do
            for k in {"1.","0.8","0.6"}; do
                /home/carla/Documents/DebSim/DataAna2.sh "/home/carla/Documents/DebSim/" ""$1"/Data/Mit/Mod_"$l"/" "DataToPlot_Mit_ExDOJ_"$j"_"$l"_"$k".txt" ""$2"/Mit/Mod_"$l"/" "$l" "$j" "Eigen" "Ana2_Mit_ExDOJ_"$j"_"$l"_"$k".out"
            done
        done
        mv *.out Outs
        mv Dreierles*.txt Texts
        mv *.jpeg Histos
        if [ "$l" == "c" ]  ; then
                mkdir Ratios
                mv Ratio*.txt Ratios
        fi
        cd ..
    done
else 
    LIST_ANA_MOD=$(find "$1"/Data/Ohne -type d -name Mod_* | sed 's/.*\(.\)/\1/')
    cd "$2"
    mkdir Ohne
    cd Ohne
    for l in ${LIST_ANA_MOD}; do
        mkdir "Mod_"$l""
        cd "Mod_"$l""
        mkdir Histos Outs Texts
        for j in {"doj","z","f"}; do
            for k in {"1.","0.8","0.6"}; do
                /home/carla/Documents/DebSim/DataAna2.sh "/home/carla/Documents/DebSim/" ""$1"/Data/Ohne/Mod_"$l"/" "DataToPlot_Ohne_ExDOJ_"$j"_"$l"_"$k".txt"  ""$2"/Ohne/Mod_"$l"/" "$l" "$j" "Eigen" "Ana2_Ohne_ExDOJ_"$j"_"$l"_"$k".out"
            done
        done
        mv *.out Outs
        mv Dreierles*.txt Texts
        mv *.jpeg Histos
        if [ "$l" == "c" ]  ; then
                mkdir Ratios
                mv Ratio*.txt Ratios
        fi
        cd ..
    done
fi
