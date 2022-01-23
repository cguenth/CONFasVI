#!/bin/bash
## usage :
## ./sort_Ana1.sh /home/carla/Documents/DebSim/Data/Genese_09_2017/Ana1/14_09_2017

cd "$1"

mkdir Outs Data

mv *.out Outs
mv *.txt Data

ANZ_MIT=$(find Data -name DataToPlot_Mit_*.txt | wc -l)
ANZ_OHNE=$(find Data -name DataToPlot_Ohne_*.txt | wc -l)

cd Data

if [[ "$ANZ_MIT" -ne "0" && "$ANZ_OHNE" -ne "0" ]] ; then

    for i in {"Mit","Ohne"}; do
        mkdir "$i"
        cd "$i"
        for j in {"a","b","c"}; do
            mkdir Mod_"$j"
            for DATEI in $(ls ../DataToPlot_"$i"_*"$j"*.txt); do
                mv ${DATEI} "Mod_"$j""
            done
        done
        cd ..
    done
elif [[ "$ANZ_MIT" -ne "0" && "$ANZ_OHNE" -eq "0" ]] ; then
     mkdir "Mit"
     cd "Mit"
     for j in {"a","b","c"}; do
        mkdir Mod_"$j"
        for DATEI in $(ls ../DataToPlot_Mit_*"$j"*.txt); do
           mv ${DATEI} "Mod_"$j""
        done
     done
     cd ..
else 
     mkdir "Ohne"
     cd "Ohne"
     
     for j in {"a","b","c"}; do
        mkdir Mod_"$j"
        for DATEI in $(ls ../DataToPlot_Ohne_*"$j"*.txt); do
           mv ${DATEI} "Mod_"$j""
        done
     done
     cd ..
fi

        
