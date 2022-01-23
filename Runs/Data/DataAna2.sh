#!/bin/bash

#if [ ! -d  "$4" ]
#then
    #mkdir "$4"
#fi

cd "$1"


#WolframScript -script DataAna2.m "/home/carla/Documents/DebSim/" "/home/carla/P_15_11_Ana1/" "DataToPlot_doj_0.8.txt" "/home/carla/P_15_11_Ana2/" "a" "doj" "Eigen"

WolframScript -script DataAna2_New.m "$1" "$2" "$3" "$4" "$5" "$6" "$7" > "$8"

mv "$8" "$4"
