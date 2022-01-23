#!/bin/bash

if [ ! -d  "$8" ]
then
	mkdir "$8"
fi


WolframScript -script $HOME/DebSim_HC3/DataGenNew.m  "{$1}" "{$2}" "{$3}" "$4" "$5" "$6" "$7"  "$8" "Data_Acc_"$3"_"$2"_"$1".txt" "$9" > Data_Acc_"$3"_"$2"_"$1".out

mv  Data_Acc_"$3"_"$2"_"$1".out "$8"



