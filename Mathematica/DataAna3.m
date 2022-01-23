(* ::Package:: *)

dataPfad=$ScriptCommandLine[[2]]; Print[dataPfad];
dataBox1=$ScriptCommandLine[[3]]; Print[dataBox1];
dataBox2=$ScriptCommandLine[[4]]; Print[dataBox2];
alphabetaMod=ToString[$ScriptCommandLine[[5]]]; Print[alphabetaMod];
typMod=ToString[$ScriptCommandLine[[6]]]; Print[typMod];


If[!MemberQ[$Path,FileNameJoin[{dataPfad,"DebateDynamics"}]],AppendTo[$Path,FileNameJoin[{dataPfad,"DebateDynamics"}]]];
 Print["Package path ", FindFile[ "DebateDynamics`"]];
<<DebateDynamics`


dojHistoGraphic[dataListe_, alpOrBet_, histMod_]:=GraphicsGrid[
Map[ 
Function[dataDoc,
(* dataDDc:  {doc,listeAcc} *)
Module[
{
doc=dataDoc[[1]],
docName,
(* listeAcc  :  Liste mit Eintr\[ADoubleDot]gen {acc,{List3DPlot} *)
listeAcc = dataDoc[[2]]
},

docName=ToString[doc];

Map[
Function[dataAccDoc ,
(* dataAccDoc :{acc,{List3DPlot}*)
Module[
{
acc=dataAccDoc[[1]],
plot=dataAccDoc[[2]]
},

ToString[doc]<>"; acc="<>ToString[acc]<>"; "<>histMod<>alpOrBet

Show[plot,ImageSize->200,AxesLabel->{"D(\[Tau])","\!\(\*SubscriptBox[\(E\), \(k\)]\)", None}]

]
],
listeAcc
]
]
],
dataListe
]
];


(*exportingFunc[acc,histoGraphic,dreiData,dB2,docm,histMod, abm]*)
exportingFunc[plot_,dataB2_, histMod_, abcMod_]:=Module[
{pName, abName, pNamePlot},

Print["histMod ", histMod];
Print["abcMod ", abcMod];

If[histMod=="Mit", pName="_Mit_Extremen_DOJs";, pName="_Ohne_Extreme_DOJs";];
abName=Which[abcMod=="a","_Alpha_",abcMod=="b","_Beta_",abcMod=="c","_EU_"];

pNamePlot="Vergleich"<>abName<>pName<>".jpeg";

SetDirectory[dataB2];

Export[pNamePlot,plot, ImageSize->850]
];


dataDocAccFunc[dataFileName_, abcMod_,dB1_]:=Module[
{acc,data,graph,dataName,abLow,abHigh, dataN},


If[StringTake[dataFileName,{-6}]=="1", acc=1.0;,acc=StringTake[dataFileName,{-12,-5}];];
Print["Akk ", acc];

abLow=Which[abcMod=="a",0.0,abcMod=="b",0.0,abcMod=="c",0.0];
abHigh=Which[abcMod=="a",0.4,abcMod=="b",0.6,abcMod=="c",0.3];

data=Get[dataFileName];
Print["data ", data];

If[abcMod=="b", dataN=Map[{#[[1]],#[[2]],1-#[[3]]}&,data];, dataN=data;];

Print["dataN ", dataN];

graph=ListDensityPlot[{dataN},Mesh->All,InterpolationOrder->3,PlotRange->{{0.15,0.45},{2,10}},PlotLegends->Automatic,ColorFunctionScaling->False,ColorFunction->Function[{z},Hue[(z-abLow)/(abHigh-abLow)]]];
(*graph=ListDensityPlot[{data},Mesh->All,InterpolationOrder->3,PlotRange->{{0.15,0.45},{2,10}},PlotLegends->Automatic,ColorFunctionScaling->False,ColorFunction->Function[{z},Hue[(z-abLow)/(abHigh-abLow)]]];
*)

Return[{acc,graph}];
];


dataDocFunc[dataFileAuswahl_, doc_, abcMod_,dB1_]:=Module[{Auswahl,AuswahlInv,dataListe,dataDoc,docNew},

Print["doc ", doc];

Auswahl=Select[dataFileAuswahl, StringContainsQ[#,doc]&];
AuswahlInv=Reverse[Auswahl];


dataDoc=Map[dataDocAccFunc[#, abcMod,dB1]&,AuswahlInv];

docNew=Which[doc=="doj","DOJ", doc=="z","Z'", doc=="f","F"];

Return[{docNew,dataDoc}];
];


dataAccVariabel[dB1_, dB2_, abcMod_, histMod_]:=Module[
{dataFileFull, dataFileAuswahl, histoGraphic, abFestVal,histM,docListe,dataFileDocs, dojEx},

dataFileFull=FileNames["*.txt",{dB1}];
(*Print["dataFileFull ",dataFileFull];*)

(*histM="Typ_"<>ToString[histMod];*)
dataFileAuswahl=Select[dataFileFull, StringContainsQ[#,histMod]&];
Print["Einzulesende Dateien ", dataFileAuswahl];

(*docListe={"doj"};*)
docListe={"doj", "z", "f"};

dataFileDocs=Map[dataDocFunc[dataFileAuswahl, #, abcMod,dB1]&,docListe];

If[histMod=="Ohne", dojEx="Ohne "<>"\!\(\*SubscriptBox[\(DOJ\), \(Ex\)]\)";, dojEx="Mit "<>"\!\(\*SubscriptBox[\(DOJ\), \(Ex\)]\)";];
Print["dojEx ", dojEx];

abFestVal=Which[abcMod=="a","; \[Beta]=0.75", abcMod=="b","; \[Alpha]=0.05",abcMod=="c"," "];
Print["abcMod ", abcMod];

histoGraphic=dojHistoGraphic[dataFileDocs,abFestVal, dojEx];
(*exportingFunc[plot_,dataB2_, histMod_, modAB_]*)
exportingFunc[histoGraphic,dB2, histMod,abcMod]
];


dataAccVariabel[dataBox1,dataBox2,alphabetaMod,typMod]
