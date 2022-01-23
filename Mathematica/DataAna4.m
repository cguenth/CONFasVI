(* ::Package:: *)

dataPfad=$ScriptCommandLine[[2]]; Print[dataPfad];
dataBox1=$ScriptCommandLine[[3]]; Print[dataBox1];
dataBox2=$ScriptCommandLine[[4]]; Print[dataBox2];
(* Map[ToExpression[#]&,StringSplit["1,2,3,4",","]] outputs a list of integers *)
If[StringContainsQ[$ScriptCommandLine[[5]], ","],
evSizeListe=Map[ToExpression[#]&, StringSplit[StringDelete[$ScriptCommandLine[[5]]," "], ","]], 
evSizeListe={ToExpression[$ScriptCommandLine[[5]]]}];
Print[evSizeListe];
If[StringContainsQ[$ScriptCommandLine[[6]], ","], 
denseListe=Map[ToExpression[#]&, StringSplit[StringDelete[$ScriptCommandLine[[6]]," "], ","]],
denseListe={ToExpression[$ScriptCommandLine[[6]]]}];
Print[denseListe];


If[!MemberQ[$Path,FileNameJoin[{dataPfad,"DebateDynamics"}]],AppendTo[$Path,FileNameJoin[{dataPfad,"DebateDynamics"}]]];
 Print["Package path ", FindFile[ "DebateDynamics`"]];
<<DebateDynamics`


geradeFunc[x_, acc_, evidenceSize_]:=Module[{numberSen,fracIn,accTat,c,m,y},

numberSen=20;
fracIn=Round[ToExpression[ acc]*(numberSen-1.0)];
accTat=N[fracIn/(numberSen-1.0)];

If[evidenceSize==0,c=0.0;m=1.0;,c=0.5*(1-accTat);m=accTat;];

y=x*m+c;
Return[y];
];


geradeListe[docMod_,evidenceSize_,dataHisto_]:=Module[{accListe,xPoints,geradeYpoints,listGerade,xPointsAuswahl},
If[docMod=="doj",
accListe={0.6, 0.8, 1.0};
xPoints=Map[dataHisto[[#,All,1]]&,Range[3]];
xPointsAuswahl=Map[Drop[Drop[xPoints[[#]],1],-1]&,Range[3]];
Print["xPointsAuswahl ", xPointsAuswahl];

(*listGerade=Map[{xPointsOhneDOJ01[[#]],geradeYpoints[[#]]}&,Range[Length[xPointsOhneDOJ01]]];*)

listGerade=Map[Function[index,
Map[Function[xpoint,
{xpoint,geradeFunc[xpoint,accListe[[index]], evidenceSize]}
],xPointsAuswahl[[index]]]
],Range[3]];,

listGerade={};
];
Return[listGerade];
];


EvidenceHistoFunc[Rdata_, docMod_]:= Module[
{evidenceSize,densFestEvSFest,dataHisto,plotRatio, plots, plotRangeList,labelName,accList,
listGerade,plotGerade,listGeradeColor,
colorList, dataColor},

(* Alt !!! Rdata : {evSIze,{{{"acc1","OhneEx",{dataPointsOhneEx}},{"acc1","MItEx",{dataPointsMitEx}}},{"acc2","OhneEx",{dataPointsOhneEx}},{"acc2","MItEx",{dataPointsMitEx}}}}
Neu !!! Rdata : {evSIze,{{{{"acc1","MItEx",{dataPointsMitEx_h,dataPointsMitEx_Non_h}}},{"acc2","MItEx",{dataPointsMitEx_h,dataPointsMitEx_Non_h}}}}*)
evidenceSize=Rdata[[1]];
Print["evidenceSize ", evidenceSize];
densFestEvSFest=Rdata[[2]];
Print["densFestEvSFest ",densFestEvSFest];

(*dataHistoOhneDOJ01=Flatten[DeleteCases[Map[If[densFestEvSFest[[#,2]]=="OhneEx", Take[densFestEvSFest,{#}]]&,Range[Length[densFestEvSFest]]],Null],1];*)
(*dataHisto=Flatten[DeleteCases[Map[If[densFestEvSFest[[#,2]]=="MitEx", Take[densFestEvSFest,{#}]]&,Range[Length[densFestEvSFest]]],Null],1];*)
(*dataHisto=Map[Extract[densFestEvSFest,{#,3,1}]&,Range[Length[densFestEvSFest]]];*)
dataHisto=Map[Extract[densFestEvSFest,{#,3}]&,Range[Length[densFestEvSFest]]];

Print["L\[ADoubleDot]nge dataHisto ",Length[dataHisto]];
Print["dataHisto ",dataHisto];



colorList={Blue,Green,Purple};
(*accList={0.6,0.8,1.0};*)
accList=densFestEvSFest[[All,1]];
dataColor= Map[{dataHisto[[#]],colorList[[#]],accList[[#]]}&,Range[3]];

Which[docMod=="doj",
labelName="DOJ";plotRangeList={{-0.05,1.05},{-0.05,1}};,
docMod=="z",
labelName="Z'";plotRangeList={{-1.1,1.1},{-0.05,1}};,
docMod=="f",
labelName="F";plotRangeList={{-1.1,1.1},{-0.05,1}};
];


Map[Print[#[[2]], ";    ",#[[1,3]]]&,dataColor];
plotRatio=Map[ListPlot[#[[1]],PlotRange->plotRangeList, PlotLegends->{#[[3]]}, AxesLabel->{labelName},PlotMarkers->{\[FilledUpTriangle]},PlotStyle->{#[[2]]}]&,dataColor];

(*If[docMod=="doj",
listGerade=geradeListe[docMod,evidenceSize,dataHisto];
Print["listGerade ", listGerade];
listGeradeColor= Map[{listGerade[[#]],colorList[[#]]}&,Range[3]];
plotGerade=Map[ListLinePlot[#[[1]],PlotRange->plotRangeList, AxesLabel->{labelName},PlotStyle->{#[[2]]}]&,listGeradeColor];
plots=Catenate[{plotGerade,plotRatio}];,
listGeradeColor={};
plots=plotRatio;
];*)

Return[{evidenceSize,plotRatio}];
];


dataDfestEvariabelHistos[RdataDfestEvariabel_, docM_]:=Module[
{density, dataHEvariabel, plotsHEvariabel},

density=RdataDfestEvariabel[[1]];
Print["Dichte ", density];
dataHEvariabel=RdataDfestEvariabel[[2]];

plotsHEvariabel=Map[EvidenceHistoFunc[#, docM]&,dataHEvariabel];

Return[{density,plotsHEvariabel}];
];


dojHistoGraphicOne[dataListe_, modDOC_]:=Module[
{
dens = dataListe[[1]],
evSize = dataListe[[2,1]],
plots = dataListe[[2,2]],
plotName,
densName,
evSizeName,
plotRangeList,
axesName
},

Print["dens ", dens];
Print["evSize ", evSize];


Which[modDOC=="doj",
axesName="DOJ";plotRangeList={{-0.05,1.05},{-0.05,1}};,
modDOC=="z",
axesName="Z'";plotRangeList={{-1.1,1.1},{-0.05,1}};,
modDOC=="f",
axesName="F";plotRangeList={{-1.1,1.1},{-0.05,1}};
];


evSizeName="size of \!\(\*SubscriptBox[\(E\), \(i\)]\)="<>ToString[evSize];
plotName = densName <> "; " <> evSizeName ;

Show[plots,PlotRange->plotRangeList, AspectRatio->1,AxesLabel->{axesName,None},PlotLabel-> plotName,ImageSize->200]
];


dojHistoGraphic[dataListe_, modDOC_]:=GraphicsGrid[
Map[ 
Function[dataD,
(* dataD :  {density,listeDEH} *)
Module[
{
dens = dataD[[1]],
densName,
(* listeDEH  :  Liste mit Eintr\[ADoubleDot]gen {evidenceSize,dataMH,{histoDE,plotRatio,alpha,beta,dojC, doj0, doj1}}} *)
listeDEH = dataD[[2]]
},

densName="D(\[Tau])="<>ToString[dens];

Map[
Function[dataDEH ,
(* dataDEH  :{evidenceSize,{plotRatioOhneDOJ01,plotRatioDOJ01,plotGerade},{histoDE,histoDEOhneDOJ01,histoDEDOJ01},{alpha,beta,dojC}}*)
Module[
{
evSize=dataDEH[[1]],
plots=dataDEH[[2]],
plotName,
evSizeName,
plotRangeList,axesName
},


Which[modDOC=="doj",
axesName="DOJ";plotRangeList={{-0.05,1.05},{-0.05,1}};,
modDOC=="z",
axesName="Z'";plotRangeList={{-1.1,1.1},{-0.05,1}};,
modDOC=="f",
axesName="F";plotRangeList={{-1.1,1.1},{-0.05,1}};
];


evSizeName="size of \!\(\*SubscriptBox[\(E\), \(i\)]\)="<>ToString[evSize];
plotName = densName <> "; " <> evSizeName ;

Show[plots,PlotRange->plotRangeList, AspectRatio->1,AxesLabel->{axesName,None},PlotLabel-> plotName,ImageSize->200]

]
],
listeDEH
]
]
],
dataListe
]
];


(*exportingFunc[acc,histoGraphic,dreiData,dB2,docm,histMod, abm]*)
exportingFunc[plot_,doc_,dataB2_]:=Module[
{pName, abName, pNamePlot},

pNamePlot="Ratio_Grid_"<>doc<>".jpeg";

SetDirectory[dataB2];

Export[pNamePlot,plot, ImageSize->850]
];


dataDocFunc[dataFileAuswahl_, doc_,B2_, evSListe_, dListe_]:=Module[{Auswahl,dataListe,
dataListeDocFest,docNew,assDens,listDensEvS,plotsDensEvS,histoGrid},

Print["doc ", doc];

Auswahl=Select[dataFileAuswahl, StringContainsQ[#,doc]&];
Print["Auswahl ", Auswahl];

(*dataListeDocFest : Liste MIt assoziationen*)
dataListeDocFest=Catenate[Map[Get[#]&,Auswahl]];
Print["Typ von dataListeDocFest[[1]] ", Head[dataListeDocFest[[1]]]];
assDens=Merge[dataListeDocFest,Identity]; 
Print["Keys von assDens ", Keys[assDens]];

listDensEvS=Map[Function[dens,{dens,
Map[Function[evS,
{evS,Merge[assDens[dens],Identity][evS]}],
(*EvidenceSizes {2,5,8,10}
ALT : {10}*)
evSListe]
}],
(*{0.15,0.2,0.3,0.45}*)
(*Keys[assDens]
ALT : {Keys[assDens][[4]]}*)
dListe];

Print["listDensEvS ",listDensEvS];

plotsDensEvS=Map[dataDfestEvariabelHistos[#, doc]&,listDensEvS];

Print["L\[ADoubleDot]nge plotsDensEvS ", Length[plotsDensEvS]];

If[Head[plotsDensEvS]==List, 
histoGrid=dojHistoGraphic[plotsDensEvS, doc];,
Print["Typ plotsDensEvS ", Head[plotsDensEvS]];
Print["plotsDensEvS ", plotsDensEvS];
histoGrid=dojHistoGraphicOne[plotsDensEvS, doc];
];

exportingFunc[histoGrid,doc,B2]
];


dataCollect[dB1_, dB2_, evSListe_, dListe_]:=Module[
{dataFileFull, dataFileAuswahl, docListe, dataFileDocs, histoGraphic, dataAccMerged},

(*Ratio_diff2_0.789474.txt*)
dataFileFull=FileNames["*.txt",{dB1}];
Print["dataFileFull ",dataFileFull];

docListe={"doj", "z", "f"};

dataFileDocs=Map[dataDocFunc[dataFileFull, #,dB2, evSListe, dListe]&,docListe];
];


(*Mod_a oder MOd_b, egal hauptsache nur eins, und Histo-Typ 1*)
dataCollect[dataBox1,dataBox2, evSizeListe, denseListe]
