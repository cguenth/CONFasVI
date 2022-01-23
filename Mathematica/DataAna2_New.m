(* ::Package:: *)

dataPfad=$ScriptCommandLine[[2]]; Print[dataPfad];
dataBox1=$ScriptCommandLine[[3]]; Print[dataBox1];
If[StringContainsQ[$ScriptCommandLine[[4]], ","], dataNListe=StringSplit[StringDelete[$ScriptCommandLine[[4]]," "], ","], dataNListe={$ScriptCommandLine[[4]]}];
Print[dataNListe];
dataBox2=$ScriptCommandLine[[5]]; Print[dataBox2];
alphabetaMod=ToString[$ScriptCommandLine[[6]]]; Print[alphabetaMod];
docMod=ToString[$ScriptCommandLine[[7]]]; Print[docMod];
histOptMod=ToString[$ScriptCommandLine[[8]]]; Print[histOptMod];


If[!MemberQ[$Path,FileNameJoin[{dataPfad,"DebateDynamics"}]],AppendTo[$Path,FileNameJoin[{dataPfad,"DebateDynamics"}]]];
 Print["Package path ", FindFile[ "DebateDynamics`"]];
<<DebateDynamics`


EvidenceDataRatioFunc[Rdata_]:= Module[
{evidenceSize,RatioTruthsOhneDOJ01,RatioTruths},
(*NEUER!!!
{evidenceSize,labelName, dataHAll,{tMinHisto,tMaxHisto},tBinSize,abModListe}
NEU!!!
{{listRatioTruths,listGerade},eu}
{evidenceSize,labelName,{{dataHAll,dataNonHAll},dataOhneDOJ01,gesZahlAll},{tMinHisto,tMaxHisto},tBinSize,{abModListe, abModListeOhneDOJ01}}*)
(* Rdata :{evidenceSize,labelName,{{dataHAll,dataNonHAll},dataOhneDOJ01,dataDOJ01, gesZahlAll},{listRatioTruthsOhneDOJ01,listRatioTruthsDOJ01,listGerade},{tMinHisto,tMaxHisto},tBinSize,alpha,beta,dojC}*)

evidenceSize=Rdata[[1]];
RatioTruths=Rdata[[6,1,1]];

Return[{evidenceSize,RatioTruths}];
];


EvidenceDataFunc[Rdata_, abcMod_]:= Module[
{evidenceSize,doc,alpha,beta,data, abcListe},
(*NEUER!!!
{evidenceSize,labelName,{dataHAll,gesZahlAll},{tMinHisto,tMaxHisto},tBinSize,abModListe}
NEU!!!
{evidenceSize,labelName,{{dataHAll,dataNonHAll},dataOhneDOJ01,gesZahlAll},{tMinHisto,tMaxHisto},tBinSize,{abModListe, abModListeOhneDOJ01}}*)
(* Rdata :{evidenceSize,labelName,{{dataHAll,dataNonHAll},dataOhneDOJ01,dataDOJ01, gesZahlAll},{listRatioTruthsOhneDOJ01,listRatioTruthsDOJ01,listGerade},{tMinHisto,tMaxHisto},tBinSize,alpha,beta,dojC}*)
evidenceSize=Rdata[[1]];
(*Print["Evidenzgr\[ODoubleDot]\[SZ]e ", evidenceSize];*)
doc=Rdata[[2]];
abcListe=Rdata[[6]];

data=Which[abcMod=="a",{evidenceSize,abcListe[[1]]},abcMod=="b",{evidenceSize,abcListe[[2]]},abcMod=="c",{evidenceSize,abcListe[[2]]}];

Print[" abMod ",abcMod," data ", data];

Return[data];
];


EvidenceHistoFunc[Rdata_, docMod_, histOptMod_, abMod_]:= Module[
{evidenceSize,histoDE,dataHisto,optMod,histoToPlot,gesZahlAll,
plotRatio,plotGerade,
tMinMaxHisto,tBinSize,labelName,specCount,abModListe,plotLine,
g, binCounts
},

(* Rdata :
NEUER!!!
{evidenceSize,labelName,dataHAll,{tMinHisto,tMaxHisto},tBinSize,abModListe}

NEU!!!
{evidenceSize,labelName,{dataHAll,dataOhneDOJ01},{tMinHisto,tMaxHisto},tBinSize,{abModListe, abModListeOhneDOJ01}}
ALT!!!
{evidenceSize,labelName,{{dataHAll,dataNonHAll},dataOhneDOJ01,dataDOJ01, gesZahlAll},{listRatioTruthsOhneDOJ01,listRatioTruthsDOJ01,listGerade},{tMinHisto,tMaxHisto},tBinSize,alpha,beta,dojC}*)
evidenceSize=Rdata[[1]];
Print["evidenceSize ", evidenceSize];
labelName=Rdata[[2]];

histoToPlot=Rdata[[3]];
gesZahlAll=Length[Rdata[[3,1]]];
Print["gesZahlAll ",gesZahlAll];

tMinMaxHisto=Rdata[[4]];
tBinSize=Rdata[[5]];

abModListe=Rdata[[6]];

If[histOptMod=="Eigen", 
specCount[bins_,counts_]:=counts/gesZahlAll;
optMod=specCount;,
 optMod=histOptMod;];

(*{g,{binCounts}}=Reap[Histogram[RandomVariate[NormalDistribution[0,1],200],{-3,3,0.25},Function[{bins,counts},Sow[counts]]]];
Print[Flatten[binCounts]];
Append[{0., 1.05},0.05] returns {0., 1.05,0.05}*)

{g,{binCounts}}=Reap[Histogram[histoToPlot[[1]],Append[tMinMaxHisto,tBinSize],Function[{bins,counts},Sow[counts]]]];
Print["binCountsFlat ",Flatten[binCounts]];

(*Print["Das sind Daten f\[UDoubleDot]r Histogramm ", histoToPlot];*)
histoDE=Histogram[N[histoToPlot],{tBinSize},optMod,PlotRange->{tMinMaxHisto,Automatic},AxesOrigin->{0,0},AxesLabel->{labelName,None},
ChartLayout->"Stacked",ChartStyle->{Orange,LightOrange}];
(*"Probability"*)


If[abMod=="c",
(*abModListe={{listRatioTruths,listGerade},eu} \[FilledDownTriangle] \[FilledUpTriangle]*)
plotRatio=ListPlot[abModListe[[1,1]],PlotRange->{tMinMaxHisto},AxesLabel->{labelName,None},PlotMarkers->{\[FilledUpTriangle]},PlotStyle->{Red}];
plotGerade=ListLinePlot[abModListe[[1,2]],PlotRange->{tMinMaxHisto},AxesLabel->{labelName,None},PlotStyle->{Black,Thickness[0.01],Dashing[0.02]}];
Return[{evidenceSize,{histoDE,plotRatio,plotGerade},{abModListe[[2]]}}];, 

plotLine=Graphics[{Purple,Dashed,Line[{{abModListe[[3]],0},{abModListe[[3]],1}}]}];
Return[{evidenceSize,{histoDE,plotLine},abModListe}];
];
];


(*dataDfestEvariabelDreier[#,docm, histMod,histOptMod, abm]*)
dataDfestEvariabelDreier[RdataDfestEvariabel_, docM_, histOptMod_, abMod_]:=Module[
{density, dataHEvariabel, dojsDfestEvariabel,dataABCEVariabel,
plotsHEvariabel},

density=RdataDfestEvariabel[[1]];
Print["Dichte ", density];
dataHEvariabel=RdataDfestEvariabel[[2]];

(* (i) HIER : Kein evSize min, f\[UDoubleDot]r DOJ 
If[docM=="doj", dataHEvariabelAuswahl=Drop[dataHEvariabel,1];,dataHEvariabelAuswahl=dataHEvariabel;];
*)

dataABCEVariabel=Map[EvidenceDataFunc[#, abMod]&, dataHEvariabel];
(*Print["dataAlphaBetaEVariabel",dataAlphaBetaEVariabel];*)

Return[{density,dataABCEVariabel}];
];


dataDfestEvariabelHistos[RdataDfestEvariabel_, docM_, histOptMod_, abMod_]:=Module[
{density, 
dojsDfestEvariabel,dataAlphaBetaEVariabel,
dataHEvariabel,
dataHEvariabelAuswahl,dataHEvariabelAuswahlEnd, stepsize,
 plotsHEvariabel},

density=RdataDfestEvariabel[[1]];
Print["Dichte ", density];
(*Ist dataHEvariabel eine Liste von Listen? Ja, denn es kommt aus file nach merging
{{"0.15", {{ 0, {<|"DOJp"\[Rule] 0.5, "DOJE"\[Rule] 0.5, "DOJpAndE"\[Rule] 0.5|>, ...}}, {2, ...},...},{"0.2", ...}*)
dataHEvariabel=RdataDfestEvariabel[[2]];

(*Hier verliere ich evSizes, aber nur f\[UDoubleDot]r Histos !!! *)

(* (i) Entferne EvSize 0 falls doj
If[docM=="doj", dataHEvariabelAuswahl=Drop[dataHEvariabel,1];, dataHEvariabelAuswahl=dataHEvariabel;];*)

(* (ii) Nimm die kleinste und gr\[ODoubleDot]\[SZ]te Evsize und diejenige, die ungef\[ADoubleDot]hr in der Mitte liegt *)
stepsize=Round[N[(Length[dataHEvariabel]-1)/2]];
dataHEvariabelAuswahlEnd={dataHEvariabel[[1]],dataHEvariabel[[1+stepsize]],dataHEvariabel[[-1]]};


plotsHEvariabel=Map[EvidenceHistoFunc[#, docM, histOptMod,abMod]&,dataHEvariabelAuswahlEnd];
(*Print["plotsHEvariabel ", plotsHEvariabel];*)

(*ALT Return[{density,plotsHEvariabel}]*)
Return[{density,plotsHEvariabel}];
];


dataDfestEvariabelRatios[RdataDfestEvariabel_, docM_]:=Module[
{density,dataHEvariabel,dataHEvariabelAuswahl,plotsHEvariabel},

density=RdataDfestEvariabel[[1]];
dataHEvariabel=RdataDfestEvariabel[[2]];

(* (i) HIER : Kein evSize max, f\[UDoubleDot]r Ratios 
If[docM=="doj", dataHEvariabelAuswahl=Drop[Drop[dataHEvariabel,1],-1];,dataHEvariabelAuswahl=Drop[dataHEvariabel,-1];]

 (ii) HIER: Nur EvSize  max
If[docM=="doj", dataHEvariabelAuswahl=Take[Drop[dataHEvariabel,1],{-1}];,dataHEvariabelAuswahl=Take[dataHEvariabel,{-1}];];

(i) HIER : Kein evSize min, f\[UDoubleDot]r Ratios 
If[docM=="doj", dataHEvariabelAuswahl=Drop[dataHEvariabel,1];,dataHEvariabelAuswahl=dataHEvariabel;];
*)



plotsHEvariabel=Map[EvidenceDataRatioFunc[#]&,dataHEvariabel];

(*ALT:{evidenceSize,{RatioTruths,RatioTruthsOhneDOJ01}}
NEU:{evidenceSize,RatioTruths}*)

Return[{density,plotsHEvariabel}];
];


dojHistoGraphic[dataListe_, modDOC_, acc_]:=GraphicsGrid[
Map[ 
Function[dataD,
(* dataD :  {density,listeDEH} *)
Module[
{
dens = dataD[[1]],
densName,
(* listeDEH  :  {density, dataDEH } *)
listeDEH = dataD[[2]]
},

densName="D(\[Tau])="<>ToString[dens];

Map[
Function[dataDEH ,
(* {evidenceSize,{histoDE,plotRatio,plotGerade},{abModListe[[2]]}} bzw. {evidenceSize,{histoDE,plotLine},abModListe}
{evidenceSize,{histoDE,plotRatio,plotGerade},{eu} bzw. {evidenceSize,{histoDE,plotLine},{alpha, beta, dojC}}
*)
Module[
{
evSize=dataDEH[[1]],
histoToPlot=dataDEH[[2]],
abcModListe=dataDEH[[3]],
plotName,
testSizeName,
evSizeName,
plotRangeList,axesName
},


Which[modDOC=="doj",
axesName="DOJ";plotRangeList={{0,1.05},{0,1}};,
modDOC=="z",
axesName="\!\(\*SuperscriptBox[\(Z\), \(*\)]\)";plotRangeList={{-1,1.1},{0,1}};,
modDOC=="f",
axesName="F";plotRangeList={{-1,1.1},{0,1}};
];


evSizeName="size of \!\(\*SubscriptBox[\(E\), \(i\)]\)="<>ToString[evSize];
plotName = densName <> "; " <> evSizeName ;
If[Length[abcModListe]>1,
testSizeName="\[Alpha] = "<>ToString[abcModListe[[1]]]<>"; \[Beta] = "<>ToString[abcModListe[[2]]];,
testSizeName="eu = "<>ToString[abcModListe[[1]]];
];

Show[histoToPlot,PlotRange->plotRangeList,AspectRatio->1,AxesLabel->{axesName,None},PlotLabel-> plotName,ImageSize->200]

"acc = "<>ToString[acc]<>"; "<>testSizeName

]
],
listeDEH
]
]
],
dataListe
]
];


ratioDataFunction[dataListe_,entMod_, acc_]:=Module[{},
Map[Function[dataD,
(* dataD :  {density,listeDEH} *)
Module[{density=dataD[[1]],
listeDEH=dataD[[2]]},
Map[Function[dataDEH,
(* dataDEH  :{evidenceSize,RatioTruths}*)
Module[
{evidenceSize=dataDEH[[1]],
ratioPoint},
Print["dataDEH ",dataDEH];
ratioPoint=dataDEH[[2]];
<|ToExpression[density]-><|evidenceSize->{acc,entMod,ratioPoint}|>|>
]
],listeDEH]
]
],dataListe]
];


dreierlesFunction[dataListe_, mod_]:=Module[{},
Map[Function[dataD,
(* dataD :  {density,listeDEH} *)
Module[{density=dataD[[1]],
listeDEH=dataD[[2]]},
Map[Function[dataDEH,
(* dataDEH  :{evidenceSize,alphaOderbeta}*)
Module[
{evidenceSize=dataDEH[[1]],
alphaOderbeta=dataDEH[[2]]},

{ToExpression[density],evidenceSize,alphaOderbeta}
]
],listeDEH]
]
],dataListe]
];


(*exportingFunc[acc,histoGraphic,dreiData,dB2,docm,histMod, abm]*)
exportingFunc[acc_,histoGraphic_,data_, dataB2_, moddoc_, entMod_, modAB_]:=Module[
{pName, pNameFile,numberSen, fracIn, accTat,pNameRatioFile,DreierlesData,ratioData},

numberSen=20;
Print["Number of sentences ", numberSen];
fracIn=Round[ToExpression[acc]*(numberSen-1.0)];
accTat=N[fracIn/(numberSen-1.0)];
Print["Korrektheit von E ", accTat];


pNameFile="Dreierles_"<>ToString[moddoc]<>"_"<>ToString[modAB]<>"_"<>entMod<>"_ExDOJ_"<>ToString[accTat]<>".txt";
pName="Histos_"<>ToString[moddoc]<>"_"<>ToString[modAB]<>"_"<>entMod<>"_ExDOJ_"<>ToString[accTat]<>".jpeg";

SetDirectory[dataB2];

If[Length[data]>1,
pNameRatioFile="Ratio_"<>ToString[moddoc]<>"_"<>entMod<>"_ExDOJ_"<>ToString[accTat]<>".txt";
Print[pNameRatioFile, " ist nicht leer"];
Export[pNameRatioFile,{data[[2]]}];
];

Export[pNameFile,{data[[1]]}];
Export[pName,histoGraphic, ImageSize->800];
];


dataHistoModFunc[dataFile_,abm_, docm_, entMod_,histOptMod_,acc_, dB2_]:=Module[
{dataDreier,dataHisto, dataHistoAuswahl, histoGraphic,dreiData, data,
dataRatio,dataRatioAuswahl,ratioData,ratioDataMit,ratioDataOhne, modDojEx,dataToExport,
stepsize},

dataHisto=Map[dataDfestEvariabelHistos[#,docm, histOptMod, abm]&,dataFile];
(* ALT: Entfernen von Density= 0.2, i.e. dataHisto with 2nd nth element dropped : dataHistoNew= Drop[dataHisto,{2}] 
NEU : Nimm das kleinste und gr\[ODoubleDot]\[SZ]te Dichte, und diejenige die ungef\[ADoubleDot]hr in der Mitte liegt *)
stepsize=Round[N[(Length[dataHisto]-1)/2]];
dataHistoAuswahl={dataHisto[[1]],dataHisto[[1+stepsize]],dataHisto[[-1]]};
histoGraphic=dojHistoGraphic[dataHistoAuswahl, docm, acc];

dataDreier=Map[dataDfestEvariabelDreier[#,docm, histOptMod, abm]&,dataFile];
dreiData=Flatten[dreierlesFunction[dataDreier, abm],1];
Print["dreiData ", dreiData];


If[entMod=="Mit" && abm=="c",
dataRatio=Map[dataDfestEvariabelRatios[#, docm]&,dataFile]; 
(*outputs Liste mit Eintr\[ADoubleDot]gen {evidenceSize,{RatioTruths,RatioTruthsOhneDOJ01}}
NEU:{evidenceSize,RatioTruths}*)
(*Entfernen von Density= 0.2*)
(*ratioDataFunction returns <|ToExpression[density]-><|evidenceSize->{acc,entMod,ratioPoint}|>|>*)
ratioData=ratioDataFunction[dataRatio,entMod,acc];
Print["Values von ratioData ", Values[ratioData]];
Print["Keys von ratioData ", Keys[ratioData]];
dataToExport={dreiData,ratioData};,

dataToExport={dreiData};
];

exportingFunc[acc,histoGraphic,dataToExport,dB2,docm,entMod, abm]
];


dataAccVariabel[dataName_, dB1_, dB2_, abm_, docm_,histOptMod_]:=Module[
{dataFile, dataFileAuswahl, dojDL, dojHP, acc, dataFileFullName, densityList, entMod},

(*DataToPlot_doj_0_8.txt, DataToPlot_doj_1..txt*)
If[StringTake[dataName,{-6}]=="1", acc=1.0;,acc=StringReplace[StringTake[dataName,{-7,-5}],"_"->"."];];
(*pName ="DataToPlot_"<>entMod<>"ExDOJ"<>docMod<>"_"<>ToString[abMod]<>"_"<>ToString[acc]<>".txt";*)
If[StringTake[dataName,{12}]=="M",
entMod="Mit";,
entMod="Ohne";
];

dataFileFullName=FileNameJoin[{dB1,dataName}];
Print["Einzulesende Datei ", dataFileFullName];
dataFile=Get[dataFileFullName];


dojDL=dataHistoModFunc[dataFile,abm, docm, entMod,histOptMod,acc, dB2];
];


Map[dataAccVariabel[#,dataBox1,dataBox2,alphabetaMod,docMod,histOptMod]&,dataNListe]
