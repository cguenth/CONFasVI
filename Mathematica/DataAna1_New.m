(* ::Package:: *)

dataPfad=$ScriptCommandLine[[2]]; Print[dataPfad];
dataBox1=$ScriptCommandLine[[3]]; Print[dataBox1];
If[StringContainsQ[$ScriptCommandLine[[4]], ","], dataNListe=StringSplit[StringDelete[$ScriptCommandLine[[4]]," "], ","], dataNListe={$ScriptCommandLine[[4]]}];
Print[dataNListe];
dataBox2=$ScriptCommandLine[[5]]; Print[dataBox2];
alphabetaMod=ToString[$ScriptCommandLine[[6]]]; Print[alphabetaMod];
docMod=ToString[$ScriptCommandLine[[7]]]; Print[docMod];
entailMod=ToString[$ScriptCommandLine[[8]]]; Print[entailMod];


If[!MemberQ[$Path,FileNameJoin[{dataPfad,"DebateDynamics"}]],AppendTo[$Path,FileNameJoin[{dataPfad,"DebateDynamics"}]]];
 Print["Package path ", FindFile[ "DebateDynamics`"]];
<<DebateDynamics`


qoudFunc[dojpgegE_, dojE_,dojp_]:=Module[{quod,dojNonpgegE,dojNonp,dojEgegp,dojEgegNonp},

dojEgegp=dojpgegE*dojE/dojp;

dojNonpgegE=1-dojpgegE;
dojNonp=1-dojp;
dojEgegNonp=(dojNonpgegE*dojE)/dojNonp;

If[Boole[dojEgegp+dojEgegNonp==0]==1, Print["DOJ(E|p)=-DOJ(E|non-p)!, Division durch NUll "]];
quod=(dojEgegp-dojEgegNonp)/(dojEgegp+dojEgegNonp);

Return[quod]; 
];



(*z*)
zCalc[i_, dp_,dpgegE_]:=Module[{hilf},
hilf=Which[
Boole[dpgegE[[i]]>= dp[[i]] && 0<dp[[i]]<1]==1,(dpgegE[[i]]-dp[[i]])/(1-dp[[i]]),
Boole[dpgegE[[i]]==1 && dp[[i]]==1]==1, 1,
Boole[dpgegE[[i]]==0 && dp[[i]]==0]==1, -1, 
Boole[dpgegE[[i]]<dp[[i]]]==1,(dpgegE[[i]]-dp[[i]])/dp[[i]]
];
Return[hilf];
];

(*f*)
fCalc[i_, dp_, dE_,dpgegE_]:=Module[{dNonpgegE,doch2},
dNonpgegE=1-dpgegE;

doch2=Which[
Boole[dpgegE[[i]]!= 1 && dNonpgegE[[i]]!=1]==1,qoudFunc[dpgegE[[i]], dE[[i]],dp[[i]]],
Boole[dpgegE[[i]]==1 && dE[[i]]!= 0]==1,1,
Boole[dNonpgegE[[i]]==1]==1,-1
];

Return[doch2]; 
];


DocCalc2Func[docModus_, docsIngListe_]:=Module[
{dojpgegE, dojE ,dojp,dojNonp,dojEgegNonp,dojNonpgegE, dojEgegp, quodp,quodNonp,docp,docNonp},

Which[docModus=="doj", 
dojpgegE=docsIngListe[[1]];
docp=dojpgegE; docNonp=1-docp;,

docModus=="z",
dojpgegE=docsIngListe[[1]];
dojp=docsIngListe[[2]];
dojNonp=1-dojp;
dojNonpgegE=1-dojpgegE;

docp=Table[zCalc[j,dojp,dojpgegE],{j,1,Length[dojp]}];
docNonp=Table[zCalc[j,dojNonp,dojNonpgegE],{j,1,Length[dojp]}];,

docModus=="f",
(*{dojp,dojE,dojpgegE}*)
dojp=docsIngListe[[1]];
dojNonp=1-dojp;
dojE=docsIngListe[[2]];
dojpgegE=docsIngListe[[3]];
dojNonpgegE=1-dojpgegE;

docp=Table[fCalc[j,dojp,dojE,dojpgegE],{j,1,Length[dojp]}];
docNonp=Table[fCalc[j,dojNonp,dojE,dojNonpgegE],{j,1,Length[dojp]}];
];

Return[{docp, docNonp}];
];



AlphaBetaCalc[dataH_, dataNonH_, abModus_]:=Module[
{listH0sortiert,listH1sortiert, gesZ, indDOJcc, numH0w,numH1WMinusw, dojcc, alp, bet},

listH0sortiert=Sort[dataNonH,Less];
listH1sortiert=Sort[dataH,Less];

gesZ=Length[listH0sortiert];

Which[abModus=="a", 

(*1/4 aller wahren Aussagen soll im kritischen Bereich von H0 liegen *)
indDOJcc=Round[gesZ*0.75];
dojcc=N[listH1sortiert[[indDOJcc]],4];

(*NICHT : numberH0w=Length[listH0sortiertiert[[indizeeDOJC+1;;]]];*)
numH0w=Length[Cases[listH0sortiert, x_/; x>=dojcc]];
alp=N[numH0w/gesZ];

numH1WMinusw=Length[listH1sortiert[[;;indDOJcc]]];
bet=N[numH1WMinusw/gesZ];,

abModus=="b",

alp = 0.05;

indDOJcc=Round[gesZ*0.95];
dojcc=N[listH0sortiert[[indDOJcc]], 4];

numH1WMinusw = Length[Cases[listH1sortiert, x_ /; x < dojcc]];
bet = N[numH1WMinusw/gesZ];
 ];

Print["Alpha ", alp, " Beta ", bet, " DOJC ", Round[dojcc, 0.0001]];
Return[{alp, bet, dojcc}];
];


indexMengeFunc[data_,tBinSize_, tMinHisto_,tMaxHisto_]:=Module[
{binCountsFlat,binCounts,g,i,indexMenge,indexMengeFlach,gesamtAnzahl},

{g,{binCounts}}=Reap[Histogram[data,{tMinHisto,tMaxHisto,tBinSize},Function[{bins,counts},Sow[counts]]]];
binCountsFlat=Flatten[binCounts];
gesamtAnzahl=Sum[i,{i,binCountsFlat}];

(*Print["Anzahl der Eintr\[ADoubleDot]ge pro Intervall des Histograms ", binCountsFlat];
Print["Gesamtzahl der Eintr\[ADoubleDot]ge ", gesamtAnzahl];*)

Return[{binCountsFlat,gesamtAnzahl}];
];



ratioPointMeanFunc[data_,index_, tMinHisto_,tBinSize_]:=Module[{point, selection},

(*If[index==11, Print["Ist 0 ein Element der Datenmenge? ",MemberQ[data,0]];
Print["Untere Grenze f\[UDoubleDot]r Index 11 ", Round[tMinHisto+(tBinSize*index-tBinSize), 0.01], "  Obere Grenze f\[UDoubleDot]r Index 11 ",Round[tMinHisto+tBinSize*index,0.01]];];*)
selection=Select[data, (Round[tMinHisto+(tBinSize*index-tBinSize), 0.01] <= # < Round[tMinHisto+tBinSize*index,0.01])&];
If[Length[selection]==0, Print["Selection ist leere Menge f\[UDoubleDot]r inedx ", index]; point=Null;,point=Round[Mean[selection], 0.0001];];

Return[point];
];


geradeFunc[x_, acc_, evidenceSize_]:=Module[{numberSen,fracIn,accTat,c,m,y},

numberSen=20;
fracIn=Round[ToExpression[ acc]*(numberSen-1.0)];
accTat=N[fracIn/(numberSen-1.0)];

If[evidenceSize==0,c=0.0;m=1.0;,c=0.5*(1-accTat);m=accTat;];

y=x*m+c;
Return[y];
];



euAntHFunc[dataListe_,tBinSize_, tMinHisto_,tMaxHisto_, modT_, docMod_, acc_, evidenceSize_]:=Module[
{dataAll,dataH,dataNonH,
gesamtAnzahl,indexMenge2,BinCountsDataH,BinCountsDataNonH,BinCountsDataHNonH,pHgegE,pNonHgegE,pE,eu,
hUndNonh, h,nonH, ratioTruths, eMean, geradeYpoints, listGerade},

(*dataListe = {dataH, dataNonH}*)
dataH=dataListe[[1]];
dataNonH=dataListe[[2]];
dataAll=Catenate[dataListe];

(*indexMengeFunc returns {binCounts, SumOfBinCounts}
indexMengeFunc[data_,tBinSize_, tMinHisto_,tMaxHisto_]*)
hUndNonh=indexMengeFunc[dataAll,tBinSize, tMinHisto,tMaxHisto];
h=indexMengeFunc[dataH,tBinSize, tMinHisto,tMaxHisto];
nonH=indexMengeFunc[dataNonH,tBinSize, tMinHisto,tMaxHisto];

indexMenge2=Position[hUndNonh[[1]],x_/;x>10];
Print["indexMenge2 ", indexMenge2];

BinCountsDataH=Extract[h[[1]],indexMenge2];
BinCountsDataNonH=Extract[nonH[[1]],indexMenge2];
BinCountsDataHNonH=Extract[hUndNonh[[1]],indexMenge2];

pHgegE=BinCountsDataH/BinCountsDataHNonH;
pNonHgegE=BinCountsDataNonH/BinCountsDataHNonH;
pE=BinCountsDataHNonH/hUndNonh[[2]];

If[modT=="eu",
Print["h ", h];
Print["nonH ", nonH];
Print["hUndNonh ", hUndNonh];
eu=Total[Map[(((pHgegE[[#]]-0.5)^2+(pNonHgegE[[#]]-0.5)^2)*pE[[#]])&, Range[1,Length[pE]]]];
Print["eu ", eu];
Return[eu];,

eMean=Map[ratioPointMeanFunc[dataAll,#,tMinHisto,tBinSize]&,Flatten[indexMenge2]];
ratioTruths=Map[{eMean[[#]], N[pHgegE[[#]]]}&, Range[1,Length[pHgegE]]];
Print["ratioTruths ", ratioTruths]; 

If[docMod=="doj",
geradeYpoints=Map[geradeFunc[#,acc, evidenceSize]&,eMean];
listGerade=Map[{eMean[[#]],geradeYpoints[[#]]}&,Range[1,Length[eMean]]],
listGerade={};
];

Return[{ratioTruths, listGerade}];
];
];


EvidenceHistoDataFunc[Rdata_, acc_, abMod_, docMod_]:= Module[
{evidenceSize, docsData,tBinSize,tMinHisto,tMaxHisto,labelName,
datap, dataNonp, dataAll,
euList,abList,listDens, viList
},

evidenceSize=Rdata[[1]];
docsData=Rdata[[2]];
Print["evidenceSize ", evidenceSize];

(* ALT: Rdata[[2]] {{docpOhneDOJ01, docNonpOhneDOJ01},{docpDOJ01, docNonpDOJ01}}
NEU: {docs} das heisst {docp, docNonp}*)

datap=docsData[[1]];
dataNonp=docsData[[2]];

dataAll={datap,dataNonp};


Which[docMod=="doj",
tMinHisto=0.0;tMaxHisto=1.05;tBinSize=0.05;labelName="DOJ";,
docMod=="z",
tMinHisto=-1.0;tMaxHisto=1.1;tBinSize=0.1;labelName="Z";,
docMod=="f",
tMinHisto=-1.0;tMaxHisto=1.1;tBinSize=0.1;labelName="F";
];


If[abMod=="c",
(* eu-Berechnung  *)
viList={"antT", "eu"};
euList=Map[euAntHFunc[dataAll,tBinSize,tMinHisto,tMaxHisto, #, docMod, acc, evidenceSize]&,viList];
listDens=euList;,
(* alpha/beta-Berechnung  *)
abList=AlphaBetaCalc[datap, dataNonp, abMod];
listDens=abList;
];

Return[{evidenceSize,labelName,dataAll,{tMinHisto,tMaxHisto},tBinSize,listDens}];
];


(*DocCalc[dataDOJp,dataDOJE,dataDOJpAndE]*)
entailmentCalc[dojp_, dojE_, dojpAndE_]:=Module[
{dojpgegE, posDOJ01},

If[Length[Position[dojE, x_/;x==0]]!=0, Print["Positionen mit dojE ist Null! ",Position[dojE, x_/;x==0]];];

dojpgegE=dojpAndE/dojE;
posDOJ01=Position[dojpgegE, x_/;x==1||x==0];

(*Print["Positionen mit DOJ ist 0 oder 1 : ", posDOJ01];*)
Print["Anzahl der DOJ  mit 0 oder 1 : ", Length[posDOJ01]];

Return[posDOJ01];
];


dataD045EFest[dataDE_]:=Module[
{evidenceSize,dataM,dataDOJp,dataDOJE,dataDOJpAndE,dataDOC, entHilf},

evidenceSize=dataDE[[1]];
(*Merge[{<|a\[Rule]1,b\[Rule]2|>,<|a\[Rule]5,b\[Rule]10|>},Identity] outputs \[LeftAssociation]a\[Rule]{1,5},b\[Rule]{2,10}\[RightAssociation]*)
dataM=Merge[dataDE[[2]],Identity];
dataDOJp=dataM["DOJp"];
dataDOJE=dataM["DOJE"];
dataDOJpAndE=dataM["DOJpAndE"];

entHilf=entailmentCalc[dataDOJp, dataDOJE,dataDOJpAndE];

Return[{evidenceSize,entHilf}];
];


dataD045Evariabel[dataD_]:=Module[
{density, posEnt},

density=dataD[[1]];Print["Dichte ", density];

posEnt=Map[dataD045EFest[#]&,dataD[[2]]];

Return[{density,posEnt}];
];


(*DocCalc[dataDOJp,dataDOJE,dataDOJpAndE]*)
DocCalc[dojp_, dojE_, dojpAndE_, docModus_, entHilf_]:=Module[
{dojpgegE, posDOJ01, dojpgegETat, dojpTat, dojETat, docs},

If[Length[Position[dojE, x_/;x==0]]!=0, Print["Positionen mit dojE ist Null! ",Position[dojE, x_/;x==0]];];

dojpgegE=dojpAndE/dojE;

If[!StringQ[entHilf],

dojpgegETat=Delete[dojpgegE, entHilf];
If[Length[Position[dojpgegETat, x_/;x==0]]!=0, Print["non-p folgt aus E! ",Position[dojpgegETat, x_/;x==0]];];
If[Length[Position[dojpgegETat, x_/;x==1]]!=0, Print["p folgt aus E! ",Position[dojpgegETat, x_/;x==1]];];
dojpTat=Delete[dojp, entHilf];
dojETat=Delete[dojE, entHilf];,

dojpgegETat=dojpgegE;
dojpTat=dojp;
dojETat=dojE
];


Which[docModus=="doj", 
docs=DocCalc2Func[docModus,{dojpgegETat}];,
docModus=="diff",
docs=DocCalc2Func[docModus,{dojpgegETat, dojpTat}];,
docModus=="z",
docs=DocCalc2Func[docModus,{dojpgegETat, dojpTat}];,
docModus=="f",
docs=DocCalc2Func[docModus,{dojpTat,dojETat,dojpgegETat}];
];

(*ALT: Return[{docsOhne01, docs01}]
NEU: Return[{docs}]*)
Return[docs];
];


DOCDFestEFestFunc[dataDE_, docModus_, entMod_]:=Module[
{evidenceSize,dataM,dataDOJp,dataDOJE,dataDOJpAndE,dataDOC, entHilf, dataDETat},

(*Print["dataDE ", dataDE];*)

(*If[StringQ[entMod],
Print[entMod];
dataDETat=dataDE;
entHilf="MitEnt";,
dataDETat=dataDE[[1]];
entHilf=dataDE[[2]];
];*)

evidenceSize=dataDE[[1]];
Print["evidenceSize ", evidenceSize];
(*Merge[{<|a\[Rule]1,b\[Rule]2|>,<|a\[Rule]5,b\[Rule]10|>},Identity] outputs \[LeftAssociation]a\[Rule]{1,5},b\[Rule]{2,10}\[RightAssociation]*)
dataM=Merge[dataDE[[2]],Identity];
dataDOJp=dataM["DOJp"];
dataDOJE=dataM["DOJE"];
dataDOJpAndE=dataM["DOJpAndE"];

dataDOC=DocCalc[dataDOJp,dataDOJE,dataDOJpAndE, docModus, entMod];

Return[{evidenceSize,dataDOC}];
];


(*dataDfestEvariabel[#,acc,abm, docm]*)
dataDfestEvariabel[dataD_, acc_,abM_, docM_, entMod_]:=Module[
{density, dataDTat,
docsD,dataHisto},

density=dataD[[1]];
Print["Dichte ", density];
(*
(*list1={a,b,c,d}; list2={1,2,3,4}; 
Map[{list1[[#]],list2[[#]]}&,Range[4]] outputs {{a,1},{b,2},{c,3},{d,4}}*)
If[!StringQ[entMod],
Print["Anzahl Evidenzen bez dataD ", Length[dataD[[2]]]];
(*dataDTat=Map[{dataD[[2]][[#]],entMod[[#,2]]}&, Range[Length[entMod]]];*)
dataDTat=Map[{dataD[[2]][[#]],entMod}&, Range[dataD[[2]]]];,
dataDTat=dataD[[2]];
];*)

docsD=Map[DOCDFestEFestFunc[#, docM, entMod]&,dataD[[2]]];
dataHisto=Map[EvidenceHistoDataFunc[#, acc,abM, docM]&,docsD];

Return[{density,dataHisto}];
];


exportingFunc[dataName_, acc_, dataDOJHP_, dataB2_, docMod_, abMod_, entMod_]:=Module[
{pName},

pName ="DataToPlot_"<>entMod<>"_ExDOJ_"<>docMod<>"_"<>ToString[abMod]<>"_"<>ToString[acc]<>".txt";

SetDirectory[dataB2];

Export[pName,{dataDOJHP}];
];




delOfEvSizesFunc[dataFile_,docm_]:=Module[
{numDensities,numEvSizes,evSizeToDel,dataToDel, dataFileAuswahl},

(* Anzahl Dichten und Anzahl Evidenzgr\[ODoubleDot]\[SZ]en
Print["L\[ADoubleDot]nge dataFile ",Length[dataFile]];
Print["Eintrag 1 ", dataFile[[1]]];
*)

(*{{1,2},{3,4}}[[All,1]] outputs {1,3}
{{1,2},{3,4}}[[All,2]] outputs {2,4}*)
numDensities=Length[dataFile[[All,1]]];
numEvSizes=Length[dataFile[[All,2]][[1]]];

Print["Anzahl Dichten ", numDensities];
Print["Anzahl Evidenzgr\[ODoubleDot]\[SZ]en ", numEvSizes];

If[docm!="doj", 
evSizeToDel=1; dataToDel=Map[{#,2,evSizeToDel}&, Range[numDensities]]; dataFileAuswahl=Delete[dataFile, dataToDel];,
dataFileAuswahl=dataFile;];

Return[dataFileAuswahl];
];



dataAccVariabel[dataName_, dB1_, dB2_, abm_, docm_, entMod_]:=Module[
{dataFileFullName,dataFile,
acc,
dojDL,
dataFileDens045,
dojD045, posEntDFest, hilfsListe},

(*Data_Acc_0.8_0.3_5.txt*)

If[StringTake[dataName,{10}]=="1", acc=1.0;,acc=StringReplace[StringTake[dataName,{10,12}],"_"->"."];];

dataFileFullName=FileNameJoin[{dB1,dataName}];
Print["Einzulesende Datei ", dataFileFullName];
dataFile=Get[dataFileFullName];

(*F\[UDoubleDot]r alle cm auch - z und f - wird min EvSize ber\[UDoubleDot]cksichtigt !
ALT : Dies ist nur f\[UDoubleDot]r doj der Fall, min EvSize wird f\[UDoubleDot]r alle anderen entfernt :
 dataFileAuswahl=delOfEvSizesFunc[dataFile, docm];*)

If[entMod=="Ohne",
hilfsListe=Map[dataD045Evariabel[#]&,dataFile];
Print["Anzahl Evidenzen bez hilfListe ", Length[hilfsListe[[1,2]]]];
(*hilfsListe2=Map[{First[hilfsListe[[All,2,#,1]]],Catenate[hilfsListe[[All,2,#,2]]]}&, Range[Length[hilfsListe[[1,2]]]]];
Print["Anzahl Evidenzen bez hilfListe2 ", Length[hilfsListe2[[1,2]]]];*)
posEntDFest=DeleteDuplicates[Flatten[Map[Catenate[hilfsListe[[All,2,#,2]]]&, Range[Length[hilfsListe[[1,2]]]]],1]];
(*Print["posEntDFest ", posEntDFest];*),
posEntDFest="Mit";
];

dojDL=Map[dataDfestEvariabel[#,acc,abm, docm, posEntDFest]&,dataFile];
exportingFunc[dataName,acc,dojDL,dB2,docm, abm, entMod];
];


Map[dataAccVariabel[#,dataBox1,dataBox2,alphabetaMod, docMod, entailMod]&,dataNListe]
