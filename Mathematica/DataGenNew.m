(* ::Package:: *)

evidenceSizeListe=ToExpression[$ScriptCommandLine[[2]]]; Print["evidenceSizeListe ", evidenceSizeListe];
densityListe=ToExpression[$ScriptCommandLine[[3]]]; Print[densityListe];
accListe=ToExpression[$ScriptCommandLine[[4]]]; Print[accListe];
If[StringContainsQ[$ScriptCommandLine[[5]], ","], ensembleListe=StringSplit[StringDelete[$ScriptCommandLine[[5]]," "], ","],ensembleListe={$ScriptCommandLine[[5]]}];
Print[ensembleListe];
kmax=ToExpression[$ScriptCommandLine[[6]]]; Print[kmax];
lmax=ToExpression[$ScriptCommandLine[[7]]]; Print[lmax];
dataBox1=ToString[$ScriptCommandLine[[8]]]; Print[dataBox1];
dataBox2=ToString[$ScriptCommandLine[[9]]]; Print[dataBox2];
dName=ToString[$ScriptCommandLine[[10]]]; Print[dName];
(* F\[UDoubleDot]r testLauf \[NotEqual] 0 l\[ADoubleDot]uft ein Test*) 
test=ToExpression[$ScriptCommandLine[[11]]]; Print["test ",test];



If[!MemberQ[$Path, FileNameJoin[{dataBox1,"DebateDynamics/GeneralTDSFunctions"}]],AppendTo[$Path, FileNameJoin[{dataBox1,"DebateDynamics/GeneralTDSFunctions"}]]];
 Print["Package path ", FindFile[ "GeneralTDSFunctions`"]];
<<GeneralTDSFunctions`

If[!MemberQ[$Path, FileNameJoin[{dataBox1,"DebateDynamics/SimuTypes"}]], AppendTo[$Path, FileNameJoin[{dataBox1,"DebateDynamics/SimuTypes"}]]];
 Print["Package path ", FindFile[ "SimuSpecification`"]];
<<SimuSpecification`

If[!MemberQ[$Path, FileNameJoin[{dataBox1,"DebateDynamics"}]], AppendTo[$Path, FileNameJoin[{dataBox1,"DebateDynamics"}]]];
 Print["Package path ", FindFile[ "DebateDynamics`"]];
<<DebateDynamics`

If[!MemberQ[$Path, FileNameJoin[{dataBox1,"DebateDynamics/DebateDiagnostics"}]], AppendTo[$Path, FileNameJoin[{dataBox1,"DebateDynamics/DebateDiagnostics"}]]];
 Print["Package path ", FindFile[ "DebateDiagnostics`"]];
<<DebateDiagnostics`


(* ::Text:: *)
(*Hier folgen Hilfsfunktionen f\[UDoubleDot]r Hilfsfunktionen :*)
(*  (i) Transformation von  {True, 2, False, False} zu 1 && !3 && !4 *)
(*  (ii) Berechnung der Wahrheit*)
(*  (iii) Erzeugung eines Evidenzstreams*)
(*  (iv) Berechnung des Begr\[UDoubleDot]ndungsgrades einer Hypothese gegeben eine Reihe von Evidenzen*)


(*Abbildung zwischen {True, 2, False, False} und 1 && !3 && !4 
Doppelte If-Abfrage und DeleteCases[,Null] n\[ODoubleDot]tig, da nicht alle Eintr\[ADoubleDot]ge boolsche Variablen sind *)

Bool2NumFunc[boolList_]:=Module[
{},
And @@ DeleteCases[MapIndexed[If[BooleanQ[#1],If[#1,First[#2],!First[#2]]]&,boolList],Null]
];



(* ::Text:: *)
(*Wahl der Wahrheit*)
(*Die Position, die als Wahrheit bezeichnet wird, wird zuf\[ADoubleDot]llig aus der Menge all jener Positionen gew\[ADoubleDot]hlt, die zu einem bestimmten Zeitpunkt dialektisch koh\[ADoubleDot]rent sind*)


TruthCalc[numberSen_,tauf_]:=Module[
{truth},

truth=RandomChoice[{True,False},numberSen];

While[!SatisfiableQ[tauf&&Bool2NumFunc[truth]],
Clear[truth];
truth=RandomChoice[{True,False},numberSen];
];

Return[truth];
];


TruthCalcList[numberSen_, tauf_, kmax_]:=Module[
{truthListe,
truthListe2},
truthListe=Table[TruthCalc[numberSen,tauf],{i,kmax}];
truthListe2=DeleteDuplicates[truthListe];
(*Print["Anzahl der Wahrheiten ", Length[truthListe2]];*)
Return[truthListe2];
];


(* ::Text:: *)
(*Hier folgen Hilfsfunktionen zur Erzeugung einer Reihe von Evidenzen:*)
(*(i) maximale Evidenz generieren *)
(*(ii) rekursiv defineirte Reie aus maximaler Evidenz generieren *)


(* Maximale Evidenz:
(i)  Wenn um senID\[Rule] truth[[senID]] erg\[ADoubleDot]nzt, dann vollst\[ADoubleDot]ndige, zum Zeitpunkt stepi dialektisch koh\[ADoubleDot]rente Position.
(ii) Format: Liste mit Zuweisungsregeln  {5\[Rule] true, 12\[Rule] false, ...} 
(iii) Jede einzelne WW-Zuweisung entspricht mit einer Wahrscheinlichkeit von accuracy der Wahrheit truth*)

(* MaxEvidenceFunc[truth,accuracy,senID,numberSen,taui]*)
MaxEvidenceFunc[truth_,acc_,senID_,numberSen_,taui_]:=Module[
{mengeOhneH,
zuwOhneH,zuwOhneH1,zuwOhneH2,fracIn,
posOhneH},

fracIn=Round[acc*(numberSen-1.0)];

mengeOhneH=RandomSample[Complement[Range[numberSen],{senID}],numberSen-1];
zuwOhneH1=(#->truth[[#]])&/@mengeOhneH[[;;fracIn]];zuwOhneH2=(#->!truth[[#]])&/@mengeOhneH[[fracIn+1;;]];
zuwOhneH=Catenate[{zuwOhneH1,zuwOhneH2}];

posOhneH=Bool2NumFunc[ReplacePart[Range[numberSen],zuwOhneH]];

(*Pr\[UDoubleDot]fe ob posOhneH zum Zeitpunkt stepi gegeben Tau dialektisch-koh\[ADoubleDot]rent ist.
Falls ja,dann nimm sie als MaxEvidenz. Falls nicht,dann starte erneut. *)

While[!SatisfiableQ[taui&&posOhneH],
Clear[mengeOhneH];
Clear[zuwOhneH];
Clear[zuwOhneH1];
Clear[zuwOhneH2];
Clear[posOhneH];

mengeOhneH=RandomSample[Complement[Range[numberSen],{senID}],numberSen-1];
zuwOhneH1=(#->truth[[#]])&/@mengeOhneH[[;;fracIn]];zuwOhneH2=(#->!truth[[#]])&/@mengeOhneH[[fracIn+1;;]];
zuwOhneH=Catenate[{zuwOhneH1,zuwOhneH2}];

posOhneH=Bool2NumFunc[ReplacePart[Range[numberSen],zuwOhneH]];
];

Return[zuwOhneH];
];


(* EvidenceStream ist eine Liste mit 20 Eintr\[ADoubleDot]gen {{},{12\[Rule]False},{12\[Rule]False,6\[Rule]True},...}
Die einzelnen Eintr\[ADoubleDot]ge sind Teilmengen, die durch Akkumulation auseinender hervorgehen: 

(i) Kein Glied enth\[ADoubleDot]lt "senID\[Rule]..." , also die Wahrhiteswertzuweisung der Hypothese,deren Doj relative zu evidence berechnet werden soll
ii) das erste Glied ist die leere Menge
(iii) ein Glied n+1 entspricht Glied n erweitert um eine weitere Aussage. 
(iv) das letzte Glied entspricht der maximalen EvidenzPosition, d.h. einer Liste von 19 Zuweisungsregeln *)

EvidenceSetsFunc[zuwOhneH_,evSize_, numSen_]:=Module[
{ZuwOhneHSets},

ZuwOhneHSets=NestList[Append[#,RandomChoice[Complement[zuwOhneH,#]]]&,{},numSen-1];

Return[ZuwOhneHSets[[evSize+1]]];
];



EvidenceSetsListFunc[zuwOhneH_, evSize_,numSen_,lmax_]:=Module[
{EvSetsListe, EvSetsListe2},
EvSetsListe=Table[EvidenceSetsFunc[zuwOhneH, evSize,numSen],{i,lmax}];
EvSetsListe2=DeleteDuplicates[EvSetsListe];
(*Print["Anzahl der Evidenzreihen ", Length[EvSetsListe2]];*)
Return[EvSetsListe2];
];


(* ::Text:: *)
(*Hier folgen Hilfsfunktionen, die mit der Berechnung des Begr\[UDoubleDot]ndungsgrades zu tun haben.*)
(*(i)  Berechnung des Begr\[UDoubleDot]ndungsgrades  f\[UDoubleDot]r eine feste Evidenz*)
(*(ii)  Berechnung einer Reihe von Evidenzen und anschlie\[SZ]ende Berechnung des Begr\[UDoubleDot]ndungsgrades  f\[UDoubleDot]r jedes Glied*)


(*Berechnung des bedingten Begr\[UDoubleDot]ndungsgrades einer Hypothese senID zum Zeitpunkt stepi gegeben eine Position E und eine dialektische Struktur tau  *)

DojpCalc[senID_,taui_,numberSen_, wwSenID_]:=Module[
{zuwp,posp,sigma,sigmap,dojp},

zuwp={senID-> wwSenID};
posp=Bool2NumFunc[ReplacePart[Range[numberSen],zuwp ]];
sigma=SatisfiabilityCount[taui, Range[numberSen]];
sigmap=SatisfiabilityCount[taui&&posp, Range[numberSen]];
dojp=sigmap/sigma;

Return[dojp];
];

DojECalc[zuwE_,senID_,taui_,numberSen_, wwSenID_]:=Module[
{posE,sigma,sigmaE, dojE},

posE=Bool2NumFunc[ReplacePart[Range[numberSen],zuwE ]];
sigma=SatisfiabilityCount[taui, Range[numberSen]];
sigmaE=SatisfiabilityCount[taui&&posE, Range[numberSen]];
 dojE= sigmaE/sigma;

Return[ dojE];
];

DojpAndECalc[zuwE_,senID_,taui_,numberSen_, wwSenID_]:=Module[
{zuwEp, posEp,zuwp,sigma,sigmaEp,dojpAndE},

zuwp={senID-> wwSenID};
zuwEp=Catenate[{zuwE,zuwp}];
posEp=Bool2NumFunc[ReplacePart[Range[numberSen],zuwEp ]];

sigma=SatisfiabilityCount[taui, Range[numberSen]];
sigmaEp=SatisfiabilityCount[taui&&posEp, Range[numberSen]];
dojpAndE=sigmaEp/sigma;

Return[dojpAndE];
];


(*F\[UDoubleDot]r eine wahre Hypothese H wird gegeben Tau und Bgk zum Zeitpunkt stepi folgendes bestimmt :
(i) eine maximale EvidenzPosition, d.h. eine partielle Position, die erg\[ADoubleDot]nzt um H vollst\[ADoubleDot]ndig und zum dial koh\[ADoubleDot]rent ist
(ii) aus der maximalen EvidenzPosition einen EvidenzStream
(iii) Berechne den DOJ von H gegeben EvidenzPosition E_i f\[UDoubleDot]r jedes E_i aus dem EvidenzStream:
{{DOJ(1|E_1),...,DOJ(1|E_19)}, ..., {DOJ(20|E_1),...,DOJ(20|E_19)}}*)

DojCalc[truth_,zuwEMaxSets_,accuracy_,senID_,numberSen_, taui_]:=Module[
{wwSenID, 
dojp,i,dojE,dojpAndE,
dojAss},

(*zuwEMax= MaxEvidenceFunc[truth,accuracy,senID,numberSen,taui];
zuwEMaxSets=EvidenceSetsFunc[zuwEMax,numberSen][[evidenceSize+1]];*)

wwSenID= truth[[senID]];

dojp=DojpCalc[senID,taui,numberSen, wwSenID];
dojE=DojECalc[zuwEMaxSets,senID,taui,numberSen, wwSenID];
dojpAndE=DojpAndECalc[zuwEMaxSets,senID,taui,numberSen, wwSenID];

dojAss=<|"DOJp"->dojp, "DOJE"-> dojE, "DOJpAndE"-> dojpAndE|>;
Return[dojAss];
];



(* ::Text:: *)
(*Hier folgt die zentrale Hilfsfunktion f\[UDoubleDot]r die sp\[ADoubleDot]tere Datenverarbeitung. Sie liest die Daten ein und generiert eine Liste von Listen von Begr\[UDoubleDot]ndungsgraden einer Hypothese gegeben eine Reihe von Evidenz.*)


(*Funktion, die ein Liste zur\[UDoubleDot]ckgibt mit Eintr\[ADoubleDot]gen {DOJ_j(H_i|E_1),...,DOJ_j(H_i|E_19)} wobei j=1,...,1000 und i=1,...,20 *)
RobustnessData[dBox1_, ensembleListe_,density_,evidenceSize_,accuracy_,kmax_,lmax_,test_]:=Module[
{
finalDensity=0.45,
robustnessData,
debateData,
numberSen,infDensEvolution,bgkEvolution,tauListEvolution,
debateFiles,debFiles,
stepi,stepf,truthListe,zuwEMax,evidenceSeriesList,
taui,tauf
},

Print["Evidenzgr\[ODoubleDot]\[SZ]e ", evidenceSize];
(*F\[UDoubleDot]r jedes Ensemble: *)
robustnessDataListe=
Map[
Function[ensembleDir,
(*Get a list of all debate files that belong to this ensemble*)
SetDirectory[FileNameJoin[{dBox1,ensembleDir}]];
(*lists files with names matching "*.deb" in any of the directories"Debates" *)
debateFiles=FileNames["*.deb",{"Debates"}];
(*don't do tests over all debateFiles! Rather over debateFiles[[1;;3]]*)
debFiles =If[test != 0, debateFiles[[1;;3]],debateFiles];

(* Liste mit 1000 Eintr\[ADoubleDot]gen der Art {DOJ(1|E_0),DOJ(1|E_5), DOJ(1|E_10),DOJ(1|E_15)} mit E_0={}*)
Map[
Function[debateFile,
(*F\[UDoubleDot]r jede Debatte tau: *)
debateData=ReadDebateData[debateFile];
numberSen=GetNumberSentences[debateData];
bgkEvolution=GetBgkEvolution[debateData];
tauListEvolution=GetTauListEvolution[debateData];
infDensEvolution=First[GetDebateDiagnostics[debateFile,{"INFDENSEVOL"}]];

stepi=Position[N[infDensEvolution],x_/;x>=density,1,1][[1,1]];
stepf=Position[N[infDensEvolution],x_/;x>=finalDensity,1,1][[1,1]];

(*Darstellung der Argumente von tau und bgk zum Zeitpunkt stepi bzw. stepf als boolscher Ausdruck. Argumente sind:
(i) die Listendarstellung der Argumente zum Zeitpunkt stepi bzw. stepf
(ii) die Anzahl boolscher Variablen*)

(*&&BgkAsFormula[bgkEvolution[[stepi]],Range[numberSen]]*)
taui=TauAsFormula[tauListEvolution[[stepi]],Range[numberSen]];
tauf=TauAsFormula[tauListEvolution[[stepf]],Range[numberSen]];

(*F\[UDoubleDot]r jede Aussage h: *)
Map[
Function[hyp,
truthListe=TruthCalcList[numberSen,tauf, kmax];
(*F\[UDoubleDot]r jede Wahrheit truth: *)
Map[
Function[truth,
zuwEMax=MaxEvidenceFunc[truth,accuracy,hyp,numberSen,taui];
evidenceSeriesList=EvidenceSetsListFunc[zuwEMax,evidenceSize, numberSen,lmax];
Map[Function[evidenceSerie,
(*DojCalc[truth_,zuwEMaxSets[[evSize+1]]_,accuracy_,senID_,numberSen_, taui_]*)
DojCalc[truth,evidenceSerie,accuracy,hyp,numberSen,taui]],
evidenceSeriesList]
],
truthListe]],
Range[numberSen]]],
debFiles
]],
ensembleListe];

Return[Flatten[robustnessDataListe]];
];



(* ::Text:: *)
(*Hier folgt eine Funktionen, die die Ergebnisse exportiert*)


exportingFunc[dName_,assRobustdata_,dataB2_]:=Module[
{},
SetDirectory[dataB2];
Put[assRobustdata,dName];
];



DataAccFest[ensembleListe_, dataB1_, densityListe_,evidenceSizeListe_,acc_,kmax_,lmax_,test_]:=Module[
{dojData},
Print["Akuratess ", acc];
dojData=Map[Function[density,
Print["Dichte ", density];
<|
ToString[density]->
Map[Function[evidenceSize,
{evidenceSize,RobustnessData[dataB1,ensembleListe,density,evidenceSize,acc,kmax,lmax,test]}
],evidenceSizeListe]
|>
],densityListe];
(*<|"0.3"\[Rule]{5,"ass5"}|>*)
Return[dojData];
];




DataAccVariabel[ensembleListe_, dataB1_,dataB2_, dName_, densityListe_,evidenceSizeListe_,accListe_,kmax_,lmax_,test_]:=Module[
{finalData},
finalData=Map[
Function[acc,
<|
ToString[acc]->
DataAccFest[ensembleListe,dataB1,densityListe,evidenceSizeListe,acc,kmax,lmax,test]
|>
],
accListe];
(*<|"1.0"\[Rule] <|"0.3"\[Rule]{5,"ass5"}|>|>*)
exportingFunc[dName,finalData,dataB2];
];



(* evidenceSizeListe, densityListe, accListe, ensembleListe, kmax, lmax, dataBox1, dataBox2, dName, test=T *)
DataAccVariabel[ensembleListe,dataBox1,dataBox2,dName, densityListe,evidenceSizeListe,accListe,kmax,lmax,test]
