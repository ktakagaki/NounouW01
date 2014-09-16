(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Data`", {"HokahokaW`","JLink`","NounouW`"}]


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*NNLoad*)


NNLoad::usage="Load data objects from files.";


(* ::Subsection:: *)
(*NNToList*)


NNToList::usage="Import data objects into Mathematica List.";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*NNLoad*)


NNLoad[fileName_String]:=NNDataReader`load[fileName];
NNLoad[fileNames:{__String}]:=NNDataReader`load[fileNames];


NNLoad[args___]:=Message[NNLoad::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNToList*)


NNToList[eventObj_/;HHJavaObjectQ[eventObj,"nounou.data.XEvents"]]:=
Module[{tempret, tempPortEvt},
	tempret=Table[
		tempPortEvt=eventObj@filterByPortA[p];
		{p, #@timestamp[], #@duration[], #@code[], #@comment[]}& /@ tempPortEvt,
		{p,eventObj@ports[]}
	];
	Flatten[tempret,1]
];
NNToList[args___]:=Message[NNToList::invalidArgs, {args}];


NNToList[args___]:=Message[NNLoad::invalidArgs, {args}];


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]
