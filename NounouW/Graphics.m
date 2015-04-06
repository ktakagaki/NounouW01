(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Graphics`", {"HokahokaW`","JLink`","NounouW`","NounouW`Data`"}]


(* ::Section:: *)
(*Declarations*)


NNDetectorPlot::usage="Plots detector field.";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


NNDetectorPlot[layoutObj_/;HHJavaObjectQ[layoutObj,$NNDataLayoutSpatialClass]]:=
Module[{tempret, tempPortEvt},
	tempret=Table[
		tempPortEvt=eventObj@filterByPortA[p];
		{p, #@timestamp[], #@duration[], #@code[], #@comment[]}& /@ tempPortEvt,
		{p,eventObj@ports[]}
	];
	tempret=Flatten[tempret,1];
	Sort[tempret, (#1[[2]] < #2[[2]])&]
];
NNDetectorPlot[args___]:=Message[NNToList::invalidArgs, {args}];


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]
