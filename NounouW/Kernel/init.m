(* ::Package:: *)

(* Mathematica Init File *)

Get[ "NounouW`NounouW`"];
NounouW`$JavaStackSize = 6144;


Needs[ "JLink`" ];
Needs[ "HokahokaW`" ];
(*SetOptions[JLink`InstallJava, JVMArguments -> "-Xmx1024m"];
SetOptions[JLink`ReinstallJava, JVMArguments -> "-Xmx1024m"];
ReinstallJava[];*)


NounouW`IncreaseJavaStack[stackSize_Integer]:=
	Module[{tempOptStringI,tempOptStringR,tempReI=False, tempReR=False, 
		tempPrint},
		
		tempPrint=PrintTemporary["Checking Java stack size..."];

    	(*Extract the stack settings for InstallJava*)
		tempOptStringI=OptionValue[JLink`InstallJava, JLink`JVMArguments];
		If[tempOptStringI===None, tempReI=True,
		If[Head[tempOptStringI]===String,
			tempOptStringI=StringCases[tempOptStringI,"-"~~Shortest[__]~~tempns:NumberString..~~"m"->tempns];
			If[Length[tempOptStringI]>=1,
				tempOptStringI=ToExpression[tempOptStringI[[1]]];
				If[tempOptStringI<stackSize,tempReI=True]
			];
		]];

    	(*Extract the stack settings for ReinstallJava*)
		tempOptStringR=OptionValue[JLink`ReinstallJava, JLink`JVMArguments];
		If[tempOptStringR===None, tempReR=True,
		If[Head[tempOptStringR]===String,
			tempOptStringR=StringCases[tempOptStringR,"-"~~Shortest[__]~~tempns:NumberString..~~"m"->tempns];
			If[Length[tempOptStringR]>=1,
				tempOptStringR=ToExpression[tempOptStringR[[1]]];
				If[tempOptStringR<stackSize,tempReR=True]
			];
		]];

		(*Change and ReinstallJava as necessary*)
		If[tempReI,
			SetOptions[JLink`InstallJava, JLink`JVMArguments -> "-Xmx"<>ToString[stackSize]<>"m"]
		];
		If[tempReR,
			SetOptions[JLink`ReinstallJava, JLink`JVMArguments -> "-Xmx"<>ToString[stackSize]<>"m"]
		];
		If[tempReI || tempReR,
			JLink`ReinstallJava[];
			Print["<<Set JLink` java stack size to "<>ToString[stackSize]<>"Mb>>"];
		];

		NotebookDelete[tempPrint];
	]; (*Module for KKMInstallJava*)


NounouW`IncreaseJavaStack[NounouW`$JavaStackSize];


(*Convenience object for static methods*)
NN=LoadJavaClass["nounou.NN", StaticsVisible->False, AllowShortContext->True];
(*NNData=LoadJavaClass["nounou.NNData", StaticsVisible->True, AllowShortContext\[Rule]True];*)
NNDataReader=LoadJavaClass["nounou.NNDataReader", StaticsVisible->False, AllowShortContext->True];
NounouW`$NNData::usage="Main default reader object for NounouW.";
NounouW`$NNData = NN`newNNData[];


(*This allows Mathematica to interact transparently with Java/Scala/breeze complex numbers*)
SetComplexClass["breeze.math.Complex"];


Needs["NounouW`Data`"];
