(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`", {"HokahokaW`","HokahokaW`Package`","JLink`"}]


(* ::Section:: *)
(*Declarations*)


(* Exported symbols added here with SymbolName::usage *) 
(*General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `2` -> `1` is invalid.";*)


HHPackageMessage["NounouW`"];


(*Convenience object for static methods*)
NN=LoadJavaClass["nounou.NN", StaticsVisible->False, AllowShortContext->True];
NounouW`$NNData::usage="Main default reader object for NounouW.";
NounouW`$NNData = NN`newNNData[];


$NNDataLayoutSpatialClass = "nounou.elements.layouts.NNDataLayoutSpatial";
$NNEventClass = "nounou.elements.NNEvents";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]


(* ::Section:: *)
(*Bak*)


(*
NNStackLists::usage=" ";
(*NNAxes::usage=" ";
NNBaselineCorrection::usage="";*)

NNTimeUnitMS::usage="True (plot in ms) or False.";
NNMasking::usage=" ";
NNSpikes::usage=" ";
NNAbsoluteValue::usage="Whether to plot traces in an absolute unit";



*)


(*NNReloadPackage[string_String]:=
	Module[{tempPackages},
		tempPackages=Select[ $Packages, StringMatchQ[#, (___ ~~ string ~~ ___)]& ];
		If[Length[tempPackages]>0,
			Unprotect[$Packages];
			($ContextPath=DeleteCases[$ContextPath, #])& /@ tempPackages;
			($Packages=DeleteCases[$Packages, # ])& /@ tempPackages;
			(Remove[Evaluate[#<>"*"]])& /@ tempPackages;
			Needs /@ tempPackages;
			Print["Reloaded: " <> # ]& /@ tempPackages;
		];
		(*Protect[$ContextPath];--$ContextPath is not Protected*)
		Protect[$Packages];
	];
NNReloadPackage[strings_List/;(And @@ StringQ /@ strings)]:= 
	Module[{},
		NNReloadPackage /@ strings;
	];
NNReloadPackage[args___]:=Message[NNReloadPackage::invalidArgs,{args}];*)


(*NNReloadPackage::usage=
"ReloadPackage[string_String].... unloads and reloads any package which \
contains the specified string within its name. \
This function is convenient during package developemnt--the package under development can \
be reloaded to test functionality, without losing any precalculated results in the kernel.";*)


(*NNNextPower[base_, n_]:= Ceiling[Log[base, n]];
NNNextPower[args___]:=Message[NNNextPower::invalidArgs,{args}];*)


(* ::Subsection:: *)
(*DataReader Java Object Handling, transferred to Hokahoka*)


(*NNDataReaderJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.DataReader";

NNRangeFrSpecifierJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.ranges.RangeFrSpecifier";

NNXDataJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XData";
NNXMaskJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XMask";
NNXLayoutJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XLayout";
NNXLayoutNullJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XLayoutNull";
NNXLayoutSquareJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XLayoutSquare";
*)


(*NNDataReaderJavaObjectQ[
	dataReaderJavaObj_/;(JavaObjectQ[dataReaderJavaObj] 
					&& InstanceOf[dataReaderJavaObj, "nounou.DataReader"])
						]:= True ;
NNDataReaderJavaObjectQ[args___]:= False ;
NNRangeFrSpecifierJavaObjectQ[x_/;(JavaObjectQ[x] && InstanceOf[x, "nounou.data.ranges.RangeFrSpecifier"])] := True;
NNRangeFrSpecifierJavaObjectQ[args___] := False;
NNXDataJavaObjectQ[x_/;(JavaObjectQ[x] && InstanceOf[x, "nounou.data.XData"])]:= True;
NNXDataJavaObjectQ[args___]:= False;
NNXMaskJavaObjectQ[x_/;(JavaObjectQ[x] && InstanceOf[x, "nounou.data.XMask"])]:= True;
NNXMaskJavaObjectQ[args___]:= False;
NNXLayoutJavaObjectQ[x_/;(JavaObjectQ[x] && InstanceOf[x, "nounou.data.XLayout"])]:= True;
NNXLayoutJavaObjectQ[args___]:= False;
NNXLayoutNullJavaObjectQ[x_/;(JavaObjectQ[x] && InstanceOf[x, "nounou.data.XLayoutNull"])]:= True;
NNXLayoutNullJavaObjectQ[args___]:= False;
NNXLayoutSquareJavaObjectQ[x_/;(JavaObjectQ[x] && InstanceOf[x, "nounou.data.XLayoutSquare"])]:= True;
NNXLayoutSquareJavaObjectQ[args___]:= False;
*)


(* ::Subsection:: *)
(*Old Package Message*)


(*$PackageDirectoryNounouW = ParentDirectory[DirectoryName[FindFile["NounouW`"]]];
$PackageNewestFileDateNounouW = DateString[Max @@ AbsoluteTime /@ FileDate /@ FileNames[ "*",$PackageDirectoryNounouW,Infinity] ];
$GitCurrentHeadNounouW = Module[{tempretNN},
	SetDirectory[ ParentDirectory[DirectoryName[ FindFile["NounouW`"] ]] ];
	Run["git rev-parse HEAD > GitCurrentHEADHash.txt"];
	tempretNN = Import["GitCurrentHEADHash.txt"];
	ResetDirectory[];
	tempretNN
];*)


(*CellPrint[TextCell[Row[{
Style["NounouW  (http://github.org/ktakagaki/NounouW)", 
    FontWeight -> "Bold", FontVariations -> {"Underline" -> True}], "\n" ,
"    ( current Git HEAD:  "<> HHGitHEADHash["NounouW`"]<>" )\n" <>
"    ( newest file:  "<> HHNewestFileDate["NounouW`"]<>" )"
}],"Text"]];*)


(* ::Subsection:: *)
(*Old File Loading*)


(*NNData=LoadJavaClass["nounou.NNData", StaticsVisible->True, AllowShortContext\[Rule]True];*)
(*NNDataReader=LoadJavaClass["nounou.NNDataReader", StaticsVisible->False, AllowShortContext->True];*)

