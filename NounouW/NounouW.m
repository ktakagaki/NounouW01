(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`", {"HokahokaW`","JLink`"}]


(* ::Section:: *)
(*Declarations*)


(* Memo: Following from HokahokaW *) 
(*
General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `1` -> `2` is invalid.";
General::deprecated="Function is deprecated, use `1` instead.";
General::nullArgument="At least one of the required arguments is null!";
*)


HHPackageMessage["NounouW`"];


(* ::Subsection:: *)
(*Load Static NN Object*)


(*Convenience object for static methods*)
NN=LoadJavaClass["nounou.NN", StaticsVisible->False, AllowShortContext->True];
Print[NN`toString[]];


(* ::Subsection:: *)
(*nounou Java class paths and object checking*)


$NNJavaClass$NNDataLayout = "nounou.elements.layouts.NNDataLayoutSpatial";
$NNJavaClass$NNEvent = "nounou.elements.NNEvents";
$NNJavaClass$NNData = "nounou.elements.data.NNData";

$NNJavaClass$NNSampleRangeSpecifier = "nounou.elements.ranges.NNSampleRangeSpecifier";


NNJavaObjectQ$NNData::usage="Checks whether something is a Java object and an instance of $NNJavaClass$NNData";

NNJavaObjectQ$NNSampleRangeSpecifier::usage=
	"Checks whether something is a Java object and an instance of $NNJavaClass$NNSampleRangeSpecifier";


(*NNFrameRangeJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.FrameRange";

NNXMaskJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XMask";
NNXLayoutJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XLayout";
NNXLayoutNullJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XLayoutNull";
NNXLayoutSquareJavaObjectQ::usage="Checks whether something is a Java object and an instance of nounou.data.XLayoutSquare";*)


(* ::Subsection:: *)
(*Package-wide option keys*)


NNValueUnit::usage="Specifies what units the data output should be in (Absolute, \"microV\")";
NNTimeUnit::usage="Specifies what time units the data output should be in (\"ms\", \"samples\").
For trace reading, only relevant if time stamps are returned (i.e. NNReturnTimestamps -> True).";
NNReturnTimestamps::usage="Whether to return timestamps when reading data (NNTraceRead). True or False.";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*Java object checking*)


NNJavaObjectQ$NNData[ obj_/;(JavaObjectQ[obj] && InstanceOf[obj, $NNJavaClass$NNData ])]:= True ;
NNJavaObjectQ$NNData[ args___]:= False ;


NNJavaObjectQ$NNSampleRangeSpecifier[ obj_/;(JavaObjectQ[obj] && InstanceOf[obj, $NNJavaClass$NNSampleRangeSpecifier ])]:= True ;
NNJavaObjectQ$NNSampleRangeSpecifier[ args___]:= False ;


(* ::Section:: *)
(*End Private*)


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
(*Old File Loading*)


(*NNData=LoadJavaClass["nounou.NNData", StaticsVisible->True, AllowShortContext\[Rule]True];*)
(*NNDataReader=LoadJavaClass["nounou.NNDataReader", StaticsVisible->False, AllowShortContext->True];*)

