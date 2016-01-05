(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Data`", {"HokahokaW`","JLink`","NounouW`"}]


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*NNLoad*)


NNLoad::usage="Load data object(s) from file(s).";


(* ::Subsection:: *)
(*NNData Accessors*)


NNPrintTiming::usage="Print out timing information for a NNDataObject";


NNReadTrace::usage="";

Options[NNReadTrace] = {
	NNOptReadTraceTimings -> True,
	NNOptReadTraceUnit -> 
};


(* ::Subsection:: *)
(*NNToList*)


(*NNToList::usage="Import data objects into Mathematica List.";*)


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*NNLoad*)


NNLoad[fileName_String]:=NNLoad[{fileName}];
NNLoad[fileNames:{__String}]:=Module[{tempret},
	tempret = NN`load[fileNames];
	If[ Length[tempret]==1, tempret[[1]], tempret ]
];


NNLoad[args___]:=Message[NNLoad::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNData Accessors*)


$NNSpanToRangeSpecifier[range_Span]:=
Module[{tempCh, tempRan},
	
];

$NNSpanToRangeSpecifier[args___]:=Message[$NNSpanToRangeSpecifier::invalidArgs, {args}];


(* ::Subsubsection:: *)
(*NNPrintTiming*)


NNPrintTiming[dataObj_/;NNJavaObjectQ$NNData[dataObj]]:=
Module[{tempCh, tempRan},
	dataObj@getTiming[]@toStringFull[]
];

NNPrintTiming[args___]:=Message[NNPrintTiming::invalidArgs, {args}];


(* ::Subsubsection:: *)
(*NNReadTrace*)


NNReadTrace[dataObj_/;NNJavaObjectQ$NNData[dataObj], 
			channel_/;NumberQ[channel], 
			range_Span, 
			opts:OptionsPattern[]]:=



NNReadTrace[dataObj_/;NNJavaObjectQ$NNData[dataObj], 
			channel_/;NumberQ[channel], 
			range_/;NNJavaObjectQ$NNSampleRangeSpecifier[range], 
			opts:OptionsPattern[]]:=
Module[{optTimings, tempTimePoints, tempTrace},

	optTimings= OptionValue[ NNReadTraceTimings ];
	If[optTimings =!= True && optTimings =!= False, Message[NNReadTrace::invalidOptionValue, "NNReadTraceTimings", optTimings];
	
	If[optTimings,
		tempTimePoints = range@ dataObj@
	dataObj@readTrace[Round[channel]]
];

NNReadTrace[args___]:=Message[NNReadTrace::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*NNToList*)


(*NNToList[eventObj_/;HHJavaObjectQ[eventObj,$NNEventClass]]:=
Module[{tempret, tempPortEvt},
	tempret=Table[
		tempPortEvt=eventObj@filterByPortA[p];
		{p, #@timestamp[], #@duration[], #@code[], #@comment[]}& /@ tempPortEvt,
		{p,eventObj@ports[]}
	];
	tempret=Flatten[tempret,1];
	Sort[tempret, (#1[[2]] < #2[[2]])&]
];
NNToList[args___]:=Message[NNLoad::invalidArgs, {args}];*)


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]


(* ::Section:: *)
(*Backup*)


(*NNEventSegmentTimestamps::usage="Extracts segment timestamps from event record.";
NNEventSelect::usage="";*)


(*NNERPExtractTS::usage="Extracts ERP traces from a data object.";
NNERPPlotTS::usage="Extracts segments from a data object and plots ERP.";*)


(*NNEventSegmentTimestamps[events_List]:=
NNEventSegmentTimestamps[events]=
Module[{tempEventStarts, tempEventEnds, tempSegs},
	tempEventStarts=Select[events, (#[[1]]==0 && #[[5]]=="Starting Recording")& ];
	tempEventEnds=Select[events, (#[[1]]==0 && #[[5]]=="Ending Recording")& ];
	tempSegs={#[[2]], NNEventSegmentEndTSSelect[#, tempEventEnds]}& /@ tempEventStarts;
	tempSegs=Table[ 
		If[ n >= Length[tempSegs], tempSegs[[n]], {tempSegs[[n, 1]], Min[tempSegs[[n + 1, 1]],tempSegs[[n, 2]]]} ],
		{n, 1, Length[tempSegs]}
	];
	tempSegs
];

NNEventSegmentEndTSSelect[startEvent_, eventEnds_]:=
	SelectFirst[eventEnds, (#[[2]] > startEvent[[2]])&, {Null,Infinity}][[2]];
NNEventSegmentTimestamps[args___]:=Message[NNEventSegmentTimestamps::invalidArgs, {args}];
NNEventSelect[events_List, eventNo_Integer]:=
Module[{tempEST},
	tempEST = NNEventSegmentTimestamps[events];
	If[eventNo<=0 || Length[tempEST]< eventNo,
		Message[NNEventSelect::invalidEventNo,Length[events], eventNo];,
		Select[events, (tempEST[[eventNo, 1]] <= #[[2]] && #[[2]] < tempEST[[eventNo, 2]])&]
	]
];
NNEventSelect::invalidEventNo="The event list only has `1` detected segments, and eventNo `2` is therefore invalid.";
NNEventSelect[args___]:=Message[NNEventSelect::invalidArgs, {args}];*)


(*NNERPExtractTS[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"], channel_Integer, timeStamps_List, {preFrames_, postFrames_, step_:1} ]:=
Module[{tempEvents},
	tempEvents = JavaNew["nounou.data.ranges.RangeTSEvent", #, preFrames, postFrames]& /@ timeStamps;
	(dataObj@readTraceAbsA[channel, #]&) /@ tempEvents
];


NNERPExtractTS[args___]:=Message[NNERPExtractTS::invalidArgs, {args}];*)


(*NNERPPlotTS[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"], channel_Integer, timeStamps_List, {preFrames_, postFrames_, step_:1} ]:=
Module[{tempERP},
	tempERP =  NNERPExtractTS[dataObj, channel, timeStamps, {preFrames, postFrames, step}];
	ListLinePlot[tempERP, PlotRange->All]
];


NNERPPlotTS[args___]:=Message[NNERPPlotTS::invalidArgs, {args}];*)


(* ::Subsection:: *)
(*FILTER RELATED: NNFilterData*)


(*NNFilterData[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"]]:= 
	JavaNew["nounou.data.filters.XDataFilterFIR", dataObj];


NNFilterData[args___]:=Message[NNFilterData::invalidArgs, {args}];*)


(*NNDownsampleData[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"], factor_:10]:= 
	JavaNew["nounou.data.filters.XDataFilterDownsample", dataObj, factor];


NNDownsampleData[args___]:=Message[NNDownsampleData::invalidArgs, {args}];*)


(* ::Subsection:: *)
(*FILTER RELATED: NNFilterData, NNDownsampleData*)


(*NNFilterData::usage="Applies FIR filter to data object and gives resulting filter object.";
NNDownsampleData::usage="Applies Downsample filter to data object and gives resulting filter object.";*)
