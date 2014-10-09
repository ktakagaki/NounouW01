(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Data`", {"HokahokaW`","JLink`","NounouW`"}]


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*NNLoad*)


NNLoad::usage="Load data objects from files.";


(* ::Subsection:: *)
(*FILTER RELATED: NNFilterData, NNDownsampleData*)


NNFilterData::usage="Applies FIR filter to data object and gives resulting filter object.";
NNDownsampleData::usage="Applies Downsample filter to data object and gives resulting filter object.";


(* ::Subsection::Closed:: *)
(*NNToList*)


NNToList::usage="Import data objects into Mathematica List.";


(* ::Subsection::Closed:: *)
(*EVENT RELATED: NNEventSegmentTimestamps, NNEventSelect*)


NNEventSegmentTimestamps::usage="Extracts segment timestamps from event record.";


NNEventSelect::usage="";


(* ::Subsection:: *)
(*TRACE PLOTTING: NNERPExtract, NNERPPlot*)


NNERPExtractTS::usage="Extracts ERP traces from a data object.";
NNERPPlotTS::usage="Extracts segments from a data object and plots ERP.";


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*NNLoad*)


NNLoad[fileName_String]:=NNDataReader`load[fileName];
NNLoad[fileNames:{__String}]:=NNDataReader`load[fileNames];


NNLoad[args___]:=Message[NNLoad::invalidArgs, {args}];


(* ::Subsection:: *)
(*FILTER RELATED: NNFilterData*)


NNFilterData[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"]]:= 
	JavaNew["nounou.data.filters.XDataFilterFIR", dataObj];


NNFilterData[args___]:=Message[NNFilterData::invalidArgs, {args}];


NNDownsampleData[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"], factor_:10]:= 
	JavaNew["nounou.data.filters.XDataFilterDownsample", dataObj, factor];


NNDownsampleData[args___]:=Message[NNDownsampleData::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
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


(* ::Subsection::Closed:: *)
(*EVENT RELATED: NNEventSegmentTimestamps, NNEventSelect*)


NNEventSegmentTimestamps[events_List]:=
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


NNEventSelect[args___]:=Message[NNEventSelect::invalidArgs, {args}];


(* ::Subsection:: *)
(*TRACE PLOTTING: NNERPExtract, NNERPPlot*)


NNERPExtractTS[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"], channel_Integer, timeStamps_List, {preFrames_, postFrames_, step_:1} ]:=
Module[{tempEvents},
	tempEvents = JavaNew["nounou.data.ranges.RangeTSEvent", #, preFrames, postFrames]& /@ timeStamps;
	(dataObj@readTraceAbsA[channel, #]&) /@ tempEvents
];


NNERPExtractTS[args___]:=Message[NNERPExtractTS::invalidArgs, {args}];


NNERPPlotTS[dataObj_/;HHJavaObjectQ[dataObj,"nounou.data.XData"], channel_Integer, timeStamps_List, {preFrames_, postFrames_, step_:1} ]:=
Module[{tempERP},
	tempERP =  NNERPExtractTS[dataObj, channel, timeStamps, {preFrames, postFrames, step}];
	ListLinePlot[tempERP, PlotRange->All]
];


NNERPPlotTS[args___]:=Message[NNERPPlotTS::invalidArgs, {args}];


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]
