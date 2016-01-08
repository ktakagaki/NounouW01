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


$NNSpanToNNRangeSpecifier::usage="";
NNReadTimepoints::usage="";


NNPrintInfo::usage="Prints out java object information for an NNElement child class (calls toStringFull[]).";


NNReadTrace::usage="";

Options[NNReadTrace] = {
	NNOptReturnTimepoints -> True
};


NNReadEvents::usage="";

Options[NNReadEvents] = {
};

NNReadPorts::usage="";


(* ::Subsection:: *)
(*NNToList*)


(*NNToList::usage="Import data objects into Mathematica List.";*)


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*NNLoad*)


NNLoad[fileName_String]:=NNLoad[{fileName}];
NNLoad[fileNames:{__String}]:=Module[{tempret},
	tempret = NN`load[fileNames];
	If[ Length[tempret]==1, tempret[[1]], tempret ]
];


NNLoad[args___]:=Message[NNLoad::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNData Accessors*)


(* ::Subsubsection::Closed:: *)
(*RangeSpecifier related*)


$NNSpanToNNRangeSpecifier[ Span[start_/;NumberQ[start], last_/;NumberQ[last]],  segment_/;NumberQ[segment] ]:=
	$NNSpanToNNRangeSpecifier[ Span[start, last, 1], segment ];

$NNSpanToNNRangeSpecifier[ Span[
							start_/;NumberQ[start], 
							last_/;NumberQ[last], 
							step_/;NumberQ[step]
						],  segment_/;NumberQ[segment] ]:= NN`NNRange[start, last, step, segment];

$NNSpanToNNRangeSpecifier[args___]:=Message[$NNSpanToNNRangeSpecifier::invalidArgs, {args}];


NNReadTimepoints[ range_/;NNJavaObjectQ[range, $NNJavaClass$NNRangeSpecifier],
				  dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement]
				]:=
If[ NNJavaObjectQ[range, $NNJavaClass$NNRange] || NNJavaObjectQ[range, $NNJavaClass$NNRangeAll],
	range@readTimepoints[ dataObj ],
	If[ NNJavaObjectQ[range, $NNJavaClass$NNRangeTs],
		range@readTimepointsTs[ dataObj ],
		Message[NNReadTimepoints::incompatible]
	]
];

NNReadTimepoints[args___]:=Message[NNReadTimepoints::invalidArgs, {args}];
NNReadTimepoints::incompatible="modify NNReadTimepoints to handle this NNRangeSpecifier.";


(* ::Subsubsection:: *)
(*NNData filters*)


(* ::Subsubsection:: *)
(*NNPrintInfo*)


NNPrintInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNElement]]:= dataObj@toStringFull[];
NNPrintInfo[dataObj_/;JavaObjectQ[dataObj]]:= dataObj@toString[];

NNPrintInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement], "Timing"]:= dataObj@getTiming[]@toStringFull[];


NNPrintInfo[args___]:=Message[NNPrintInfo::invalidArgs, {args}];


(* ::Subsubsection:: *)
(*NNReadTrace*)


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel_/;NumberQ[channel], {range_Span, segment_/;NumberQ[segment]}, 
			opts:OptionsPattern[]]:=
NNReadTrace[dataObj, channel, $NNSpanToNNRangeSpecifier[range, segment], opts];


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel_/;NumberQ[channel], 
			range_/;NNJavaObjectQ[ range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
Module[{optTimepoints, tempTimepoints, tempTrace},

	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	If[optTimepoints =!= True && optTimepoints =!= False, Message[NNReadTrace::invalidOptionValue, "NNOptReturnTimepoints", NNOptReturnTimepoints]];
	
	If[optTimepoints,
		Transpose[ {NNReadTimepoints[range, dataObj], dataObj@readTrace[Round[channel], range]} ],
		dataObj@readTrace[range]
	]

];

NNReadTrace[args___]:=Message[NNReadTrace::invalidArgs, {args}];


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNDataChannel], 
			{range_Span, segment_/;NumberQ[segment]}, 
			opts:OptionsPattern[]]:=
NNReadTrace[dataObj, $NNSpanToNNRangeSpecifier[range, segment], opts];


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNDataChannel], 
			range_/;NNJavaObjectQ[range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
Module[{optTimepoints, tempTimepoints, tempTrace},

	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	If[optTimepoints =!= True && optTimepoints =!= False, Message[NNReadTrace::invalidOptionValue, "NNOptReturnTimepoints", NNOptReturnTimepoints]];
	
	If[optTimepoints,
		Transpose[ {NNReadTimepoints[range, dataObj], dataObj@readTrace[range]} ],
		dataObj@readTrace[range]
	]

];

NNReadTrace[args___]:=Message[NNReadTrace::invalidArgs, {args}];


(*NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel_/;NumberQ[channel], 
			range_/;NNJavaObjectQ$NNRangeSpecifier[range], 
			opts:OptionsPattern[]]:=
Module[{optTimepoints, tempTimepoints, tempTrace},

	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	If[optTimepoints =!= True && optTimepoints =!= False, Message[NNReadTrace::invalidOptionValue, "NNOptReturnTimepoints", NNOptReturnTimepoints]];
	
	If[optTimepoints,
		Transpose[ {NNReadTimepoints[range], dataObj@readTrace[Round[channel]]} ],
		dataObj@readTrace[Round[channel]]
	]

];

NNReadTrace[args___]:=Message[NNReadTrace::invalidArgs, {args}];*)


(* ::Subsection::Closed:: *)
(*NNEvents Accessors*)


NNReadEvents[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNEvents], "Ports", opts:OptionsPattern[]]:=
		dataObj@getPorts[];

NNReadPorts[args___]:=Message[NNReadPorts::invalidArgs, {args}];


NNReadEvents[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNEvents], port_Integer,
			opts:OptionsPattern[]]:=
Module[{},

	Transpose[{Transpose[{dataObj@readPortTimestampArray[port], dataObj@readPortDurationArray[port]}],
				dataObj@readPortCodeArray[port],
				dataObj@readPortCommentArray[port]}
	]

];

NNReadEvents[args___]:=Message[NNReadEvents::invalidArgs, {args}];


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


End[];


EndPackage[];


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
