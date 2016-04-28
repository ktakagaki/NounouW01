(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Data`", {"HokahokaW`","JLink`","NounouW`"}];


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*NNLoad/NNSave*)


NNLoad::usage="Load data object(s) from file(s).";
Options[NNLoad] = {NNOptFileNameSort -> True};
NNSave::usage="Save data object(s) to a file.";


(* ::Subsection:: *)
(*NNFilenameSort*)


NNFilenameSort::usage="Sorts data filenames based on trailing digits, which may not be straight forward. \
For example, XXX\\CSC2.ncs => XXX\\CSC10.ncs";


(* ::Subsection:: *)
(*NNData Accessors*)


$NNSpanToNNRangeSpecifier::usage="";
NNReadTimepoints::usage="";
NNOptTimepointUnit::usage="";


NNPrintInfo::usage="Prints out java object information for an NNElement child class (calls toStringFull[]).";


NNReadTrace::usage="";

Options[NNReadTrace] = {
	NNOptReturnTimepoints -> True,
	NNOptTimepointUnit -> Automatic
};

NNReadPage::usage="";

Options[NNReadPage] = {
	NNOptReturnTimepoints -> True
};


NNReadEvents::usage="";
NNOptDurationEvents::usage="";

Options[NNReadEvents] = {
	NNOptDurationEvents -> True
};

NNReadTimestamps::usage="";
Options[NNReadTimestamps] = {
	NNOptDurationCheck -> False
};


(* ::Subsection:: *)
(*NNFilter methods*)


NNFilterDownsample::usage="";
NNFilterDecimate::usage="";
NNFilterMedianSubtract::usage="";
NNFilterFIR::usage="";
NNFilterBuffer::usage="";


(* ::Subsection:: *)
(*NNToList*)


(*NNToList::usage="Import data objects into Mathematica List.";*)


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*NNLoad*)


NNLoad[fileName_String, opts:OptionsPattern[]]:=NNLoad[{fileName}, opts];

NNLoad[fileNames:{__String}, opts:OptionsPattern[]]:=
Module[{tempret, optSort},
	optSort = OptionValue[NNOptFileNameSort];
		
	tempret = NN`load[
				If[ optSort, NNFilenameSort[ fileNames ], fileNames]
	];
	If[ Head[tempret]===List && Length[tempret]==1, tempret[[1]], tempret ]
];


NNLoad[args___]:=Message[NNLoad::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*NNSave*)


NNSave[fileName_String, obj_/;NNJavaObjectQ[obj, $NNJavaClass$NNElement], opts:OptionsPattern[]]:=
NNSave[fileName, {obj}, opts];


NNSave[fileName_String, objList_List/;NNJavaObjectListQ[objList, $NNJavaClass$NNElement], opts:OptionsPattern[]]:=
Module[{tempret},
	(*optSort = OptionValue[NNOptFileNameSort];*)
		
	tempret = NN`save[fileName, objList]
];


NNSave[args___]:=Message[NNSave::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*NNFilenameSort*)


NNFilenameSort[fileNames:{__String}]:=
Module[{},
		
		SortBy[fileNames,
			StringCases[ #,
					Shortest[__] ~~ x:NumberString~~"."~~WordCharacter ..  :> ToExpression[x]
			]&
		]

];


NNFilenameSort[args___]:=Message[NNFilenameSort::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*NNPrintInfo*)


NNPrintInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNElement]]:= dataObj@toStringFull[];
NNPrintInfo[dataObj_/;JavaObjectQ[dataObj]]:= dataObj@toString[];

NNPrintInfo[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement], "Timing"]:= dataObj@getTiming[]@toStringFull[];


NNPrintInfo[args___]:=Message[NNPrintInfo::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNData Accessors*)


(* ::Subsubsection:: *)
(*$NNSpanToNNRangeSpecifier*)


(* { start;;last , segment}  *)
$NNSpanToNNRangeSpecifier[ {Span[start_/;NumberQ[start], last_/;NumberQ[last]],  segment_/;NumberQ[segment]} ]:=
	NN`NNRange[start, last, 1, segment];
(* { start;;last;;step , segment} *)
$NNSpanToNNRangeSpecifier[ {Span[start_/;NumberQ[start], last_/;NumberQ[last], step_/;NumberQ[step] ],  segment_/;NumberQ[segment]} ]:= 
	NN`NNRange[start, last, step, segment];

(*  timestamp -> start;;last  *)
$NNSpanToNNRangeSpecifier[ 
	Rule[ timestamp_/;NumberQ[timestamp], 
		  Span[   start_/;NumberQ[start], last_/;NumberQ[last] ]
	]
]:= NN`NNRangeTsEvent[timestamp, start, last, 1];

(*  timestamp -> start;;last;;step  *)
$NNSpanToNNRangeSpecifier[ 
	Rule[ timestamp_/;NumberQ[timestamp], 
		  Span[ start_/;NumberQ[start], last_/;NumberQ[last], step_/;NumberQ[step]]
	]
]:= NN`NNRangeTsEvent[timestamp, start, last, step];

(*  {timestamps} -> start;;last  *)
$NNSpanToNNRangeSpecifier[ 
	Rule[ timestamps_List/;(And@@(NumberQ /@ timestamps)), 
		  Span[   start_/;NumberQ[start], last_/;NumberQ[last]	]
	]
]:= NN`NNRangeTsEvent[#, start, last, 1]& /@ timestamps;

(*  {timestamps} -> start;;last;;step  *)
$NNSpanToNNRangeSpecifier[ 
	Rule[ timestamps_List/;(And@@(NumberQ /@ timestamps)), 
		  Span[   start_/;NumberQ[start], last_/;NumberQ[last], step_/;NumberQ[step] 	]
	]
]:= NN`NNRangeTsEvent[#, start, last, step]& /@ timestamps;


$NNSpanToNNRangeSpecifier[args___]:=Message[$NNSpanToNNRangeSpecifier::invalidArgs2, {args}];
$NNSpanToNNRangeSpecifier::invalidArgs2 = "`1` is not a correctly formatted span specification!";


(* ::Subsubsection:: *)
(*NNReadTimepoints*)


NNReadTimepoints[ range_/;NNJavaObjectQ[range, $NNJavaClass$NNRangeSpecifier],
				  dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNTimingElement],
				  type_String: "Timestamps"
				]:=
Module[{},
	Switch[type,
		"Timestamps", range@readTimepointsTs[ dataObj ],
		"Frames", range@readTimepoints[ dataObj ]
	]
];
		
(*If[ NNJavaObjectQ[range, $NNJavaClass$NNRange] || NNJavaObjectQ[range, $NNJavaClass$NNRangeAll],
	range@readTimepoints[ dataObj ],
	If[ NNJavaObjectQ[range, $NNJavaClass$NNRangeTs] || NNJavaObjectQ[range, $NNJavaClass$NNRangeTsEvent],
		range@readTimepointsTs[ dataObj ],
		Message[NNReadTimepoints::incompatible]
	]
];
*)
NNReadTimepoints[args___]:=Message[NNReadTimepoints::invalidArgs, {args}];
NNReadTimepoints::incompatible="modify NNReadTimepoints to handle this NNRangeSpecifier.";


(* ::Subsubsection:: *)
(*NNReadTrace*)


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel_/;NumberQ[channel], 
			range_Rule, 
			opts:OptionsPattern[]]:= NNReadTrace[dataObj, channel, $NNSpanToNNRangeSpecifier[range], opts];

NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel_/;NumberQ[channel], 
			range_List/;(Head[range[[1]]]===Span), 
			opts:OptionsPattern[]]:= NNReadTrace[dataObj, channel, $NNSpanToNNRangeSpecifier[range], opts];


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel_/;NumberQ[channel], 
			rangeList_List/;NNJavaObjectListQ[ rangeList, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
NNReadTrace[dataObj, channel, #, opts]& /@ rangeList;


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channel_/;NumberQ[channel], 
			range_/;NNJavaObjectQ[ range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
Module[{optTimepoints, optTimepointUnit, tempTimepoints, tempTrace},

	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	optTimepointUnit = OptionValue[ NNOptTimepointUnit ];
	If[optTimepoints===True, 
		If[optTimepointUnit===Automatic, optTimepointUnit="Frames"],
		optTimepoints=False 
	 ];

	If[optTimepoints(* === Null || optTimepoints === None || optTimepoints === False*),
		Transpose[ {NNReadTimepoints[range, dataObj, optTimepointUnit], 
					dataObj@readTrace[Round[channel], range]
		} ],
		dataObj@readTrace[Round[channel], range]
	]
];


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNDataChannel], 
			range_Rule, 
			opts:OptionsPattern[]]:= NNReadTrace[dataObj, $NNSpanToNNRangeSpecifier[range], opts];

NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNDataChannel], 
			range_List/;(Head[range[[1]]]===Span), 
			opts:OptionsPattern[]]:= NNReadTrace[dataObj, $NNSpanToNNRangeSpecifier[range], opts];


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNDataChannel], 
			rangeList_List/;NNJavaObjectListQ[ rangeList, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
NNReadTrace[dataObj, #, opts]& /@ rangeList;


NNReadTrace[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNDataChannel], 
			range_/;NNJavaObjectQ[range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
Module[{optTimepoints, optTimepointUnit, tempTimepoints, tempTrace},

	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	optTimepointUnit = OptionValue[ NNOptTimepointUnit ];
	If[optTimepoints===True, 
		If[optTimepointUnit===Automatic, optTimepointUnit="Frames"],
		optTimepoints=False 
	 ];

	If[optTimepoints(* === Null || optTimepoints === None || optTimepoints === False*),
		Transpose[ {NNReadTimepoints[range, dataObj, optTimepointUnit], 
					dataObj@readTrace[range]
		} ],
		dataObj@readTrace[range]
	]
];


NNReadTrace[args___]:=Message[NNReadTrace::invalidArgs, {args}];


(* ::Subsubsection:: *)
(*NNReadPage*)


NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			range_Rule, 
			opts:OptionsPattern[]]:= NNReadPage[dataObj, channels, $NNSpanToNNRangeSpecifier[range], opts];

NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			range_List/;(Head[range[[1]]===Span]), 
			opts:OptionsPattern[]]:= NNReadPage[dataObj, channels, $NNSpanToNNRangeSpecifier[range], opts];


NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			rangeList_List/;NNJavaObjectListQ[ rangeList, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
NNReadPage[dataObj, channels, #, opts]& /@ rangeList;

NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			range_Rule, 
			opts:OptionsPattern[]]:= NNReadPage[dataObj, channels, $NNSpanToNNRangeSpecifier[range], opts];

NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			range_List/;(Head[range[[1]]===Span]), 
			opts:OptionsPattern[]]:= NNReadPage[dataObj, channels, $NNSpanToNNRangeSpecifier[range], opts];
NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			channels_List/;And@@(NumberQ/@channels), 
			range_/;NNJavaObjectQ[ range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
Module[{optTimepoints, tempTimepoints, tempTrace},

	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	If[optTimepoints===True, optTimepoints="Frames" ];

	If[optTimepoints === Null || optTimepoints === None || optTimepoints === False,
		dataObj@readPage[Round[channels], range],
		Prepend[ dataObj@readPage[Round[channels], range], NNReadTimepoints[range, dataObj, optTimepoints] ]
	]
];


NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			rangeList_List/;NNJavaObjectListQ[ rangeList, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
NNReadPage[dataObj, #, opts]& /@ rangeList;


NNReadPage[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
			range_/;NNJavaObjectQ[ range, $NNJavaClass$NNRangeSpecifier], 
			opts:OptionsPattern[]]:=
Module[{optTimepoints, tempTimepoints, tempTrace},

	optTimepoints = OptionValue[ NNOptReturnTimepoints ];
	If[optTimepoints===True, optTimepoints="Frames" ];

	If[optTimepoints === Null || optTimepoints === None || optTimepoints === False,
		dataObj@readPage[range],
		Prepend[ dataObj@readPage[range], NNReadTimepoints[range, dataObj, optTimepoints] ]
	]
];


NNReadPage[args___]:=Message[NNReadPage::invalidArgs, {args}];


(* ::Subsection::Closed:: *)
(*NNFilterXXX*)


NNFilterDownsample[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], 
				initialFactor_Integer:16, opts:OptionsPattern[]]:=
Module[{tempret},
	tempret=JavaNew[$NNJavaClass$NNFilterDownsample, dataObj, initialFactor];
	tempret
];

NNFilterDownsample[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				 initialFactor_Integer:16, opts:OptionsPattern[]]:=
	(NNFilterDownsample[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, {dataChannelObj}], 
		initialFactor, opts
	])@getNNDataChannel[ 0 ];

NNFilterDownsample[args___]:=Message[NNFilterDownsample::invalidArgs, {args}];


NNFilterDecimate[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], initialFactor_Integer:16, opts:OptionsPattern[]]:=
Module[{tempret},
	tempret=JavaNew[$NNJavaClass$NNFilterDecimate, dataObj, initialFactor];
	tempret
];

NNFilterDecimate[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				 initialFactor_Integer:16, opts:OptionsPattern[]]:=
	(NNFilterDecimate[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, dataChannelObj], 
		initialFactor, opts
	])@getNNDataChannel[ 0 ];

NNFilterDecimate[args___]:=Message[NNFilterDecimate::invalidArgs, {args}];


NNFilterMedianSubtract[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], initialFactor_Integer:81, opts:OptionsPattern[]]:=
Module[{tempret},
	tempret=JavaNew[$NNJavaClass$NNFilterMedianSubtract, dataObj];
	tempret@setWindowLength[ initialFactor ];
	tempret
];

NNFilterMedianSubtract[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				 initialFactor_Integer:81, opts:OptionsPattern[]]:=
	(NNFilterMedianSubtract[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, dataChannelObj], 
		initialFactor, opts
	])@getNNDataChannel[ 0 ];

NNFilterMedianSubtract[args___]:=Message[NNFilterMedianSubtract::invalidArgs, {args}];


NNFilterFIR[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], {highPass_/;NumberQ[highPass], lowPass_/;NumberQ[lowPass]}, opts:OptionsPattern[]]:=
Module[{tempret},
	tempret=JavaNew[$NNJavaClass$NNFilterFIR, dataObj];
	tempret@setFilterHz[ highPass, lowPass ];
	tempret
];

NNFilterFIR[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				 {highPass_/;NumberQ[highPass], lowPass_/;NumberQ[lowPass]}, opts:OptionsPattern[]]:=
	(NNFilterFIR[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, dataChannelObj], 
		{highPass, lowPass}, opts
	])@getNNDataChannel[ 0 ];

NNFilterFIR[args___]:=Message[NNFilterFIR::invalidArgs, {args}];


NNFilterBuffer[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNData], opts:OptionsPattern[]]:=
Module[{tempret},
	JavaNew[$NNJavaClass$NNFilterBuffer, dataObj]
];

NNFilterBuffer[dataChannelObj_/;NNJavaObjectQ[dataChannelObj, $NNJavaClass$NNDataChannel],
				opts:OptionsPattern[]]:=
	(NNFilterBuffer[ 
		JavaNew[$NNJavaClass$NNDataChannelArray, dataChannelObj], opts
	])@getNNDataChannel[ 0 ];

NNFilterBuffer[args___]:=Message[NNFilterBuffer::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNEvents Accessors*)


NNReadEvents[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNEvents], "Ports", opts:OptionsPattern[]]:=
		dataObj@getPorts[];


NNReadEvents[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNEvents], port_Integer,
			opts:OptionsPattern[]]:=
Module[{tempReturn},
	
	(*If[!OptionValue[NNOptDurationEvents], dataObj@expandDurationEventsToStartAndReset[] ];*)

	tempReturn=Transpose[{Transpose[{
				dataObj@readPortEventArrayTimestamp[port], 
				dataObj@readPortEventArrayDuration[port]}],
				dataObj@readPortEventArrayCodes[port],
				dataObj@readPortEventArrayComments[port]}
	];

	If[!OptionValue[NNOptDurationEvents], 
		tempReturn=If[#[[1,2]]!=0,
			{{{#[[1,1]], 0}, #[[2]], #[[3]]}, {{#[[1,1]]+#[[1,2]], 0}, #[[2]], "END: "<>#[[3]]}},
			{#}]& /@ tempReturn;
		Flatten[tempReturn, 1],
		tempReturn
	]

];

NNReadEvents[args___]:=Message[NNReadEvents::invalidArgs, {args}];


NNReadTimestamps[dataObj_/;NNJavaObjectQ[dataObj, $NNJavaClass$NNEvents], {port_Integer, code_Integer},
	opts:OptionsPattern[]]:=
Module[{tempTimestamps, tempTimestampsChecked, optDurationCheck},

	tempTimestamps = dataObj@readPortCodeEventArray[ port, code ];

	optDurationCheck = OptionValue[NNOptDurationCheck];
	If[ optDurationCheck === False || optDurationCheck === None,
		tempTimestamps[[All, 1]],
		If[ HHFunctionQ[ optDurationCheck ],
			tempTimestampsChecked = Select[ tempTimestamps, optDurationCheck[ #[[2]] ]& ];
			If[ Length[tempTimestamps] != Length[tempTimestampsChecked],
				Message[ NNReadTimestamps::rejectDuration, Length[tempTimestamps]-Length[tempTimestampsChecked] ]
			];
			tempTimestampsChecked[[All, 1]],
			
			Message[NNReadTimestamps::invalidOptionValue, "NNOptDurationCheck", ToString[ optDurationCheck ] ];
			tempTimestamps[[All, 1]]
		]
	]

];

NNReadTimestamps::rejectDuration = "Some timestamps (n=`1`) rejected due to NNOptDurationCheck criteria";
NNReadTimestamps[args___]:=Message[NNReadTimestamps::invalidArgs, {args}];


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
