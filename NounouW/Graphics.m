(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Graphics`", {"HokahokaW`","HokahokaW`Graphics`","JLink`","NounouW`","NounouW`Data`"}]


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*NNDetectorPlot/NNDetectorInsetPlot*)


NNDetectorPlot::usage="Plots detector field.";
NNDetectorInsetPlot::usage="";
NNHexagon::usage="";


NNDetectorText::usage="Option for {NNDetectorPlot}: Whether to plot text";
NNDetectorTextFunction::usage="Option for {NNDetectorPlot}: function to use to plot text, #1 is text, #2 is coordinate.
 Requires NNDetectorText->True";


Options[NNDetectorPlot]=
	HHJoinOptionLists[
		{NNDetectorText->True, NNDetectorTextFunction -> (Text[Style[#1, Medium], #2, {0, 0}]&), ColorFunction -> ColorData["Rainbow"] },
		Options[Graphics]
	];


(* ::Subsection:: *)
(*NNTraceOverviewPlot*)


NNTraceOverviewPlot::usage=
"";


NNTraceOverviewPlot$UniqueOptions = {
	NNTraceOverviewPlotCuttingInterval -> 10*60
};
NNTraceOverviewPlot$OverrideOptions = {
	AspectRatio -> 1/10, PlotStyle->{Opacity[0.5]}, AxesLabel->Automatic,
	PlotRange->Automatic, ImageSize->10*72
};

Options[NNTraceOverviewPlot] = HHJoinOptionLists[ 
	NNTraceOverviewPlot$UniqueOptions, NNTraceOverviewPlot$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Subsection::Closed:: *)
(*NNTracePlot*)


NNTracePlot::usage=
"NNTracePlot provides an easy way to plot traces with correct axes, stimulus marks, etc.
NNTracePlot[ <<JavaObject[nounou.DataReader]>> , channel(s), <<JavaObject[nounou.FrameRange]>>, segment, opts:OptionsPattern[]]";


NNTracePlot$UniqueOptions = {
	(*NNStackLists \[Rule] Automatic,*) NNValueUnit -> Absolute, (*ScaleBars->{None, None}, *)  
	(*NNBaselineCorrection->Mean,*) NNTimeUnit -> "ms"(*, NNMasking->False*)
};
NNTracePlot$OverrideOptions = {
	AspectRatio -> Automatic, PlotStyle->{Opacity[0.75]}, AxesLabel->Automatic,
	PlotRange->Automatic, (*BaseStyle->{FontFamily->"Helvetica"},*) ImageSize->10*72
};

Options[NNTracePlot] = HHJoinOptionLists[ 
	NNTracePlot$UniqueOptions, NNTracePlot$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
(*NNDetectorPlot*)


NNDetectorPlot[layoutObj_/;HHJavaObjectQ[layoutObj, $NNJavaClass$NNLayoutSpatial ], opts:OptionsPattern[]]:=
Module[{tempret, 
		channelCount, channelRadius,
		textFunc},

	channelCount = layoutObj@getChannelCount[];
	channelRadius = layoutObj@getChannelRadius[];
		
	tempret=Table[
		{If[ layoutObj@isMasked[n],
				{Gray, Disk[layoutObj@getChannelCoordinates[n], channelRadius ]}       ],
			{Circle[ layoutObj@getChannelCoordinates[n], channelRadius ]}
		},
		{n, 0, channelCount-1}
	];
	tempret = If[ OptionValue[NNDetectorText], 
		(*append text notations if NNDetectorText\[Rule]True*)
		textFunc = OptionValue[NNDetectorTextFunction];
		Append[tempret, 
			Table[ 
				{Black, textFunc[n, layoutObj@getChannelCoordinates[n]]},
				{n, 0, channelCount-1}
			]
		],
		(*return original if NNDetectorText\[Rule]False*)
		tempret 
	];

	Graphics[tempret, Sequence@@FilterRules[{opts}, Options[Graphics]]]
];


NNDetectorPlot[layoutObj_/;HHJavaObjectQ[layoutObj, $NNJavaClass$NNLayoutSpatial ], data_List, opts:OptionsPattern[]]:=
Module[{channelCount, channelRadius,
		tempGraphic},

	channelCount = layoutObj@getChannelCount[];
	channelRadius = layoutObj@getChannelRadius[];
	tempGraphic=If[channelCount < Length[data],
		Message[NNDetectorPlot::invalidChCount, channelCount, Length[data]];
		{},
		Table[ 
			{If[ layoutObj@isMasked[n],
				{Black, Circle[layoutObj@getChannelCoordinates[n], channelRadius ]},
				{(*EdgeForm[None],*)OptionValue[ColorFunction][data[[n+1]]],NNHexagon[ layoutObj@getChannelCoordinates[n], channelRadius*2/Sqrt[3] ]}
			]},
			{n, 0, channelCount-1}
		]
	];
		
	Graphics[tempGraphic, Sequence@@FilterRules[{opts}, Options[Graphics]]]
];

NNDetectorPlot::invalidChCount="Channel count for layout object (`1`) must be longer than data length (`2`)";


NNDetectorPlot[args___]:=Message[NNDetectorPlot::invalidArgs, {args}];


NNHexagon[{x_, y_}, r_]:= Polygon[ Table[{x,y}+r*{Sin[theta], Cos[theta]},{theta, Pi/3, 7 Pi/3, Pi/3}]];


NNHexagon[args___]:=Message[NNHexagon::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNDetectorInsetPlot*)


NNDetectorInsetPlot[insetList_List,
					layoutObj_/;HHJavaObjectQ[layoutObj, $NNJavaClass$NNLayoutSpatial ], 
					opts:OptionsPattern[]]:=
Module[{tempReturn, 
		channelCount, channelRadius,
		textFunc},

	channelCount = layoutObj@getChannelCount[];
	channelRadius = layoutObj@getChannelRadius[];

	If[ channelCount < Length[insetList],
		Message[NNDetectorInsetPlot::notEnoughChannels, channelCount, Length[insetList]];
		Null,
		
		If[ channelCount > Length[insetList], NNDetectorInsetPlot::insetsNotForAllChannels, channelCount, Length[insetList]];
		tempReturn=
		MapThread[
			If[ layoutObj@isMasked[#1],
					{Gray, Disk[layoutObj@getChannelCoordinates[#1], channelRadius ]},
					{Inset[ #2, layoutObj@getChannelCoordinates[#1], Center, channelRadius*1.75 ]}
			]&,
			{Range[ Length[insetList] ]-1, insetList}
		];
		Graphics[tempReturn, Sequence@@FilterRules[{opts}, Options[Graphics]]]

	]
	
];

NNDetectorInsetPlot::notEnoughChannels="Not enough channels in layout object (`1`) to display all insets (`2`)";
NNDetectorInsetPlot::insetsNotForAllChannels="Not all channels in layout object (getChannelCount = `1`) will have insets (Length = `2`)";


NNDetectorInsetPlot[args___]:=Message[NNDetectorInsetPlot::invalidArgs, {args}];


(* ::Subsection:: *)
(*NNTraceOverviewPlot*)


NNTracePlot[nnData_/;NNJavaObjectQ$NNData[nnData],
			channels:{_Integer ..}, 
			sampleRange_/;NNJavaObjectQ$NNSampleRangeSpecifier[sampleRange], 
			opts:OptionsPattern[]]:= 

Block[{ optCuttingInterval,

		tempStackAmplitude, tempDataRange, tempMask,
		opNNTimeUnitMS, opNNAbsoluteValue,  opMasking },

	(*==========Handle options==========*)
	optCuttingInterval = OptionValue[ NNTraceOverviewPlotCuttingInterval ];

	(*==========Read timestamps==========*)
	tempStart =
	tempEnd

	(*==========Data==========*)
	If[ optValueAbsolute,
		nnData@readTrace[channels, sampleRange],
		nnData@readTraceAbs[channels, sampleRange]
	];
    
(*	(*==========Data stacking==========*)
	tempStackAmplitude = 150;(*(Max[traces]-Min[traces];*)
*)
	(*==========Handle graphing options==========*)
	optAspectRatio = OptionValue[AspectRatio];
	If[ optAspectRatio === Automatic, optAspectRatio = (Length[channels]+1)*10];
	optAxesLabels = OptionValue[AxesLabel];
	If[ optAxesLabels === Automatic, 
		optAxesLabels = {optTimeUnit, If[optValueAbsolute, "bits", "\[Mu]v"]}];

	(*==========Plot==========*)
	ListLinePlot[ NNStackLists[traces, tempStackAmplitude, NNBaselineCorrection-> None],
			Sequence@@NNJoinOptionLists[ ListLinePlot,
				{  AxesLabel->{tempTimeUnit, tempDataUnit}, DataRange->tempDataRange, AspectRatio->opAspectRatio,
					PlotRange->{tempDataRange, {0, tempStackAmplitude*Length[channels]}}
				 },
				NNTracePlot$UniqueOptions
			]
	]
  
];


(* ::Subsection:: *)
(*NNTracePlot*)


(*NNTracePlot[ channels:{_Integer ..}, x___ ]:= NNTracePlot[ NounouM2`$NNReader@data[], channels, x];
NNTracePlot[ channel_Integer, x___ ]:= NNTracePlot[ NounouM2`$NNReader@data[], channel, x];
NNTracePlot[dataReader_/;NNDataReaderJavaObjectQ[dataReader], x___] := NNTracePlot[dataReader@data[], x]; 
NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channel_Integer , times_Span, opts:OptionsPattern[]]:= 
							NNTracePlot[xData, {channel}, times, 0, opts];
NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channel_Integer , times_Span, segment_/;NumberQ[segment], opts:OptionsPattern[]]:= 
							NNTracePlot[xData, {channel}, times, segment, opts];
NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channels:{_Integer ..}, span_Span, opts:OptionsPattern[]]:= 
							NNTracePlot[xData, channels, span, 0, opts];*)


(*Open up one-element lists*)
NNTracePlot[{nnData_/;NNJavaObjectQ$NNData[xData]}, rest___]:= NNTracePlot[nnData, rest];


NNTracePlot[nnData_/;NNJavaObjectQ$NNData[xData], 
			All, 
			sampleRange_/;NNJavaObjectQ$NNSampleRangeSpecifier[sampleRange], 
			opts:OptionsPattern[]
]:= 
	NNTracePlot[nnData, Range[0, nnData@getChannelCount-1], sampleRange, opts];


(*NNTracePlot$UniqueOptions = {
	NNStackLists \[Rule] Automatic, NNValueUnit \[Rule] Absolute, (*ScaleBars->{None, None}, *)  
	NNBaselineCorrection->Mean, NNTimeUnit \[Rule] "ms"(*, NNMasking->False*)
};
NNTracePlot$OverrideOptions = {
	AspectRatio \[Rule] Automatic, PlotStyle->{Opacity[0.75]}, 
	PlotRange->Automatic, (*BaseStyle->{FontFamily->"Helvetica"},*) ImageSize->10*72
};*)


NNTracePlot[nnData_/;NNJavaObjectQ$NNData[nnData],
			channels:{_Integer ..}, 
			sampleRange_/;NNJavaObjectQ$NNSampleRangeSpecifier[sampleRange], 
			opts:OptionsPattern[]]:= 

Block[{ optValueAbsolute, optTimeUnit, 
			optAspectRatio, optAxesLabels,
tempTracesWidth, 
		tempStackAmplitude, tempDataRange, tempMask,
		opNNTimeUnitMS, opNNAbsoluteValue,  opMasking },

	(*==========Handle unit options==========*)
	optValueAbsolute = Switch[ OptionValue[ NNValueUnit ],
		Absolute, True,
		x_String/;MemberQ[ {"absolute"}, ToLowerCase[x]], True,
		x_String/;MemberQ[ {"microv","\[Mu]v"}, ToLowerCase[x]], False,
		x_, Message[NNTracePlot::invalidOptionValue, "NNValueUnit", ToString[x]]; True
	];	
	optTimeUnit = Switch[ OptionValue[ NNTimeUnit ],
		Automatic, "ms",
		x_String/;MemberQ[ {"ms"}, ToLowerCase[x] ], "ms",
		x_String/;MemberQ[ {"timestamp", "timestamps", "ts"}, ToLowerCase[x] ], "timestamps",
		x_String/;MemberQ[ {"sample", "samples", "frame", "frames"}, ToLowerCase[x] ], "frames",
		x_, Message[NNTracePlot::invalidOptionValue, "NNTimeUnit", ToString[x]]; "ms"
	];	

	(*==========Data==========*)
	If[ optValueAbsolute,
		nnData@readTrace[channels, sampleRange],
		nnData@readTraceAbs[channels, sampleRange]
	];
    
(*	(*==========Data stacking==========*)
	tempStackAmplitude = 150;(*(Max[traces]-Min[traces];*)
*)
	(*==========Handle graphing options==========*)
	optAspectRatio = OptionValue[AspectRatio];
	If[ optAspectRatio === Automatic, optAspectRatio = (Length[channels]+1)*10];
	optAxesLabels = OptionValue[AxesLabel];
	If[ optAxesLabels === Automatic, 
		optAxesLabels = {optTimeUnit, If[optValueAbsolute, "bits", "\[Mu]v"]}];

	(*==========Plot==========*)
	ListLinePlot[ NNStackLists[traces, tempStackAmplitude, NNBaselineCorrection-> None],
			Sequence@@NNJoinOptionLists[ ListLinePlot,
				{  AxesLabel->{tempTimeUnit, tempDataUnit}, DataRange->tempDataRange, AspectRatio->opAspectRatio,
					PlotRange->{tempDataRange, {0, tempStackAmplitude*Length[channels]}}
				 },
				NNTracePlot$UniqueOptions
			]
	]
  
];


NNTracePlot[args___]:=Message[NNTracePlot::invalidArgs,{args}];


(*	(*==========Create mask graphics==========*)
	grMask = If[ Length[tempMask]==0,
		Graphics[
					Flatten[Join[{Opacity[0.2, Black]},
					If[opNNTimeUnitMS, 
			             {Rectangle[{(xData@tsToMs[#[[1]]]), 0},
						     		{(xData@tsToMs[#[[2]]]), tempStackAmplitude*Length[channels]}]}& /@ tempMask,
					     {Rectangle[{(xData@tsToFrameSegmentA[#[[1]]])[[1]], 0},
								    {(xData@tsToFrameSegmentA[#[[2]]])[[1]], tempStackAmplitude}]*Length[channels]}& /@ tempMask
					]
					]]
				],
		Graphics[]
	];*)


	(*(*==========Handle masking options==========*)
	opMasking = OptionValue[NNMasking];
	If[ NNXMaskJavaObjectQ[opMasking],
		tempMask = opMasking@getActiveMasksA[frameRange[[1]], frameRange[[2]], segment, xData];
		tempMask = 
			If[Length[Flatten[tempMask]]==0,  
				{},
				If[opNNTimeUnitMS, 
					{xData@tsToMs[#[[1]]], xData@tsToMs[#[[2]]]}& /@ tempMask,
					{(xData@tsToFrameSegmentA[#[[1]]])[[1]], (xData@tsToFrameSegmentA[#[[1]]])[[2]]}& /@ tempMask
				]
			],
		tempMask = {}
	];*)




(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]
