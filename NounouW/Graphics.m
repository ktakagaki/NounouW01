(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Graphics`", {"HokahokaW`","HokahokaW`Graphics`","JLink`","NounouW`","NounouW`Data`"}]


(* ::Section:: *)
(*Declarations*)


(* ::Subsection:: *)
(*NNDetectorPlot*)


NNDetectorPlot::usage="Plots detector field.";


NNDetectorText::usage="Option for {NNDetectorPlot}: Whether to plot text";
NNDetectorTextFunction::usage="Option for {NNDetectorPlot}: function to use to plot text, #1 is text, #2 is coordinate.
 Requires NNDetectorText->True";


Options[NNDetectorPlot]=
	HHJoinOptionLists[
		{NNDetectorText->True, NNDetectorTextFunction -> (Text[Style[#1, Medium], #2, {0, 0}]&) },
		Options[Graphics]
	];


(* ::Subsection:: *)
(*NNTracePlot*)


NNTracePlot::usage=
"NNTracePlot provides an easy way to plot traces with correct axes, stimulus marks, etc.
NNTracePlot[ <<JavaObject[nounou.DataReader]>> , channel(s), <<JavaObject[nounou.FrameRange]>>, segment, opts:OptionsPattern[]]";


NNTracePlot$UniqueOptions = {
	NNStackLists -> Automatic, NNValueUnit -> Absolute, (*ScaleBars->{None, None}, *)  
	NNBaselineCorrection->Mean, NNTimeUnit -> "ms"(*, NNMasking->False*)
};
NNTracePlot$OverrideOptions = {
	AspectRatio -> Automatic, PlotStyle->{Opacity[0.75]}, 
	PlotRange->Automatic, (*BaseStyle->{FontFamily->"Helvetica"},*) ImageSize->10*72
};

Options[NNTracePlot] = NNJoinOptionLists[ 
	NNTracePlot$UniqueOptions, NNTracePlot$OverrideOptions,
	Options[ListLinePlot]
];


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection::Closed:: *)
(*NNDetectorPlot*)


NNDetectorPlot[layoutObj_/;HHJavaObjectQ[layoutObj, $NNDataLayoutSpatialClass], opts:OptionsPattern[]]:=
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


NNDetectorPlot[args___]:=Message[NNToList::invalidArgs, {args}];


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


NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channels:{_Integer ..}, span_List, segment_/;NumberQ[segment], 
			opts:OptionsPattern[]]:= 
		NNTracePlot[xData, channels, Span@@( xData@msToFrame[#]& /@ span ), segment, opts];


NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], channels:{_Integer ..}, span_Span, segment_/;NumberQ[segment], 
			opts:OptionsPattern[]]:= 

Block[{tempDataUnit, tempTracesWidth, 
		tempSpan, tempStackAmplitude, tempTimeUnit, tempDataRange, tempMask,
		opNNTimeUnitMS, opNNAbsoluteValue, opAspectRatio, opMasking },

	(*==========Handle span/time options==========*)
	tempSpan = If[Length[span]==2, {span[[1]], span[[2]], 1}, List@@span];
	tempSpan = If[tempSpan[[2]]===All, {tempSpan[[1]], xData@segmentLengths[]@apply[segment -1] , tempSpan[[3]]}, tempSpan];

	opNNTimeUnitMS = OptionValue[NNTimeUnitMS];
	If[opNNTimeUnitMS, 
		tempTimeUnit = "ms";
		tempDataRange = {xData@frameToMs[ tempSpan[[1]] ], xData@frameToMs[ tempSpan[[2]] ]},
		tempTimeUnit = "frames";
		tempDataRange = {tempSpan[[1]], tempSpan[[2]]}
	];

	(*==========Data==========*)
	opNNAbsoluteValue = OptionValue[NNAbsoluteValue];
	tempDataUnit = If[ opNNAbsoluteValue,
		StringReplace[xData@absUnit[], "micro" -> "\[Mu]"],
		"Bits"
	];
    
	(*==========Data stacking==========*)
	tempStackAmplitude = 150;(*(Max[traces]-Min[traces];*)

	(*==========Handle graphing options==========*)
	opAspectRatio = OptionValue[AspectRatio];
	If[ opAspectRatio === Automatic, opAspectRatio = 1/10*(Length[channels]+1) ];

	(*==========Handle masking options==========*)
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
	];

	(*==========Plot==========*)
	NNTracePlotImpl[xData, channels, tempSpan, segment,
				tempStackAmplitude, 
				opNNTimeUnitMS, tempTimeUnit, tempDataUnit,
				tempMask,
				opAspectRatio, opNNAbsoluteValue ]
  
];


NNTracePlotImpl[xData_(*/;NNXDataJavaObjectQ[xData]*), channels_(*:{_Integer ..}*), 
				{frStart_Integer, frEnd_Integer, frStep_Integer}, segment_Integer,
				tempStackAmplitude_,
				opNNTimeUnitMS_, tempTimeUnit_String, tempDataUnit_String,
				tempMask_List,
				opAspectRatio_, opNNAbsoluteValue_ ]:= 
Block[{traces, grMask, tempSpan, tempDataRange},

	(*==========Data==========*)
	traces=  If[ opNNAbsoluteValue,
		xData@readTraceAbsA[#, NN`frameRange[frStart, frEnd, frStep], segment]& /@ channels,
		xData@readTraceAbsA[#, NN`frameRange[frStart, frEnd, frStep], segment]& /@ channels
	];

	(*==========Create mask graphics==========*)
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
	];

	(*==========data range==========*)
	tempDataRange=If[opNNTimeUnitMS, 
		{xData@frameToMs[frStart], xData@frameToMs[frEnd]},
		{frStart, frEnd}
	];

	(*==========Plot==========*)
	Show[
		ListLinePlot[ NNStackLists[traces, tempStackAmplitude, NNBaselineCorrection-> None],
			Sequence@@NNJoinOptionLists[ ListLinePlot,
				{  AxesLabel->{tempTimeUnit, tempDataUnit}, DataRange->tempDataRange, AspectRatio->opAspectRatio,
					PlotRange->{tempDataRange, {0, tempStackAmplitude*Length[channels]}}
				 },
				NNTracePlot$UniqueOptions
			]
		],
		grMask
	]
  
];


NNTracePlotImpl[args___]:=Message[NNTracePlot::invalidArgs,{args}];


(*NNTracePlot[xData_/;NNXDataJavaObjectQ[xData], 
			channels:{_Integer ..}, span_Span, segment_/;NumberQ[segment], opts:OptionsPattern[]]:= 
Block[{traces, tracesWidth,
		tempFullSpan, grOptDataRange, frameSpan, frameSeg1, frameSeg2, opNNUnit, opNNTimeUnitMS, 
		opAspectRatio, tempDataAbsUnit, tempMaskingEpilog },

	opNNTimeUnitMS = OptionValue[NNTimeUnitMS];

	(*==========Handle units and spans... must change DataRange and also frames to extract.==========*)
	(*no All specifications for the span!*)
	If[ ! (And@@NumberQ/@ span), Message[ NNTracePlot::invalidArgs, span ] ];
	If[ opNNTimeUnitMS,
		(*If plotting in ms*)
		frameSpan = 
		If[Length[span]==2, 
			{xData@msToFrame[span[[1]]], xData@msToFrame[span[[2]]], 1},
			If[Length[span]==3, 
				{xData@msToFrame[span[[1]]], xData@msToFrame[span[[2]]], xData@msToFrame[span[[3]]]},
			Message[ NNTracePlot::invalidArgs, span ]
		]],
		(*If plotting in frames*)
		frameSpan = 
		If[Length[span]==2, 
			{span[[1]], span[[2]], 1},
			If[Length[span]==3, 
				span,
			Message[ NNTracePlot::invalidArgs, span ]
		]]
	];
	grOptDataRange = {span[[1]], span[[2]]};

	(*==========Data==========*)
	traces = xData@readTraceAbsA[#, NN`frameRange@@frameSpan, segment]& /@ channels;
    tracesWidth = Max[traces]-Min[traces];

	(*==========Handle graphing options==========*)
	opAspectRatio = OptionValue[AspectRatio];
	If[ opAspectRatio === Automatic, opAspectRatio = 1/10*(Length[channels]+1) ];

	tempDataAbsUnit = xData@absUnit[];
	tempDataAbsUnit = StringReplace[tempDataAbsUnit, "micro" -> "\[Mu]" ];
	
	tempMaskingEpilog = (NounouM2`$NNReader@mask[])@getActiveMasksA[frameSpan[[1]], frameSpan[[2]], segment, xData];
	tempMaskingEpilog = If[Length[Flatten[tempMaskingEpilog]]==0,  
		Graphics[],
		Graphics[
			Flatten[Join[{Opacity[0.2, Black]},
				If[opNNTimeUnitMS, 
			        {Rectangle[{(xData@tsToMs[#[[1]]]), - tracesWidth*5},
								{(xData@tsToMs[#[[2]]]), tracesWidth*30}]}& /@ tempMaskingEpilog,
					{Rectangle[{(xData@tsToFrameSegmentA[#[[1]]])[[1]], - tracesWidth*5},
								{(xData@tsToFrameSegmentA[#[[2]]])[[1]], tracesWidth*30}]}& /@ tempMaskingEpilog
				]
			]]
		]
	];

  (*==========Plot==========*)
  Show[
      NNListLinePlotStack[ traces,
		Sequence@@NNJoinOptionLists[ NNListLinePlotStack,
			{opts}, 
			{NNStackAxes->True, 
			NNBaselineCorrection->OptionValue[NNBaselineCorrection],
			AxesLabel->{opNNUnit, tempDataAbsUnit},
			AspectRatio->opAspectRatio,
			DataRange->grOptDataRange,
		PlotRange->Automatic},
			NNTracePlot$UniqueOptions
		]
      ],
	  tempMaskingEpilog
  ]
  
];*)


NNTracePlot[args___]:=Message[NNTracePlot::invalidArgs,{args}];


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]
