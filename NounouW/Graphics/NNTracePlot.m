(* ::Package:: *)

(* Mathematica Package *)
BeginPackage["NounouW`Graphics`NNTracePlot`", 
	{"HokahokaW`","HokahokaW`Graphics`","JLink`",
	"NounouW`","NounouW`Data`", "NounouW`Graphics`NNTracePlot`"}
];


(* ::Section:: *)
(*Declarations*)


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