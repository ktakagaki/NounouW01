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


(* ::Section:: *)
(*Private*)


Begin["`Private`"];


(* ::Subsection:: *)
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


(* ::Section:: *)
(*Ending*)


End[]


EndPackage[]
