(* ::Package:: *)

(* ::Title:: *)
(*NounouW Tests: Load Package*)
(*(150528)*)


CellPrint[TextCell["\""<>DateString[]<>"\"", "Input"]]


(* ::Input:: *)
(*"Thu 28 May 2015 11:53:12"*)


(* ::Input:: *)
(*"Thu 28 May 2015 11:51:52"*)


(* ::Input:: *)
(*"Thu 28 May 2015 11:46:46"*)


<<NounouW`


(* ::Section:: *)
(*Are Main Objects Loaded? (NN, NNDataReader)*)


(* ::Input:: *)
(*(*JavaClassPath[]*)*)


Print["C:\\ProgramData\\Mathematica\\Applications\\NounouW\\Kernel\\init.m"];
FindFile["NounouW`"]


Print[{"NounouW`Graphics`","NounouW`Data`","NounouW`","HokahokaW`Signal`","HokahokaW`Graphics`","HokahokaW`","JLink`","PacletManager`","System`","Global`"}];
$ContextPath


?NounouW`*


?nounou`NN`*


Print["NN = JavaClass[nounou.NN, <>]"];
Definition[NN]


(*Methods[NN]*)


(* ::Section:: *)
(*Check calling syntax*)


NN`toString[]


(* ::Section:: *)
(*Testing*)
