(* ::Package:: *)

(* :Title: AngleCircBackend*)
(* :Context: AngleCircBackend`*)
(* :Author: Don't Panic!*)
(* :Summary: package for radiantsAngle and circumference exercises *)
(* :Copyright: Don't Panic! 2020*)
(* :Package Version:1*)
(* :Mathematica Version:12*)
(* :History:*)
(* :Keywords: *)
(* :Sources:biblio*)
(* :Discussion:*)

circleAngleGraphicsElements::usage = "circleAngleGraphicsElements[] "
Begin["Private`"]
cAGE[type_, radiantsAngle_, choice_] := Module[{circleCenter, alpha, AAngle, circAngle, points, beta, choices},
If[And[Mod[radiantsAngle, Pi/(type*1.)]==0,choice==1], Return[{Text["NO"]}]];
circleCenter={0,0};
alpha=type*Which[resto==0, Pi/(type*1.), True, resto]/.resto -> Mod[radiantsAngle, Pi/(type*1.)];
(*alpha=Which[type\[Equal]1, Which[resto\[Equal]0, Pi, True, resto]/.resto \[Rule] Mod[radiantsAngle, Pi], type\[Equal]2, 2*Which[resto==0, Pi/2., True, resto]/.resto -> Mod[radiantsAngle, Pi/2.]];*)
(*alpha=Which[type==1, Which[radiantsAngle>Pi,(2*Pi-radiantsAngle),True,radiantsAngle], type==2, 2*Which[resto==0, Pi/2, True, resto]/.resto -> Mod[radiantsAngle, Pi/2]];*)

AAngle=RandomReal[{0,Pi}];
circAngle=RandomReal[{alpha+0.1,2*Pi-0.1}];
points={CirclePoints[{1,AAngle},1][[1]]};
AppendTo[points,RotationMatrix[alpha].points[[1]]];
beta=Pi-alpha;

(*TODO Unevaluated*)
choices={RandomChoice[{-1, 1}]*(alpha/2 + 0.1 + RandomReal[{0, beta  - 0.1}]), alpha/2 + beta + RandomReal[{0, alpha}]};
AppendTo[points,RotationMatrix[choices[[choice]]].middlePoint/.middlePoint -> RotationMatrix[alpha/2].points[[1]]];

{
Circle[circleCenter, 1] ,
Point[circleCenter],
Point /@ points,

{Opacity[0.3],Style[Triangle[{points[[1]],points[[2]],circleCenter}], Blue]},
{Opacity[0.3],Style[Triangle[points], Red]},

Line[Append[points,  points[[1]]]],(* triangolo con angolo al centro *)
Line[{points[[1]],points[[2]],circleCenter, points[[1]]}], (* triangolo con angolo alla circonferenza *)

Tooltip[Style[Circle[circleCenter, 0.2, {AAngle, AAngle + alpha}], Red, Thick ], ToString[NumberForm[alpha/Degree,{3,2}]]<>"\[Degree]"],
Tooltip[Style[Circle[points[[3]], 0.2, {angletemp, angletemp+alpha/2}], Blue, Thick ], ToString[NumberForm[alpha/(2.*Degree),{3,2}]]<>"\[Degree]"],
(* Annotation[Style[Circle[points[[3]], 0.2, {angletemp, angletemp+alpha/2}], Blue, Thick ], alpha/2, "Mouse"], *)

MapIndexed[ Text[FromCharacterCode[#2[[1]]+ 64] , #1, {1.5,-1.5}]&, points], 
Text["O", circleCenter,{1.5,-1.5}]
}/.angletemp -> PlanarAngle[points[[3]] -> {{points[[3]][[1]] + 1, points[[3]][[2]]}, points[[1]]}]
]

circleAngleGraphicsElements[type_, angle_, choice_] := Module[{radiantsAngle},
radiantsAngle=angle*Degree;
cAGE[type, radiantsAngle, choice]
]

(*circleAngleGraphicsElements[type_, angle_, choice_] := Module[{circleCenter, alpha, AAngle, circAngle, points, beta, choices, radiantsAngle},
circleCenter={0,0};
radiantsAngle=angle*Degree;
alpha=Which[radiantsAngle>Pi,(2*Pi-radiantsAngle),True,radiantsAngle];
AAngle=RandomReal[{0,Pi}];
circAngle=RandomReal[{alpha+0.1,2*Pi-0.1}];
points={CirclePoints[{1,AAngle},1][[1]]};
AppendTo[points,RotationMatrix[alpha].points[[1]]];
beta=Pi-alpha;

(*TODO Unevaluated*)
choices={ RandomChoice[{-1, 1}] *(alpha/2 + 0.1+RandomReal[{0, beta  - 0.1}]),alpha/2 + beta +RandomReal[{0, alpha}]};
AppendTo[points,RotationMatrix[choices[[choice]]].middlePoint/.middlePoint \[Rule] RotationMatrix[alpha/2].points[[1]]];

{
Circle[circleCenter, 1] ,
Point[circleCenter],
Point /@ points,

{Opacity[0.3],Style[Triangle[{points[[1]],points[[2]],circleCenter}], Blue]},
{Opacity[0.3],Style[Triangle[points], Red]},

Line[Append[points,  points[[1]]]],(* triangolo con angolo al centro *)
Line[{points[[1]],points[[2]],circleCenter, points[[1]]}], (* triangolo con angolo alla circonferenza *)

Tooltip[Style[Circle[circleCenter, 0.2, {AAngle, AAngle + alpha}], Red, Thick ], ToString[NumberForm[alpha/Degree,{3,2}]]<>"\[Degree]"],
Tooltip[Style[Circle[points[[3]], 0.2, {angletemp, angletemp+alpha/2}], Blue, Thick ], ToString[NumberForm[alpha/(2.*Degree),{3,2}]]<>"\[Degree]"],
(* Annotation[Style[Circle[points[[3]], 0.2, {angletemp, angletemp+alpha/2}], Blue, Thick ], alpha/2, "Mouse"], *)

MapIndexed[ Text[FromCharacterCode[#2[[1]]+ 64] , #1, {1.5,-1.5}]&, points], 
Text["O", circleCenter,{1.5,-1.5}]
}/.angletemp \[Rule] PlanarAngle[points[[3]] \[Rule] {{points[[3]][[1]] + 1, points[[3]][[2]]}, points[[1]]}]
]*)
End[]
