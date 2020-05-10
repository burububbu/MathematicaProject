(* ::Package:: *)

(* :Title: AngleCircBackend*)
(* :Context: AngleCircBackend`*)
(* :Author: Don't Panic!*)
(* :Summary: package for angle and circumference exercises *)
(* :Copyright: Don't Panic! 2020*)
(* :Package Version:1*)
(* :Mathematica Version:12*)
(* :History:*)
(* :Keywords: *)
(* :Sources:biblio*)
(* :Discussion:*)

f::usage = "ShowAngles[] returns the graphics of the circle with the corrispondent angles."
Begin["Private`"]
f[type_, angle_, choice_] := Module[{circleCenter, alpha, AAngle, circAngle, points, beta, choices},
circleCenter={0,0};
alpha=Which[angle==180,0\[Degree],True,Mod[angle,180]\[Degree]];
AAngle=RandomReal[{0,Pi}];
circAngle=RandomReal[{alpha+0.1,2*Pi-0.1}];
points={CirclePoints[{1,AAngle},1][[1]]};
AppendTo[points,RotationMatrix[alpha].points[[1]]];
beta=180\[Degree]-alpha;

(*TODO Unevaluated*)
choices={ RandomChoice[{-1, 1}] *(alpha/2 + 0.1+RandomReal[{0, beta  - 0.1}]),alpha/2 + beta +RandomReal[{0, alpha}]};
AppendTo[points,RotationMatrix[choices[[choice]]].middlePoint/.middlePoint->RotationMatrix[alpha/2].points[[1]]];
(*drawF[c_]:=Module[{pointsS},
pointsS=Append[points,RotationMatrix[choices[[c]]].middlePoint/.middlePoint\[Rule]RotationMatrix[alpha/2].points[[1]]];
pointsS
]
points;*)
{
Circle[circleCenter, 1] ,
Point[circleCenter],
Point /@ points,

{Opacity[0.3],Style[Triangle[{points[[1]],points[[2]],circleCenter}], Blue]},
{Opacity[0.3],Style[Triangle[points], Red]},

Line[Append[points,  points[[1]]]],(* triangolo con angolo al centro *)
Line[{points[[1]],points[[2]],circleCenter, points[[1]]}], (* triangolo con angolo alla circonferenza *)

Tooltip[Style[Circle[circleCenter, 0.2, {AAngle, AAngle + alpha}], Red, Thick ], alpha],
Tooltip[Style[Circle[points[[3]], 0.2, {angletemp, angletemp+alpha/2}], Blue, Thick ], alpha/2],
(* Annotation[Style[Circle[points[[3]], 0.2, {angletemp, angletemp+alpha/2}], Blue, Thick ], alpha/2, "Mouse"], *)

MapIndexed[ Text[FromCharacterCode[#2[[1]]+ 64] , #1, {1.5,-1.5}]&, points], 
Text["O", circleCenter,{1.5,-1.5}]
}/.angletemp -> PlanarAngle[points[[3]] -> { {points[[3]][[1]] + 1, points[[3]][[2]]}, points[[1]]}]
]
End[]
