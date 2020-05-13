(* ::Package:: *)

(* :Title: Angoli al Centro e alla Circonferenza *)                         
(* :Context: AngleCircBackend`*)
(* :Author: Boccuto, Celozzi, Giacch\[EGrave] *)
(* :Summary: Package per la definizione e la risoluzione della classe di esercizi riguardo angoli al centro e alla circonferenza.
	La classe di esercizi presa in esame riguarda il calcolo dell'area e del perimetro di due triangoli in una circonferenza con
	due vertici in comune. Uno inscritto e l'altro con il terzo vertice in "O". *)
(* :Copyright: Don't Panic! 2020 *)
(* :Package Version: 1.0 *)
(* :Mathematica Version: 12.1 *)
(* :Keywords: cerchio, circonferenza, centro, angolo, angoli, triangolo, triangoli,
				circle, circumference, center, angle, angles, triangle, triangles *)
(* :Sources:
	https://reference.wolfram.com/,
	https://community.wolfram.com/,
	https://mathematica.stackexchange.com/ *)

BeginPackage["AngleCircBackend`"]

circleAngleGraphicsElements::usage = "circleAngleGraphicsElements[type_, angle_, choice_] ritorna una lista di oggetti grafici contenente:
- La circonferenza goniometrica su cui vengono costruiti i triangoli
- Due ToolTips che mostrano le dimensioni dell'angolo al centro e di quello alla circonferenza, rispettivamente
- Il triangolo AOB dove O \[EGrave] il centro della circonferenza e A e B formano un angolo al centro che rispetta i criteri passati come parametri
- Il triangolo ABC inscritto nella circonferenza che insiste sullo stesso arco AB e rispetta i criteri passati come parametri.
Parametri:	
- \"type\" definisce il tipo di angolo che \"angle\" rappresenta: 1\[Rule]angolo al centro, 2\[Rule]angolo alla circonferenza
- \"angle\" rappresenta la grandezza dell'angolo specificato
- \"choice\" specifica se il centro della circonferenza deve essere interno o esterno al triangolo \"ABC\": 1\[Rule]esterno , 2\[Rule]interno.
NOTA: Se i parametri non sono coerenti (es: [1,180,1]) verr\[AGrave] ritornata una lista con un unico elemento che rappresenta un messaggio di testo che specifica l'impossibilit\[AGrave] di seguire le condizioni imposte.
"

Begin["`Private`"]

circleAngleGraphicsElements[type_, angle_, choice_] := Module[{alpha, aAngle, cAngle, points, choices,radiantsAngle},
	radiantsAngle=angle*Degree;
	If[Mod[radiantsAngle, Pi/(type*1.)] == 0 && choice == 1, Return[{Text["Impossibile disegnare."]}]];
	alpha=type*Which[resto == 0, Pi/(type*1.), True, resto]/.resto ->Mod[radiantsAngle,Pi/(type*1.)];
	aAngle=RandomReal[{0,Pi}];

	points = {A, RotationMatrix[alpha].A }/.A->CirclePoints[{1, aAngle}, 1][[1]];
	
	cAngle=Which[
		choice==1 (* esterno *),
		(RandomChoice[{-1, 1}]*RandomReal[{alpha+0.2,-alpha+2*Pi}])/2,
		choice==2 (* interno *),
		RandomReal[{-alpha,alpha}]/2+Pi
	];
	AppendTo[points, RotationMatrix[cAngle].middlePoint/.middlePoint->RotationMatrix[alpha/2].points[[1]]];

	Return[
		{
			Circle[circleCenter, 1] ,
			Point[circleCenter],
			Point/@points,

			{
				Opacity[0.3],
				Blue,
				Triangle[{points[[1]], points[[2]], circleCenter}],
				Red, Triangle[points]
			},

			Line[Append[points,  points[[1]]]], (* triangolo con angolo al centro *)
			Line[{points[[1]], points[[2]], circleCenter, points[[1]]}], (* triangolo con angolo alla circonferenza *)

			Tooltip[{Red, Thick, Circle[circleCenter, 0.2, {aAngle, aAngle + alpha}]}, StringJoin[ToString@NumberForm[alpha/Degree, {3, 2}],"\[Degree]"]],
			Tooltip[{Blue, Thick, Circle[points[[3]], 0.2, {angletemp, angletemp + alpha/2}]}, StringJoin[ToString@NumberForm[alpha/(2.*Degree), {3, 2}],"\[Degree]"]],

			MapIndexed[Text[FromCharacterCode[#2[[1]] + 64] , #1, offset]&, points], 
			Text["O", circleCenter,offset]
		}/.{angletemp -> PlanarAngle[points[[3]] -> {{points[[3]][[1]] + 1, points[[3]][[2]]}, points[[1]]}],circleCenter->{0,0}, offset->{1.5,-1.5}}
	]
]
End[]
EndPackage[]
