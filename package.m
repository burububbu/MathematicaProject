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

CircleAngleGraphicsElements::usage = "circleAngleGraphicsElements[type_, angle_, choice_] ritorna una lista di oggetti grafici contenente:
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

CheckResult::usage = "TODO"


Begin["`Private`"]


solvingSteps;


CircleAngleGraphicsElements[type_, radiantsAngle_, choice_] := Module[{aAngle, cAngle, points, choices, bacAngle, alpha},
	(* Controlla che i parametri siano coerenti: se viene richiesto che il centro della circonferenza sia esterno
    al triangolo inscritto e che l'angolo al centro sia 180\[Degree], allora viene notificata l'impossibilit\[AGrave] di graficarlo *)
	If[Mod[radiantsAngle, Pi/(type*1.)] == 0 && choice == 1, Return[{Text["Impossibile disegnare."]}]];
	
	(* Calcola alpha partendo dai valori di type e angle passati dall'utente.
	alpha corrisponde all'angolo al centro in radianti ed \[EGrave] sempre minore o uguale a Pi.
	Considerando che type \[EGrave] "1" se "angle" rappresenta un angolo al centro e "2" se rappresenta un angolo alla circonferenza,
	si calcola il resto tra l'angolo in radianti ed il massimo valore che l'angolo stesso pu\[OGrave] avere (Pi se type = 1, Pi/2 altrimenti).
	Calcolato il resto, se questo \[EGrave] zero si ritorna il massimo valore dell'angolo, altrimenti il resto.
	Il valore ottenuto viene moltiplicato per "type", in questo modo se "angle" rappresenta l'angolo alla circonferenza, viene moltiplicato per 2,
	altrimenti viene "moltiplicato per 1" (cio\[EGrave] rimane invariato).*)
	alpha=type*Which[resto == 0, Pi/(type*1.), True, resto]/.resto ->Mod[radiantsAngle,Pi/(type*1.)];
	
	(* Viene memorizzato l'angolo che identifica il punto "A".*)
	aAngle=RandomReal[{0,2*Pi}];
	
	(*Calcola i primi due punti.
	Viene utilizzato "CirclePoints" per trovare il primo punto; viene poi costruita la matrice di rotazione ed applicata 
	("." rappresenta l'operazione di prodotto tra matrice e vettore) al punto A al fine di ottenere il punto B
	(che forma con A un angolo di alpha gradi).*)
	points = {A, RotationMatrix[alpha].A }/.A->CirclePoints[{1, aAngle}, 1][[1]];
	
	(*Calcola l'angolo di rotazione da applicare al middlepoint per trovare il punto C coerentemente con la scelta dell'utente.
	Middlepoint \[EGrave] il punto medio dell'arco AB.*)
	cAngle=Which[
		choice==1 (* centro della circonferenza esterno al triangolo ABC *),
		(* formula iniziale: RandomChoice[{-1, 1}]*(alpha/2 + 0.1 + RandomReal[{0, beta - 0.1}]) con beta = Pi-alpha
		la formula sottostante equivale a quella sovrastante semplificata
		*)
		(RandomChoice[{-1, 1}]*RandomReal[{alpha+0.2,-alpha+2*Pi}])/2,
		
		choice==2 (* centro della circonferenza interno al triangolo ABC *),
		(* formula iniziale: alpha/2 + beta + RandomReal[{0, alpha}] con beta = Pi-alpha
		la formula sottostante equivale a quella sovrastante semplificata*)
		RandomReal[{-alpha,alpha}]/2+Pi
	];
	(* Aggiunge il punto C alla lista dei punti.
	Viene calcolato moltiplicando la "RotationMatrix" derivata da cAngle applicata al punto medio (ricavato moltiplicando la "RotationMatrix" derivata da alpha/2 ad A)*)
	AppendTo[points, RotationMatrix[cAngle].middlePoint/.middlePoint->RotationMatrix[alpha/2].points[[1]]];
	
	(*"bacAngle" calcola l'angolo che definisce il punto di partenza per rappresentare graficamente l'angolo BAC*)
	bacAngle= PlanarAngle[points[[1]] -> {points[[2]], points[[3]]}];
	solvingSteps = CalculateValues[alpha, alpha/2, bacAngle];
	Return[
		{
			Circle[circleCenter, 1] ,
			Point[circleCenter],
			(* Trasforma tutti i punti in elementi grafici*)
			Point/@points,
			
			(* Crea due triangoli con opacit\[AGrave] del 30%: uno blu (costruito sui punti ABO (dove O \[EGrave] il centro della circonferenza)), un altro rosso costruito su ABC. *)
			{
				EdgeForm[Thin],
				Opacity[0.3],
				Blue,
				Triangle[{points[[1]], points[[2]], circleCenter}],
				Red, Triangle[points]
			},

			(* Permette di rappresentare graficamente gli angoli al centro e alla circonferenza.
			Aggiunge, inoltre, un tooltip che mostra esplicitamente l'ampienza dell'angolo in gradi quando il mouse viene posizionato sopra.
			  *)
			Tooltip[{Red, Thick, Circle[circleCenter, 0.2, {aAngle, aAngle + alpha}]}, StringJoin[ToString@NumberForm[alpha/Degree, {3, 2}],"\[Degree]"]],
			Tooltip[{Blue, Thick, Circle[points[[3]], 0.2, {cStartAngle, cStartAngle + alpha/2}]}, StringJoin[ToString@NumberForm[alpha/(2.*Degree), {3, 2}],"\[Degree]"]],
			
			{Magenta, Thick, Circle[points[[1]], 0.2, {aStartAngle, aStartAngle + bacAngle}]},
			
			(* Etichetta i punti A, B, C, O visualizzati. *)
			MapIndexed[Text[FromCharacterCode[#2[[1]] + 64] , #1, offset]&, points], 
			Text["O", circleCenter,offset],
			Text[StringJoin[ToString@NumberForm[bacAngle/Degree, {3, 2}], "\[Degree]"],
			points[[1]],
			-offset]
		(* "cStartAngle" calcola l'angolo che definisce il punto di partenza per rappresentare graficamente l'angolo alla circonferenza *)
		}/.{
		cStartAngle -> PlanarAngle[points[[3]] -> {{points[[3]][[1]] + 1, points[[3]][[2]]}, points[[1]]}],
		aStartAngle -> PlanarAngle[points[[1]] -> {{points[[1]][[1]] + 1, points[[1]][[2]]}, points[[2]]}],
		circleCenter -> {0,0}, offset->{-1.5,1.5}}
	]
]


CalculateValues[circleAngle_, circumferenceAngle_, aAngle_] := Module[{toReturn, teoremaDellaCorda, perimetro, area, teoremaDeiSeni, angoloTriangolo, AB,releasedAB,perimetroAOB,areaAOB, BC, releasedBC, ABC, releasedABC, AC, releasedAC, perimetroABC, areaABC},
	(* Contiene liste nel formato "{ ELEMENTO DA CALCOLARE , FORMULA TEORICA , FORMULA APPLICATA " = " RISULTATO }" *)
	(* toReturn = {}; *)
	
	teoremaDellaCorda = HoldForm[2*r*Sin[\[Beta]]];
	perimetro=HoldForm[l1+l2+l3];
	area=HoldForm[(l1*l2*Sin[angolo])/2];
	(*  a:sin(angolo2)=b:sin(angolo1)
		a:sin(Subscript[angoloOpposto, a])=b:sin(Subscript[angoloOpposto, b]); *)
	teoremaDeiSeni=HoldForm[(Sin[angoloOppostoAlLatoDaTrovare]*l1)/Sin[angoloOppostoAL1]];
	angoloTriangolo=HoldForm[Pi-angolo1-angolo2];

	(* calcolo AB sfruttando il teorema della corda: 2*radius*sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	AB = teoremaDellaCorda/.{r->1, \[Beta]->circumferenceAngle};
	releasedAB = ReleaseHold@AB;
	(*AppendTo[toReturn, {"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", "Teorema della Corda:\n"<>Beautify@teoremaDellaCorda, Beautify@AB<>" = "<>ToString@releasedAB}]; *)
	
	(* perimetro AOB *)
	perimetroAOB=perimetro/.{l1->1,l2->1,l3->releasedAB};
	
	(* AppendTo[toReturn, {"Perimetro \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[perimetro/.{l1->"\!\(\*OverscriptBox[\(BO\),
 \(_\)]\)",l2->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Beautify@perimetroAOB<>" = "<>ToString@ReleaseHold@perimetroAOB}]; *)
	
	(* calcolo area = 1/2*OB*AO*sin(alpha) dove alpha \[EGrave] l'angolo al centro *)
	areaAOB=area/.{l1->1,l2->1,angolo->circleAngle};
	(*AppendTo[toReturn, {"Area \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[area/.{l1->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BO\), \(_\)]\)",angolo->"\[Alpha]"}], Beautify@areaAOB<>" = "<>ToString@ReleaseHold@areaAOB}]; *)
	(* --- adesso abbiamo perimetro e area di AOB --- *)

	(* calcolo BC sfruttando il teorema dei seni BC = (sin(BAC)*AB)/sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	BC = teoremaDeiSeni/.{l1->releasedAB, angoloOppostoAlLatoDaTrovare->aAngle, angoloOppostoAL1->circumferenceAngle};
	releasedBC = ReleaseHold@BC;
	(*AppendTo[toReturn, {"\!\(\*OverscriptBox[\(BC\), \(_\)]\)", "Teorema dei Seni:\n"<>Beautify[teoremaDeiSeni/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", angoloOppostoAlLatoDaTrovare->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)", angoloOppostoAL1-> "\[Beta]"}], Beautify@BC<>" = "<>ToString@releasedBC}]; *)
	
	(* calcolo l'angolo ABC = Pi - circumferenceAngle - BAC *)
	ABC=angoloTriangolo/.{angolo1->circumferenceAngle, angolo2->aAngle};
	releasedABC = ReleaseHold@ABC;
	(*AppendTo[toReturn, {"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", Beautify[angoloTriangolo/.{angolo1->"\[Beta]",angolo2->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)"}], Beautify@ABC<>" = "<>ToString@releasedABC}]; *)
	
	(* sfrutto di nuovo il teorema dei seni e calcolo AC = (sin(ABC)*AB)/sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	AC = teoremaDeiSeni/.{l1->releasedAB,angoloOppostoAlLatoDaTrovare->releasedABC,angoloOppostoAL1->circumferenceAngle};
	releasedAC = ReleaseHold@AC;
	(* AppendTo[toReturn, {"\!\(\*OverscriptBox[\(AC\), \(_\)]\)", "Teorema dei Seni:\n"<>Beautify[teoremaDeiSeni/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)",angoloOppostoAlLatoDaTrovare->"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", angoloOppostoAL1-> "\[Beta]" }], Beautify@AC<>" = "<>ToString@releasedAC}];*)
		
	(* perimetro AOB *)
	perimetroABC=perimetro/.{l1->releasedAC,l2->releasedBC,l3->releasedAB};
	(*AppendTo[toReturn, {"Perimetro \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[perimetro/.{l1->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Beautify@perimetroABC<>" = "<>ToString@ReleaseHold@perimetroABC}]; *)
	
	(* calcolo area = 1/2*BC*AC*sin(beta) *)
	areaABC=area/.{l1->releasedBC,l2->releasedAC,angolo->circumferenceAngle};
	(*AppendTo[toReturn, {"Area \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[area/.{l1->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",angolo->"\[Beta]"}], Beautify@areaABC<>" = "<>ToString@ReleaseHold@areaABC}]; *)
	(* --- adesso abbiamo perimetro e area di ABC --- *)
	toReturn = {
	{"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", "Teorema della Corda:\n"<>Beautify@teoremaDellaCorda, Beautify@AB<>" = "<>ToString@releasedAB},
	{"Perimetro \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[perimetro/.{l1->"\!\(\*OverscriptBox[\(BO\),\(_\)]\)",l2->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Beautify@perimetroAOB<>" = "<>ToString@ReleaseHold@perimetroAOB},
	{"Area \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[area/.{l1->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BO\), \(_\)]\)",angolo->"\[Alpha]"}], Beautify@areaAOB<>" = "<>ToString@ReleaseHold@areaAOB},
	{"\!\(\*OverscriptBox[\(BC\), \(_\)]\)", "Teorema dei Seni:\n"<>Beautify[teoremaDeiSeni/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", angoloOppostoAlLatoDaTrovare->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)", angoloOppostoAL1-> "\[Beta]"}], Beautify@BC<>" = "<>ToString@releasedBC},
	{"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", Beautify[angoloTriangolo/.{angolo1->"\[Beta]",angolo2->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)"}], Beautify@ABC<>" = "<>ToString@releasedABC},
	{"\!\(\*OverscriptBox[\(AC\), \(_\)]\)", "Teorema dei Seni:\n"<>Beautify[teoremaDeiSeni/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)",angoloOppostoAlLatoDaTrovare->"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", angoloOppostoAL1-> "\[Beta]" }], Beautify@AC<>" = "<>ToString@releasedAC},
	{"Perimetro \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[perimetro/.{l1->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Beautify@perimetroABC<>" = "<>ToString@ReleaseHold@perimetroABC},
	{"Area \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[area/.{l1->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",angolo->"\[Beta]"}], Beautify@areaABC<>" = "<>ToString@ReleaseHold@areaABC}
	};
	
	Return[toReturn];
]


CheckResult[pAOB_, aAOB_, pACB_ , aACB_]:= Module[{},
	Return[solvingSteps];
]


Beautify[formula_]:=Module[{},
	Return[StringReplace[ToString@TraditionalForm@formula,{"AngleCircBackend`Private`"->""}]]
]


End[]
EndPackage[]
