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

grafica::usage = "TODO"

IsValid::usage = "Ritorna TRUE se il valore passato \[EGrave] non-nullo e compreso tra 0 (escluso) e 180, FALSE altrimenti"


Begin["`Private`"]


bacAngle;
(* pAOB, aAOB, pABC, aABC *)
soluzioniEsatte;


grafica:=DynamicModule[{
		angleType,
		alpha=30,
		eventualAlpha=30,
		choice,
		graphic={Circle[{0, 0}, 1]},
		dati={"Clicca \"Disegna\" per visualizzare i dati"},
		panelSoluzione={{"Clicca \"Disegna\" per inserire la soluzione"}},
		steps={},
		risultati={Null,Null,Null,Null},
		soluzioni={Automatic,Automatic,Automatic,Automatic}
	},
	Grid[{
		{ (* Riga 1 *)
			Panel[ (* Cella 1 *)
				Column[{
					Row[{"Tipo di angolo: ",
						RadioButtonBar[
							Dynamic@angleType, {1 -> "Centro", 2 -> "Circonferenza"},
							Appearance-> "Vertical"
						] 
					}, BaseStyle->FontSize -> 18],
					Row[{"Ampiezza angolo: ",
						InputField[
							Dynamic@eventualAlpha, Number, ImageSize->{100, 35},ContinuousAction->True],
						" \[Degree]"
					}, BaseStyle->FontSize -> 18],
					(* Visualizzazione Errore *)
					Row[{Dynamic[If[IsValid[eventualAlpha*angleType], "", Style["L'ampiezza deve essere compresa tra 0 (escluso) e " <> ToString[180 / angleType], Red]]]}, BaseStyle -> FontSize -> 16],
					Row[{"Centro della circonferenza \[EGrave]: ",
						RadioButtonBar[
							Dynamic@choice, {1 -> "Esterno", 2 -> "Interno"},
							Appearance->"Vertical"]
					}, BaseStyle->FontSize -> 18],
					Row[{Button[
						Style["Disegna",FontSize->16],
							If[!IsValid[eventualAlpha],Return[]];
							soluzioni={Automatic,Automatic,Automatic,Automatic};
							alpha=Round[eventualAlpha, 0.01];
							graphic=CircleAngleGraphicsElements[angleType,alpha, choice];
							dati={
								Row[{"r = 1 (r \[EGrave] il raggio)"}, BaseStyle->FontSize -> 18],
								Row[{"\!\(\*OverscriptBox[\(BAC\), \(^\)]\) = "<>ToString@Round[bacAngle,0.01]<>"\[Degree]      ",Dynamic@StringJoin[If[angleType===1,"\!\(\*OverscriptBox[\(AOB\), \(^\)]\)","\!\(\*OverscriptBox[\(ACB\), \(^\)]\)"] ," = ", ToString@alpha,"\[Degree]"]}, BaseStyle->FontSize -> 18]
							};
							panelSoluzione={
								{
									Style["Perimetro \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", FontSize->16],
									InputField[Dynamic[risultati[[1]]], Number, ImageSize->{100,35},ContinuousAction->True,Background->Dynamic@soluzioni[[1]]],
									Style["Area \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", FontSize->16],
									InputField[Dynamic[risultati[[2]]],Number,ImageSize->{100,35},ContinuousAction->True,Background->Dynamic@soluzioni[[2]]]
								},
								{
									Style["Perimetro \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", FontSize->16],
									InputField[Dynamic[risultati[[3]]], Number, ImageSize->{100,35},ContinuousAction->True,Background->Dynamic@soluzioni[[3]]],
									Style["Area \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", FontSize->16],
									InputField[Dynamic[risultati[[4]]],Number,ImageSize->{100,35},ContinuousAction->True,Background->Dynamic@soluzioni[[4]]]
								},
								{
									Button[Style["Conferma",FontSize->18], 
										steps := GetSteps[N[alpha*angleType], N[alpha*angleType/2], bacAngle];
										soluzioni=CheckSoluzioni[risultati],
										Enabled->Dynamic[And[IsValid[alpha*angleType],ContainsNone[Dynamic@risultati,{Null}]]]
									],
									SpanFromLeft
								}
							};
							steps = {},
						Enabled->Dynamic@IsValid[eventualAlpha*angleType]
					]}],
					Row[{"L'ampiezza sar\[AGrave] approssimata ai centesimi"},BaseStyle->{FontSize->16,Darker@Gray}]
				}],
				Style["Inserisci i Parametri Richiesti",FontSize->24],
				Appearance->"Frameless",
				BaseStyle->Large,
				Background-> White
			],
			(* Cella 2 *)
			Panel@Dynamic@Graphics[graphic,ImageSize->350]
			(* Cella 3 *)
			(*Panel@Dynamic@Grid[steps,BaseStyle\[Rule]{FontSize \[Rule] 6},Frame \[Rule] All,  ItemStyle\[Rule] Darker[Blue]]*)
		},
		{(* Riga 2 *)
			(* Cella 1 *)
			Panel[Dynamic@Column[dati],
				Style["Dati",FontSize->24],
				Appearance->"Frameless",
				BaseStyle->Large,
				Background-> LightYellow
			],
			(* Cella 2 *)
			SpanFromAbove
			(*Cella 3*)
			(*SpanFromAbove*)
		},
		{(* Riga 3 *)
			(* Cella 1 *)
			SpanFromAbove,
			(* Cella 2 *)
			Panel[Dynamic@Grid@panelSoluzione,
				Style["Inserisci la Soluzione",FontSize->24],
				Appearance->"Frameless",
				BaseStyle->Large,
				Background-> White
			]
			(*Cella 3*)
			(*SpanFromAbove*)
		},
		{(* Riga 4 *)
			(* Cella 1 *)
			Panel@Dynamic@Grid[steps,BaseStyle->{FontSize -> 18},Frame -> All,  ItemStyle-> Darker[Blue]],
			(* Cella 2 *)
			SpanFromLeft
			(*Cella 3*)
			(*SpanFromAbove*)
		}
		},
		Frame->All,
		Alignment->Top
	]
]


CircleAngleGraphicsElements[type_, radiantsAngle_, choice_] := Module[{aAngle, cAngle, points, choices, alpha},
	(* Controlla che i parametri siano coerenti: se viene richiesto che il centro della circonferenza sia esterno
    al triangolo inscritto e che l'angolo al centro sia 180\[Degree], allora viene notificata l'impossibilit\[AGrave] di graficarlo *)
	If[Mod[radiantsAngle, 180/(type*1.)] == 0 && choice == 1, Return[{Text["Impossibile disegnare."]}]];
	
	(* Calcola alpha partendo dai valori di type e angle passati dall'utente.
	alpha corrisponde all'angolo al centro in radianti ed \[EGrave] sempre minore o uguale a Pi.
	Considerando che type \[EGrave] "1" se "angle" rappresenta un angolo al centro e "2" se rappresenta un angolo alla circonferenza,
	si calcola il resto tra l'angolo in radianti ed il massimo valore che l'angolo stesso pu\[OGrave] avere (Pi se type = 1, Pi/2 altrimenti).
	Calcolato il resto, se questo \[EGrave] zero si ritorna il massimo valore dell'angolo, altrimenti il resto.
	Il valore ottenuto viene moltiplicato per "type", in questo modo se "angle" rappresenta l'angolo alla circonferenza, viene moltiplicato per 2,
	altrimenti viene "moltiplicato per 1" (cio\[EGrave] rimane invariato).*)
	alpha=type*Which[resto == 0, 180/(type*1.), True, resto]/.resto ->Mod[radiantsAngle,180/(type*1.)];
	
	(* Viene memorizzato l'angolo che identifica il punto "A".*)
	aAngle=RandomReal[{0,1*180}];
	
	(*Calcola i primi due punti.
	Viene utilizzato "CirclePoints" per trovare il primo punto; viene poi costruita la matrice di rotazione ed applicata 
	("." rappresenta l'operazione di prodotto tra matrice e vettore) al punto A al fine di ottenere il punto B
	(che forma con A un angolo di alpha gradi).*)
	points = {A, RotationMatrix[alpha Degree].A }/.A->CirclePoints[{1, aAngle Degree}, 1][[1]];
	
	(*Calcola l'angolo di rotazione da applicare al middlepoint per trovare il punto C coerentemente con la scelta dell'utente.
	Middlepoint \[EGrave] il punto medio dell'arco AB.*)
	cAngle=Which[
		choice==1 (* centro della circonferenza esterno al triangolo ABC *),
		(* formula iniziale: RandomChoice[{-1, 1}]*(alpha/2 + 0.1 + RandomReal[{0, beta - 0.1}]) con beta = Pi-alpha
		la formula sottostante equivale a quella sovrastante semplificata
		*)
		(RandomChoice[{-1, 1}]*RandomReal[{alpha+0.1,-alpha+2*180}])/2,
		
		choice==2 (* centro della circonferenza interno al triangolo ABC *),
		(* formula iniziale: alpha/2 + beta + RandomReal[{0, alpha}] con beta = Pi-alpha
		la formula sottostante equivale a quella sovrastante semplificata*)
		RandomReal[{-alpha,alpha}]/2+180
	];
	(* Aggiunge il punto C alla lista dei punti.
	Viene calcolato moltiplicando la "RotationMatrix" derivata da cAngle applicata al punto medio (ricavato moltiplicando la "RotationMatrix" derivata da alpha/2 ad A)*)
	AppendTo[points, RotationMatrix[cAngle Degree].middlePoint/.middlePoint->RotationMatrix[alpha/2 Degree].points[[1]]];
	
	(*"bacAngle" calcola l'angolo che definisce il punto di partenza per rappresentare graficamente l'angolo BAC*)
	bacAngle= Round[PlanarAngle[points[[1]] -> {points[[2]], points[[3]]}]/Degree,0.01];
	(*solvingSteps = CalculateValues[alpha, alpha/2, bacAngle];*)
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
			Tooltip[{EdgeForm[{Thickness@0.005,Red}], FaceForm@RGBColor[0,0,0,0], Disk[circleCenter, 0.2, {aAngle Degree, (aAngle + alpha) Degree}]}, StringJoin[ToString@NumberForm[alpha, {5, 2}],"\[Degree]"]],
			Tooltip[{EdgeForm[{Thickness@0.005,Blue}], FaceForm@RGBColor[0,0,0,0], Disk[points[[3]], 0.2, {cStartAngle Degree, (cStartAngle + alpha/2) Degree}]}, StringJoin[ToString@NumberForm[alpha/2., {5, 2}],"\[Degree]"]],
			Tooltip[{EdgeForm[{Thickness@0.005,Darker[Green]}], FaceForm@RGBColor[0,0,0,0], Disk[points[[1]], 0.15, {aStartAngle Degree, (aStartAngle + bacAngle) Degree}]}, StringJoin[ToString@Round[bacAngle,0.01], "\[Degree]"]],

			
			(* Etichetta i punti A, B, C, O visualizzati. *)
			MapIndexed[Text[FromCharacterCode[#2[[1]] + 64] , #1, offset,BaseStyle->20]&, points], 
			Text["O", circleCenter,offset,BaseStyle->20]
		(* "cStartAngle" calcola l'angolo che definisce il punto di partenza per rappresentare graficamente l'angolo alla circonferenza *)
		}/.{
		cStartAngle -> PlanarAngle[points[[3]] -> {{points[[3]][[1]] + 1, points[[3]][[2]]}, points[[1]]}]/Degree,
		aStartAngle -> PlanarAngle[points[[1]] -> {{points[[1]][[1]] + 1, points[[1]][[2]]}, points[[2]]}]/Degree,
		circleCenter -> {0,0}, offset->{-1.5,1.5}}
	]
]


GetSteps[circleAngle_, circumferenceAngle_, aAngle_] := Module[{toReturn, teoremaDellaCorda, perimetro, area, teoremaDeiSeni, angoloTriangolo, AB,releasedAB,perimetroAOB,areaAOB, BC, releasedBC, ABC, releasedABC, AC, releasedAC, perimetroABC, areaABC},
	teoremaDellaCorda = HoldForm[2*r*Sin[\[Beta]]];
	perimetro=HoldForm[l1+l2+l3];
	area=HoldForm[(l1*l2*Sin[angolo])/2];
	(*  a:sin(angolo2)=b:sin(angolo1)
		a:sin(Subscript[angoloOpposto, a])=b:sin(Subscript[angoloOpposto, b]); *)
	teoremaDeiSeni=HoldForm[(Sin[angoloOppostoAlLatoDaTrovare]*l1)/Sin[angoloOppostoAL1]];
	angoloTriangolo=HoldForm[Pi-angolo1-angolo2];

	(* calcolo AB sfruttando il teorema della corda: 2*radius*sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	AB = teoremaDellaCorda/.{r->1, \[Beta]->circumferenceAngle Degree};
	releasedAB = N@Round[ReleaseHold@AB, 0.01];
	
	(* perimetro AOB *)
	perimetroAOB=perimetro/.{l1->1,l2->1,l3->releasedAB};
	
	(* calcolo area = 1/2*OB*AO*sin(alpha) dove alpha \[EGrave] l'angolo al centro *)
	areaAOB=area/.{l1->1,l2->1,angolo->circleAngle Degree};

	(* --- adesso abbiamo perimetro e area di AOB --- *)

	(* calcolo BC sfruttando il teorema dei seni BC = (sin(BAC)*AB)/sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	BC = teoremaDeiSeni/.{l1->releasedAB*1., angoloOppostoAlLatoDaTrovare->aAngle Degree, angoloOppostoAL1->circumferenceAngle Degree};
	releasedBC = Round[ReleaseHold@BC,0.01];
	
	(* calcolo l'angolo ABC = Pi - circumferenceAngle - BAC *)
	ABC=angoloTriangolo/.{angolo1->circumferenceAngle Degree, angolo2->aAngle Degree};
	releasedABC = Round[ReleaseHold@ABC,0.01];
	
	(* sfrutto di nuovo il teorema dei seni e calcolo AC = (sin(ABC)*AB)/sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	AC = teoremaDeiSeni/.{l1->releasedAB*1.,angoloOppostoAlLatoDaTrovare->releasedABC,angoloOppostoAL1->circumferenceAngle Degree};
	releasedAC = Round[ReleaseHold@AC,0.01];
		
	(* perimetro AOB *)
	perimetroABC=perimetro/.{l1->releasedAC,l2->releasedBC,l3->releasedAB};
	
	(* calcolo area = 1/2*BC*AC*sin(beta) *)
	areaABC=area/.{l1->releasedBC,l2->releasedAC,angolo->circumferenceAngle Degree};

	(* --- adesso abbiamo perimetro e area di ABC --- *)

	(* Forma: Array di Array
		[Step1,Step2,...]
		Stepi=[CosaTrovare, Teorema Utilizzato, Formula Simbolica, FormulaApplicata=Risultato] *)
	soluzioniEsatte={Round[ReleaseHold@perimetroAOB,0.01],Round[ReleaseHold@areaAOB,0.01],Round[ReleaseHold@perimetroABC,0.01],Round[ReleaseHold@areaABC,0.01]};
	Return@{
		{"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", Beautify@teoremaDellaCorda, Beautify@AB<>" = "<>ToString@releasedAB},
		{"Perimetro \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[perimetro/.{l1->"\!\(\*OverscriptBox[\(BO\),\(_\)]\)",l2->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Beautify@perimetroAOB<>" = "<>ToString@soluzioniEsatte[[1]]},
		{"Area \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[area/.{l1->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BO\), \(_\)]\)",angolo->"\[Alpha]"}], Beautify@areaAOB<>" = "<>ToString@soluzioniEsatte[[2]]},
		{"\!\(\*OverscriptBox[\(BC\), \(_\)]\)", Beautify[teoremaDeiSeni/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", angoloOppostoAlLatoDaTrovare->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)", angoloOppostoAL1-> "\[Beta]"}], Beautify@BC<>" = "<>ToString@releasedBC},
		{"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", Beautify[angoloTriangolo/.{angolo1->"\[Beta]",angolo2->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)"}], Beautify@ABC<>" = "<>ToString@releasedABC},
		{"\!\(\*OverscriptBox[\(AC\), \(_\)]\)", Beautify[teoremaDeiSeni/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)",angoloOppostoAlLatoDaTrovare->"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", angoloOppostoAL1-> "\[Beta]" }], Beautify@AC<>" = "<>ToString@releasedAC},
		{"Perimetro \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[perimetro/.{l1->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Beautify@perimetroABC<>" = "<>ToString@soluzioniEsatte[[3]]},
		{"Area \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[area/.{l1->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",angolo->"\[Beta]"}], Beautify@areaABC<>" = "<>ToString@soluzioniEsatte[[4]]}
	}
	(*Return@{{Beautify@BC}}*)
]


CheckSoluzioni[risultati_]:=MapIndexed[Which[#1==soluzioniEsatte[[#2]][[1]], LightGreen, True, LightRed]&, risultati]


IsValid[alpha_]:=And[alpha=!=Null,alpha>0,alpha<=180]


Beautify[formula_]:=StringReplace[ToString[formula//TraditionalForm],{"AngleCircBackend`Private`"->""}]


End[]
EndPackage[]
