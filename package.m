(* ::Package:: *)

(* :Title: Angoli al Centro e alla Circonferenza *)                         
(* :Context: EsercizioTriAngoli`*)
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


BeginPackage["EsercizioTriAngoli`"]

grafica::usage = "circleAngleGraphicsElements[tipoAngolo_, angle_, posizioneCentro_] ritorna una lista di oggetti grafici contenente:
- La circonferenza goniometrica su cui vengono costruiti i triangoli
- Due ToolTips che mostrano le dimensioni dell'angolo al centro e di quello alla circonferenza, rispettivamente
- Il triangolo AOB dove O \[EGrave] il centro della circonferenza e A e B formano un angolo al centro che rispetta i criteri passati come parametri
- Il triangolo ABC inscritto nella circonferenza che insiste sullo stesso arco AB e rispetta i criteri passati come parametri.
Parametri:	
- \"tipoAngolo\" definisce il tipo di angolo che \"angle\" rappresenta: 1\[Rule]angolo al centro, 2\[Rule]angolo alla circonferenza
- \"angle\" rappresenta la grandezza dell'angolo specificato
- \"posizioneCentro\" specifica se il centro della circonferenza deve essere interno o esterno al triangolo \"ABC\": 1\[Rule]esterno , 2\[Rule]interno.
NOTA: Se i parametri non sono coerenti (es: [1,180,1]) verr\[AGrave] ritornata una lista con un unico elemento che rappresenta un messaggio di testo che specifica l'impossibilit\[AGrave] di seguire le condizioni imposte.
"


Begin["`Private`"]


angoloBAC;

(* Risultati corretti del problema.
	Memorizzati in un array: {perimetroAOB, areaAOB, perimetroABC, areaABC} *)
risultatiCorretti;


grafica:=DynamicModule[{
		tipoAngolo,
		alpha=30,
		inputAlpha=30,
		inputTipoAngolo = 1,
		posizioneCentro,
		graficoEsercizio={Circle[{0, 0}, 1]},
		datiProblema={"Clicca \"Disegna\" per visualizzare i dati"},
		formInputRisultati={{"Clicca \"Disegna\" per inserire la soluzione"}},
		passiRisoluzione=Null,
		inputRisultati=Table[Null, 4],
		coloriInputRisultati=Table[Automatic, 4],
		formRisultatiEnabled=False,
		(* formule visualizzate su richiesta dell'utente *)
		formule = {}
	},
	Row[{ (* Contiene 2 "celle": una contente tutto quello che riguarda la preparazione e lo svolgimento dell'esercizio,
			l'altra contente esclusivamente i passi di risoluzione *)
		Grid[{ (* Parentesi che non corrisponde a nulla nell'insieme degli elementi da rappresentare graficamente *)
			{ (* Riga 1 *)
			 (* Cella 1 *)
				Panel[Column[{ (* Panel per creare esercizio *)
					Row[{"Tipo di angolo: ", (* Radiobutton tipo di angolo *)
						RadioButtonBar[
							Dynamic@inputTipoAngolo, {1 -> "Centro", 2 -> "Circonferenza"},
							Appearance-> "Vertical"
						]},
						BaseStyle->FontSize -> 18],
					Row[{"Ampiezza angolo: ", (* Inputfield ampiezza angolo *)
						InputField[Dynamic@inputAlpha, Number, ImageSize->{100, 35},ContinuousAction->True],
						" \[Degree]"},
						BaseStyle->FontSize -> 18],
					Row[{ (* Visualizzazione Errore *)
						Dynamic@If[IsValid[inputAlpha, inputTipoAngolo, posizioneCentro],
							"",
							Style["L'ampiezza deve essere compresa tra 0 (escluso) e " <> ToString[180 / inputTipoAngolo], Red]
						]},
						BaseStyle -> FontSize -> 16],
					Row[{"Centro della circonferenza \[EGrave]: ",  (* Radiobutton posizione centro *)
						RadioButtonBar[
							Dynamic@posizioneCentro, {1 -> "Esterno", 2 -> "Interno"},
							Appearance->"Vertical"]},
						BaseStyle->FontSize -> 18],
					Row@{
						Button[ (* Bottone "disegna" *)
							Style["Disegna",FontSize->16],

							If[!IsValid[inputAlpha, inputTipoAngolo, posizioneCentro], Return[]];
							Clear@risultatiCorretti;
							coloriInputRisultati=Table[Automatic, 4];
							alpha=Round[inputAlpha, 0.01];
							tipoAngolo=inputTipoAngolo;
							graficoEsercizio=ElementiGrafici[tipoAngolo, alpha, posizioneCentro];
							datiProblema= {
								Row[{"r = 1 (r \[EGrave] il raggio)"},
									BaseStyle->FontSize -> 18],
								Row[{"\!\(\*OverscriptBox[\(BAC\), \(^\)]\) = "<>ToString@Round[angoloBAC,0.01]<>"\[Degree]",
									"\t",
									Dynamic@StringJoin[
										"\!\(\*OverscriptBox[\(A",
										Which[tipoAngolo===1, "O", True, "C"],
										"B\), \(^\)]\) = ",
										ToString@alpha,
										"\[Degree]"]},
									BaseStyle->FontSize -> 18]
							};
							formInputRisultati={{
									Style["Perimetro \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", FontSize->16],
									InputField[Dynamic[inputRisultati[[1]]], String, ImageSize->{100,35}, ContinuousAction -> True, Enabled -> Dynamic@And[formRisultatiEnabled, alpha*tipoAngolo < 180], Background->Dynamic@coloriInputRisultati[[1]]],
									Style["Area \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", FontSize->16],
									InputField[Dynamic[inputRisultati[[2]]], String, ImageSize->{100,35}, ContinuousAction -> True, Enabled -> Dynamic@And[formRisultatiEnabled, alpha*tipoAngolo < 180], Background->Dynamic@coloriInputRisultati[[2]]]
								},
								{
									Dynamic[If[Or[inputRisultati[[1]]==="", NumberQ@ToExpression@inputRisultati[[1]]], "", Style["Non \[EGrave] possibile inserire una stringa", Red, FontSize->14]]],
									SpanFromLeft,
									Dynamic[If[Or[inputRisultati[[2]]==="", NumberQ@ToExpression@inputRisultati[[2]]], "", Style["Non \[EGrave] possibile inserire una stringa", Red, FontSize->14]]],
									SpanFromLeft
								},
								{
									Style["Perimetro \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", FontSize->16],
									InputField[Dynamic[inputRisultati[[3]]], String, ImageSize->{100,35}, ContinuousAction -> True, Enabled -> Dynamic@formRisultatiEnabled, Background->Dynamic@coloriInputRisultati[[3]]],
									Style["Area \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", FontSize->16],
									InputField[Dynamic[inputRisultati[[4]]], String, ImageSize->{100,35}, ContinuousAction -> True, Enabled -> Dynamic@formRisultatiEnabled, Background->Dynamic@coloriInputRisultati[[4]]]
								},
								{
									Dynamic[If[Or[inputRisultati[[3]]==="", NumberQ@ToExpression@inputRisultati[[3]]], "", Style["Non \[EGrave] possibile inserire una stringa", Red, FontSize->14]]],
									SpanFromLeft,
									Dynamic[If[Or[inputRisultati[[4]]==="", NumberQ@ToExpression@inputRisultati[[4]]], "", Style["Non \[EGrave] possibile inserire una stringa", Red, FontSize->14]]],
									SpanFromLeft
								},
								{
									Button[Style["Conferma",FontSize->18], 
										
										passiRisoluzione := PassiRisoluzione[N[alpha*tipoAngolo], N[alpha*tipoAngolo/2], angoloBAC];
										coloriInputRisultati:=CheckSoluzioni[ToExpression/@inputRisultati];
										formRisultatiEnabled=False,
										
										Enabled -> Dynamic[And[
											IsValid[alpha, tipoAngolo, posizioneCentro],
											formRisultatiEnabled,
											ContainsNone[inputRisultati[[Which[alpha*tipoAngolo < 180, 1, True, 3] ;; 4]], {""}],
											AllTrue[Select[inputRisultati, # =!= ""&],
											NumberQ[ToExpression[#]]&]]]
									],
									SpanFromLeft
								}
							};
							passiRisoluzione = Null;
							formRisultatiEnabled = True;
							inputRisultati=Table["", 4],

							Enabled->Dynamic@IsValid[inputAlpha, inputTipoAngolo, posizioneCentro]
						]
					}, (* Fine bottone disegna *)
					(* Note *)
					Row[{"L'ampiezza sar\[AGrave] approssimata ai centesimi"},
						BaseStyle->{FontSize->16, Darker@Gray}],
					(*  *)
					Row[{Dynamic@Which[inputAlpha*inputTipoAngolo == 180 && posizioneCentro == 2,
						"NOTA: i dati inseriti permettono la costruzione\ndi un unico triangolo",
						True,
						""]},
						BaseStyle->{FontSize->16, Darker@Blue}],
					(* Messaggio post controllo dei risultati *)
					Row[{Dynamic@Which[ContainsAny[coloriInputRisultati, {LightRed}],
						"Riprova l'esercizio per approfondire le tue competenze",
						ContainsAll[coloriInputRisultati, {LightGreen}],
						"Hai completato l'esercizio con successo",
						True,
						""]},
						BaseStyle->{FontSize->16,
							Dynamic@Which[ContainsAny[coloriInputRisultati, {LightRed}],
								Darker@Red,
								True,
								Darker@Green]}
					]}],
					
					Style["Inserisci i Parametri Richiesti", FontSize->24],
					Appearance->"Frameless",
					BaseStyle->Large,
					Background-> White
				], (* Fine Panel *)
				(* Cella 2 *) (* Grafico esercizio *)
				Panel@Dynamic@Graphics[graficoEsercizio,ImageSize->400]
			},
			{ (* Riga 2 *)
				(* Cella 1 *)
				Column@{ (* La column serve a mettere i due panel (dati e formule) in colonna *)
					Panel[ (* Panel dei dati del problema *)
						Dynamic@Column@datiProblema,
						
						Style["Dati",FontSize->24],
						Appearance->"Frameless",
						BaseStyle->Large,
						Background-> LightYellow
					],
					Panel[ (* Panel Formule da mostrari all'utente *)
						Dynamic@Column[ (* La column serve per mostrare il bottone e le formule una sotto l'altra *)
							(* Flatten ci serve per rendere piatto l'array passato a Column: "formule" \[EGrave] un array di
								Rows (il perch\[EAcute] viene spiegato successivamente) e deve essere ridotto a "lista" di rows:
								es: {button, {Row@"a",Row@"b",Row@"c"}} deve diventare {button, Row@"a",Row@"b",Row@"c"} (senza graffe)
								*)
							Flatten@{
								Button["Mostra/nascondi formule",
									(* Un semplice toggle: se l'array di formule \[EGrave] vuoto, allora lo si riempie,
										altrimenti lo si svuota
										*)
									If[Length@formule == 0,
										(* E' stato necessario utilizzare un array di rows al fine di poter creare style diversi
											all'interno della stessa stringa (in particolare, il titolo \[EGrave] in rosso mentre le
											specificazioni sono nel colore di default)
											*)
										formule = MapThread[Row@{Style[#1, colore], #2}&,
											{
												(* #1 *)
												{
													"Teorema della Corda: " <>  Beautify@(2*r*Sin[\[Beta]]),
													"Area del Triangolo: " <> Beautify@((Subscript[l, 1]*Subscript[l, 2]*Sin[\[Theta]])/2),
													"Teorema dei Seni: " <>  Beautify@(a:Sin[\[Alpha]] == b:Sin[\[Beta]])
												},
												(* #2 *)
												{
													"\n\tcon \[Beta] angolo alla circonferenza",
													"\n\tcon \[Theta] angolo tra i segmenti \!\(\*SubscriptBox[\(l\), \(1\)]\) e \!\(\*SubscriptBox[\(l\), \(2\)]\)",
													"\n\tcon \[Alpha] angolo opposto al lato \!\(\*StyleBox[\"a\",\nFontSlant->\"Italic\"]\)\n\te \[Beta] angolo opposto al lato \!\(\*StyleBox[\"b\",\nFontSlant->\"Italic\"]\)"
												}
											}
										]/.colore->Darker@Red,
										(* L'else dell'if *)
										formule = {}
									],
									
									BaseStyle->FontSize->16
								],
								formule (* le celle successive all'interno della Column *)
							},
							BaseStyle->FontSize->18
						], (* Fine column all'interno del panel *)
						
						Style["Formule", FontSize->24],
						Appearance->"Frameless",
						BaseStyle->Large,
						Background->LightGreen
					](* Fine panel "mostra formule" *)
				}, (* Fine column *)
				(* Cella 2 *)
				SpanFromAbove
			},
			{(* Riga 3 *)
				SpanFromAbove, (* Cella 1 (si unisce alla cella in cui ci sono dati e formule) *)
				(* Cella 2 (Panel per inserimento dei risultati da parte dell'utente) *)
				Panel[Dynamic@Grid@formInputRisultati,
					Style["Inserisci la Soluzione",FontSize->24],
					Appearance->"Frameless",
					BaseStyle->Large,
					Background-> White
				]
			}
		}, (* Fine Grid *)
		Frame->All,
		Alignment->Top
		(* SIAMO ARRIVATI QUI *)
	],
	" ",
	Panel[Dynamic@Grid[Which[passiRisoluzione===Null, {{"Inserisci le soluzioni per visualizzare i passaggi\nper la risoluzione dell'esercizio"}}, True, passiRisoluzione], BaseStyle->{FontSize -> 22},Alignment->{Center,Center},Frame -> All,  ItemStyle-> Darker[Blue]], Style["Passi Soluzione",FontSize->24]]
	},
	BaselinePosition->Bottom
	]
]

ElementiGrafici[tipoAngolo_, angolo_, posizioneCentro_] := Module[{angoloA, angoloC, punti, alpha},
	(* Controlla che i parametri siano coerenti: se viene richiesto che il centro della circonferenza sia esterno
    al triangolo inscritto e che l'angolo al centro sia 180\[Degree], allora viene notificata l'impossibilit\[AGrave] di graficarlo *)
	(* If[Mod[angolo, 180/(tipoAngolo*1.)] == 0 && posizioneCentro == 1, Return[{Text["Impossibile disegnare."]}]]; *)
	
	(* Calcola alpha partendo dai valori di tipoAngolo e angle passati dall'utente.
	alpha corrisponde all'angolo al centro in radianti ed \[EGrave] sempre minore o uguale a Pi.
	Considerando che tipoAngolo \[EGrave] "1" se "angle" rappresenta un angolo al centro e "2" se rappresenta un angolo alla circonferenza,
	si calcola il resto tra l'angolo in radianti ed il massimo valore che l'angolo stesso pu\[OGrave] avere (Pi se tipoAngolo = 1, Pi/2 altrimenti).
	Calcolato il resto, se questo \[EGrave] zero si ritorna il massimo valore dell'angolo, altrimenti il resto.
	Il valore ottenuto viene moltiplicato per "tipoAngolo", in questo modo se "angle" rappresenta l'angolo alla circonferenza, viene moltiplicato per 2,
	altrimenti viene "moltiplicato per 1" (cio\[EGrave] rimane invariato).*)
	alpha=tipoAngolo*Which[resto == 0, 180/(tipoAngolo*1.), True, resto]/.resto ->Mod[angolo,180/(tipoAngolo*1.)];
	
	(* Viene memorizzato l'angolo che identifica il punto "A".*)
	angoloA=RandomReal[{0, 180}];
	
	(*Calcola i primi due punti.
	Viene utilizzato "CirclePoints" per trovare il primo punto; viene poi costruita la matrice di rotazione ed applicata 
	("." rappresenta l'operazione di prodotto tra matrice e vettore) al punto A al fine di ottenere il punto B
	(che forma con A un angolo di alpha gradi).*)
	punti = {A, RotationMatrix[alpha Degree].A }/.A->CirclePoints[{1, angoloA Degree}, 1][[1]];
	
	(*Calcola l'angolo di rotazione da applicare al middlepoint per trovare il punto C coerentemente con la scelta dell'utente.
	Middlepoint \[EGrave] il punto medio dell'arco AB.*)
	angoloC=Which[
		posizioneCentro==1 (* centro della circonferenza esterno al triangolo ABC *),
		(* formula iniziale: RandomChoice[{-1, 1}]*(alpha/2 + 0.1 + RandomReal[{0, beta - 0.1}]) con beta = Pi-alpha
		la formula sottostante equivale a quella sovrastante semplificata
		*)
		(RandomChoice[{-1, 1}]*RandomReal[{alpha+0.1,-alpha+2*180}])/2,
		
		posizioneCentro==2 (* centro della circonferenza interno al triangolo ABC *),
		(* formula iniziale: alpha/2 + beta + RandomReal[{0, alpha}] con beta = Pi-alpha
		la formula sottostante equivale a quella sovrastante semplificata*)
		RandomReal[{-alpha,alpha}]/2+180
	];
	(* Aggiunge il punto C alla lista dei punti.
	Viene calcolato moltiplicando la "RotationMatrix" derivata da angoloC applicata al punto medio (ricavato moltiplicando la "RotationMatrix" derivata da alpha/2 ad A)*)
	AppendTo[punti, RotationMatrix[angoloC Degree].middlePoint/.middlePoint->RotationMatrix[alpha/2 Degree].punti[[1]]];
	
	(*"angoloBAC" calcola l'angolo che definisce il punto di partenza per rappresentare graficamente l'angolo BAC*)
	angoloBAC= Round[PlanarAngle[punti[[1]] -> {punti[[2]], punti[[3]]}]/Degree,0.01];
	(*solvingSteps = CalculateValues[alpha, alpha/2, angoloBAC];*)
	Return[
		{
			Circle[centroCirconferenza, 1] ,
			Point[centroCirconferenza],
			(* Trasforma tutti i punti in elementi grafici*)
			Point/@punti,
			
			(* Crea due triangoli con opacit\[AGrave] del 30%: uno blu (costruito sui punti ABO (dove O \[EGrave] il centro della circonferenza)), un altro rosso costruito su ABC. *)
			{
				EdgeForm[Thin],
				Opacity[0.3],
				Blue,
				Triangle[{punti[[1]], punti[[2]], centroCirconferenza}],
				Red, Triangle[punti]
			},

			(* Permette di rappresentare graficamente gli angoli al centro e alla circonferenza.
			Aggiunge, inoltre, un tooltip che mostra esplicitamente l'ampienza dell'angolo in gradi quando il mouse viene posizionato sopra.
			  *)
			Tooltip[{EdgeForm[{Thickness@0.005,Red}], FaceForm@RGBColor[0,0,0,0], Disk[centroCirconferenza, 0.2, {angoloA Degree, (angoloA + alpha) Degree}]}, StringJoin[ToString@NumberForm[alpha, {5, 2}],"\[Degree]"]],
			Tooltip[{EdgeForm[{Thickness@0.005,Blue}], FaceForm@RGBColor[0,0,0,0], Disk[punti[[3]], 0.2, {tempAngoloC Degree, (tempAngoloC + alpha/2) Degree}]}, StringJoin[ToString@NumberForm[alpha/2., {5, 2}],"\[Degree]"]],
			Tooltip[{EdgeForm[{Thickness@0.005,Darker[Green]}], FaceForm@RGBColor[0,0,0,0], Disk[punti[[1]], 0.15, {tempAngoloA Degree, (tempAngoloA + angoloBAC) Degree}]}, StringJoin[ToString@Round[angoloBAC,0.01], "\[Degree]"]],

			
			(* Etichetta i punti A, B, C, O visualizzati. *)
			MapIndexed[Text[FromCharacterCode[#2[[1]] + 64] , #1, offset,BaseStyle->20]&, punti], 
			Text["O", centroCirconferenza,offset,BaseStyle->20]
		(* "tempAngoloC" calcola l'angolo che definisce il punto di partenza per rappresentare graficamente l'angolo alla circonferenza *)
		}/.{
		tempAngoloC -> PlanarAngle[punti[[3]] -> {{punti[[3]][[1]] + 1, punti[[3]][[2]]}, punti[[1]]}]/Degree,
		tempAngoloA -> PlanarAngle[punti[[1]] -> {{punti[[1]][[1]] + 1, punti[[1]][[2]]}, punti[[2]]}]/Degree,
		centroCirconferenza -> {0,0},
		offset->{-1.5,1.5}}
	]
]


PassiRisoluzione[angoloAlCentro_, angoloAllaCirconferenza_, angoloA_] := Module[
	{
		formule={HoldForm[2*r*Sin[\[Beta]]], HoldForm[l1+l2+l3], HoldForm[(l1*l2*Sin[angolo])/2], HoldForm[(Sin[angoloOppostoAlLatoDaTrovare]*l1)/Sin[angoloOppostoAL1]], HoldForm[Pi-angolo1-angolo2]},
		AB,
		releasedAB,
		perimetroAOB,
		areaAOB,
		BC,
		releasedBC,
		ABC,
		releasedABC,
		AC,
		releasedAC,
		perimetroABC,
		areaABC
	},

	(* teoremaDellaCorda = ;
	perimetro=;
	area=;
	(*  a:sin(angolo2)=b:sin(angolo1)
		a:sin(Subscript[angoloOpposto, a])=b:sin(Subscript[angoloOpposto, b]); *)
	teoremaDeiSeni=;
	angoloTriangolo=; *)

	(* calcolo AB sfruttando il teorema della corda: 2*radius*sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	AB = formule[[1]]/.{r->1, \[Beta]->angoloAllaCirconferenza Degree};
	releasedAB = Round[ReleaseHold@AB, 0.01];
	
	(* perimetro AOB *)
	perimetroAOB=Which[angoloAlCentro == 180, Null, True, formule[[2]]/.{l1->1,l2->1,l3->releasedAB}];
	
	(* calcolo area = 1/2*OB*AO*sin(alpha) dove alpha \[EGrave] l'angolo al centro *)
	areaAOB=Which[angoloAlCentro == 180, Null, True, formule[[3]]/.{l1->1,l2->1,angolo->angoloAlCentro Degree}];

	(* --- adesso abbiamo perimetro e area di AOB --- *)

	(* calcolo BC sfruttando il teorema dei seni BC = (sin(BAC)*AB)/sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	BC = formule[[4]]/.{l1->releasedAB*1., angoloOppostoAlLatoDaTrovare->angoloA Degree, angoloOppostoAL1->angoloAllaCirconferenza Degree};
	releasedBC = Round[ReleaseHold@BC,0.01];
	
	(* calcolo l'angolo ABC = Pi - angoloAllaCirconferenza - BAC *)
	ABC=formule[[5]]/.{angolo1->angoloAllaCirconferenza Degree, angolo2->angoloA Degree};
	releasedABC = Round[ReleaseHold@ABC,0.01];
	
	(* sfrutto di nuovo il teorema dei seni e calcolo AC = (sin(ABC)*AB)/sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	AC = formule[[4]]/.{l1->releasedAB*1.,angoloOppostoAlLatoDaTrovare->releasedABC,angoloOppostoAL1->angoloAllaCirconferenza Degree};
	releasedAC = Round[ReleaseHold@AC,0.01];
		
	(* perimetro AOB *)
	perimetroABC=formule[[2]]/.{l1->releasedAC,l2->releasedBC,l3->releasedAB};
	
	(* calcolo area = 1/2*BC*AC*sin(beta) *)
	areaABC=formule[[3]]/.{l1->releasedBC,l2->releasedAC,angolo->angoloAllaCirconferenza Degree};

	(* --- adesso abbiamo perimetro e area di ABC --- *)

	(* Forma: Array di Array
		[Step1,Step2,...]
		Stepi=[CosaTrovare, Teorema Utilizzato, Formula Simbolica, FormulaApplicata=Risultato] *)
		
	risultatiCorretti={Round[ReleaseHold@perimetroAOB,0.01],Round[ReleaseHold@areaAOB,0.01],Round[ReleaseHold@perimetroABC,0.01],Round[ReleaseHold@areaABC,0.01]};
	Return@{
		{"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", Beautify@formule[[1]], Beautify@AB<>" = "<>ToString@releasedAB},
		{"Perimetro \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[formule[[2]]/.{l1->"\!\(\*OverscriptBox[\(BO\),\(_\)]\)",l2->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Which[perimetroAOB === Null, "Il triangolo non \[EGrave] stato costruito", True, Beautify@perimetroAOB<>" = "<>ToString@risultatiCorretti[[1]]]},
		{"Area \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[formule[[3]]/.{l1->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BO\), \(_\)]\)",angolo->"\[Alpha]"}], Which[areaAOB === Null, "Il triangolo non \[EGrave] stato costruito", True, Beautify@areaAOB<>" = "<>ToString@risultatiCorretti[[2]]]},
		{"\!\(\*OverscriptBox[\(BC\), \(_\)]\)", Beautify[formule[[4]]/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", angoloOppostoAlLatoDaTrovare->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)", angoloOppostoAL1-> "\[Beta]"}], Beautify@BC<>" = "<>ToString@releasedBC},
		{"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", Beautify[formule[[5]]/.{angolo1->"\[Beta]",angolo2->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)"}], Beautify@ABC<>" = "<>ToString@releasedABC},
		{"\!\(\*OverscriptBox[\(AC\), \(_\)]\)", Beautify[formule[[4]]/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)",angoloOppostoAlLatoDaTrovare->"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", angoloOppostoAL1-> "\[Beta]" }], Beautify@AC<>" = "<>ToString@releasedAC},
		{"Perimetro \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[formule[[2]]/.{l1->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Beautify@perimetroABC<>" = "<>ToString@risultatiCorretti[[3]]},
		{"Area \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[formule[[3]]/.{l1->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",angolo->"\[Beta]"}], Beautify@areaABC<>" = "<>ToString@risultatiCorretti[[4]]}
	}
]


CheckSoluzioni[risultati_]:=MapIndexed[Which[#1==risultatiCorretti[[#2]][[1]], LightGreen, True, LightRed]&, risultati]


(* Ritorna TRUE se il valore passato \[EGrave] non-nullo e compreso tra 0 (escluso) e 180, FALSE altrimenti *)
(*[alpha == Null, alpha*tipoAngolo <= 0, alpha*tipoAngolo > 180, And[alpha*tipoAngolo \[Equal] 180, posizioneCentro \[Equal] 1]]*)
IsValid[alpha_, tipoAngolo_, posizioneCentro_]:=And[alpha =!= Null, alpha*tipoAngolo > 0, alpha*tipoAngolo <= 180, Or[alpha*tipoAngolo < 180, posizioneCentro == 2]]
	


Beautify[formula_]:=StringReplace[ToString[formula//TraditionalForm],{"EsercizioTriAngoli`Private`"->""}]


End[]
EndPackage[]
