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

grafica::usage = "grafica ritorna una tabella contenente gli elementi utili alla visualizzazione e risoluzione dell'esercizio.
In particolare, viene mostrata la form per la definizione dell'esercizio, il grafico rappresentante l'esercizio stesso,
una cella in cui sono elencati i dati e le formule da utilizzare, la form per l'immissione dei risultati del problema.
Inoltre, una volta selezionato il pulsante Conferma, \[EGrave] visualizzata una tabella che presenta gli step di risoluzione dell'esercizio."

Begin["`Private`"]


grafica:=DynamicModule[{
		tipoAngolo, (* Il tipo di angolo con cui vengono effettuate le operazioni *)
		inputTipoAngolo = 1,(* Il valore del tipo dell'angolo inserito dall'utente che viene controllato e, eventualmente,
							assegnato a tipoAngolo*)
		alpha=30, (* Il valore di alpha con cui vengono effettuate le operazioni *)
		inputAlpha=30, (* Il valore di alpha inserito dall'utente che viene controllato e, eventualmente, assegnato ad alpha *)
		posizioneCentro, (* La posizione del centro (interno o esterno) selezionata dall'utente tramite il RadioButton *)
		(* Nota: Non c'\[EGrave] un input posizione perch\[EAcute] il cambiamento di posizione una volta effettuati i calcoli non li influenza
		in quanto questo valore viene utilizzato solo al momento della costruzione del problema (cio\[EGrave] solo al click su "Disegna") *)
		angoloBAC,
		graficoEsercizio={Circle[{0, 0}, 1]}, (* La lista degli elementi grafici utilizzati al momento della costruzione del
												problema (cio\[EGrave] solo al click su "Disegna") *)
		datiProblema={"Clicca \"Disegna\" per visualizzare i dati"}, (* La lista dei dati del problema *)
		formInputRisultati={{"Clicca \"Disegna\" per inserire la soluzione"}}, (* Gli elementi che compongono la Grid in cui
																				l'utente inserisce i risultati del problema *)
		passiRisoluzione=Null, (* Gli elementi che compongono la Grid contenente i passi della risoluzione del problema *)
							   (* Formato Grid:  *)
		numeroInputFields = 4, (* Il numero di input richiesti all'utente per la risoluzione del problema
								(corrisponde al doppio del numero di triangoli costruiti) *)
		inputRisultati=Table["", 4], (* La Lista dei risultati inseriti dall'utente (corrisponde al doppio del numero di triangoli
										costruiti - area e perimetro di ogni triangolo ) *)
		coloriInputRisultati=Table[Automatic, 4], (* Lista dei colori che rappresentano la correttezza dei risultati inseriti
													dall'utente *)
		formRisultatiEnabled=False, (* Abilita la scrittura nei field e l'abilitazione del pulsante "Conferma" nella form dei risultati
									inseribili *)
		formule = {}, (* formule visualizzate su richiesta dell'utente *)
		risultatiCorretti (* Risultati corretti del problema. 
							Memorizzati in un array: { perimetroABC, areaABC, perimetroAOB, areaAOB } *)		
	},
	
	Panel[
		Row[{ (* Contiene 2 "celle": una per la configurazione dell'esercizio ed il suo svolgimento,
			l'altra contente esclusivamente i passi di risoluzione *)
			Column[{
				Column@{
					Style["Inserisci i parametri richiesti", FontSize->20],
					Row[{"Tipo di angolo: ", (* Radiobutton per la selezione del tipo di angolo *)
							RadioButtonBar[
								Dynamic@inputTipoAngolo, {1 -> "Centro", 2 -> "Circonferenza"},
								Appearance-> "Vertical"
							]},
							BaseStyle -> FontSize -> 16],
					Row[{"Ampiezza angolo: ", (* Inputfield relativo all'ampiezza dell'angolo *)
						InputField[Dynamic@inputAlpha, Number, ImageSize->{100,25},ContinuousAction->True],
						" \[Degree]"},
						BaseStyle -> FontSize -> 16],
					Row[{ (* Visualizzazione dell'errore *)
							Dynamic@If[InputDisegnaValido[inputAlpha, inputTipoAngolo, posizioneCentro],
								(* Se l'input \[EGrave] valido non stampo errori *)
								"",
								(* Altrimenti *)
								Style[
									Which[
										!AngoloValido[inputAlpha, inputTipoAngolo],
										(* Se l'angolo non \[EGrave] valido, lo notifico *)
										"L'ampiezza deve essere compresa tra 0\[Degree] (escluso) e " <> ToString[180 / inputTipoAngolo] <> "\[Degree].",
										True,
										(* Altrimenti asserisco che il problema sia la combinazione (alpha-tipoAngolo) che non \[EGrave] corretta *)
										"Non \[EGrave] possibile impostare l'ampiezza a "  <> ToString@inputAlpha <> "\[Degree]\ne richiedere il centro della circonferenza esterno."
									],
									Red
								]
							]},
							BaseStyle -> FontSize -> 14],
					Row[{"Centro della circonferenza \[EGrave]: ",  (* RadioButton per la selezione della posizione del centro *)
						RadioButtonBar[
							Dynamic@posizioneCentro, {1 -> "Esterno", 2 -> "Interno"},
							Appearance->"Vertical"]},
					BaseStyle -> FontSize -> 16],
					Row@{
						Button[ (* Pulsante "Disegna" *)
							Style["Disegna", FontSize->14],
							(* Nonostante questo controllo sia effettuato in "Enabled", la correttezza dell'input viene 
							ricontrollata per far fronte ad un "problema" di Mathematica, che consiste nel poter cliccare
							un bottone che dovrebbe essere disabilitato quando, dopo aver scritto un input sbagliato, 
							si rimane all'interno dell'InputField con il cursore ma si clicca il pulsante con il mouse *)
							If[!InputDisegnaValido[inputAlpha, inputTipoAngolo, posizioneCentro], Return[]];
							
							(* Viene impostato il numero di InputField da rendere disponibile all'utente *)
							numeroInputFields = NumeroInputField[inputAlpha, inputTipoAngolo, posizioneCentro];
							
							(* Ridefinisce coloriInputRisultati e inputRisultati in base al numero di InputField calcolati *)
							coloriInputRisultati=Table[Automatic, numeroInputFields];
							inputRisultati=Table["", numeroInputFields];
							
							(* L'alpha inserito dall'utente viene arrotondato al centesimo *)
							alpha=CustomRound@inputAlpha;
							tipoAngolo=inputTipoAngolo;
							(* Vengono caricati gli elementi da mostrare nel grafico *)
							{angoloBAC, graficoEsercizio}=ElementiGrafici[tipoAngolo, alpha, posizioneCentro];
							
							(* Nota: angoloBAC viene calcolato nella funzione "ElementiGrafici" *)
							datiProblema= {
								Row[{"r = 1 (r \[EGrave] il raggio)"},
									BaseStyle -> FontSize -> 16],
								Row[{Dynamic@StringJoin["\!\(\*OverscriptBox[\(BAC\), \(^\)]\) = ", ToString@CustomRound@angoloBAC, "\[Degree]"],
									"\t",
									Dynamic@StringJoin[
										"\!\(\*OverscriptBox[\(A",
										Which[tipoAngolo===1, "O", True, "C"],
										"B\), \(^\)]\) = ",
										ToString@alpha,
										"\[Degree]"]},
									BaseStyle->FontSize -> 16]
							};
							
							(* Costruisce il formInputRisultati: *)
							(* L'obiettivo \[EGrave] creare una riga per ogni triangolo costruito. Per fare questo ci si serve di
							Table. "With" permette di estendere lo scope della "i" definita dalla Table e inizializza un array 
							di nomi utilizzati nelle label. La "i" ha il valore iniziale a 1 e viene incrementato di 2 fino
							ad arrivare al numero di InputField da visualizzare. L'incremento di 2 unit\[AGrave] \[EGrave] dettato dalla scelta
							di mostrare 2 InputField in una stessa riga (di fatto vengono utilizzati "i" ed "i+1").
							Dato che Table restituisce l'array con una coppia di parentesi in pi\[UGrave] ( * ), viene utilizzato
							"Flatten" per "appiattire" l'array al primo livello *)
							formInputRisultati = Flatten[Table[With[{i=i, nomi = {
											"Perimetro \!\(\*OverscriptBox[\(ACB\), \(\[EmptyUpTriangle]\)]\)",
											"Area \!\(\*OverscriptBox[\(ACB\), \(\[EmptyUpTriangle]\)]\)",
											"Perimetro \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)",
											"Area \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)"
									}},
									(* ( * ) necessario per poter ritornare pi\[UGrave] liste dalla Table *)
									{
										{
											Style[nomi[[i]],FontSize->14],
											(* L'utilizzo dell'array "inputRisultati" permette di mantenere la coerenza tra
											la variabile che deve essere modificata e l'errore associato al suo valore *)
											InputField[Dynamic@inputRisultati[[i]],String, ImageSize->{80,25}, BaseStyle -> FontSize->14, ContinuousAction->True, Enabled -> Dynamic@formRisultatiEnabled, Background->Dynamic@coloriInputRisultati[[i]]],
											Style[nomi[[i+1]],FontSize->14 ],
											InputField[Dynamic@inputRisultati[[i+1]],String, ImageSize->{80,25}, BaseStyle -> FontSize->14, ContinuousAction->True, Enabled -> Dynamic@formRisultatiEnabled, Background->Dynamic@coloriInputRisultati[[i+1]]]
											},
											{ (* Se \[EGrave] stato inserito un valore non-numerico nell'InputField corrispondente, 
											si mostra l'errore. StringMatchQ assicura che la stringa in input sia un numero *)
											Dynamic[If[Or[inputRisultati[[i]]==="", StringMatchQ[inputRisultati[[i]], NumberString]], "", Style["Non \[EGrave] possibile inserire una stringa", Red, FontSize->12]]],
											SpanFromLeft,
											Dynamic[If[Or[inputRisultati[[i+1]]==="", StringMatchQ[inputRisultati[[i+1]], NumberString]], "", Style["Non \[EGrave] possibile inserire una stringa", Red, FontSize->12]]],
											SpanFromLeft
										}
										}
									],{i, 1, Length@inputRisultati, 2}], 1];
							
							(* Si inserisce una nota in testa alla lista *)
							formInputRisultati = Insert[formInputRisultati, {Style["Nota bene:\n\[Bullet] I risultati di tutte le operazioni, eccetto quelle\n  trigonometriche, devono essere approssimati\n  ai centesimi.\n\[Bullet] Le operazioni trigonometriche considerano\n  gli angoli in radianti.", FontSize->14, Darker@Gray], SpanFromLeft}, 1];
							
							(* Viene aggiunta l'ultima riga che contiene il pulsante "Conferma" che occupa due celle *)
							AppendTo[formInputRisultati, 
									{
										Button[Style["Conferma",FontSize->16], 
											{risultatiCorretti, passiRisoluzione}=Soluzione[N[alpha*tipoAngolo], N[alpha*tipoAngolo/2], angoloBAC];
											coloriInputRisultati=CheckSoluzioni[CustomRound@ToExpression@#&/@inputRisultati, risultatiCorretti];
											formRisultatiEnabled=False,
											(* Il pulsante viene abilitato quando (1) il form dei risultati \[EGrave] abilitato,
											(2) inputRisultati non contiene stringhe vuote e (3) tutti i valori sono numerici *)
											Enabled -> Dynamic@And[
												formRisultatiEnabled,
												ContainsNone[inputRisultati, {""}],
												AllTrue[
													inputRisultati,
													StringMatchQ[#, NumberString]&
												]
											]
										],
										SpanFromLeft
									}
								];
							
							(* Al click di "Disegna" viene impostato a "true" formRisultatiEnabled che permette la scrittura negli
							InputField per l'inserimento dei risultati. Inoltre, viene inizializzato a Null passiRisoluzione in quanto 
							potrebbero essere presenti dei valori impostati nell'esecuzione precedente *)
							passiRisoluzione = Null;
							formRisultatiEnabled = True,

							(* Il pulsante viene abilitato se gli input per la costruzione dell'esercizio sono corretti *)
							Enabled->Dynamic@InputDisegnaValido[inputAlpha, inputTipoAngolo, posizioneCentro]
						]
					}, (* Fine bottone disegna *)
					(* Note per l'utente *)
					Row[{"L'ampiezza sar\[AGrave] approssimata ai centesimi"},
						BaseStyle->{FontSize->14, Darker@Gray}],
					(* Nel caso in cui il numero di input della soluzione sia 2, significa che l'ampiezza dell'angolo al centro \[EGrave] 180\[Degree] 
					e che la posizione del centro sia stata definita come interna. Dunque, verr\[AGrave] visualizzato l'avviso per l'utente
					riguardo la possibilit\[AGrave] di costruire un solo triangolo *)
					Row[{Dynamic@Which[NumeroInputField[inputAlpha,inputTipoAngolo,posizioneCentro] == 2,
						"NOTA: i dati inseriti permettono la costruzione\ndi un unico triangolo",
						True,
						""]},
						BaseStyle->{FontSize->14, Darker@Blue}],
					(* Messaggio da visualizzare dopo l'immissione dei risultati da parte dell'utente *)
					Row[{Dynamic@Which[ContainsAny[coloriInputRisultati, {LightRed}],
						"Riprova l'esercizio per approfondire\nle tue competenze",
						ContainsAll[coloriInputRisultati, {LightGreen}],
						"Hai completato l'esercizio con successo",
						True,
						""]},
						BaseStyle->{FontSize->14,
							Dynamic@Which[ContainsAny[coloriInputRisultati, {LightRed}],
								Darker@Red,
								True,
								Darker@Green]}]
				},
				Column@{
					Style["Dati", FontSize->20],
					Dynamic@Column[datiProblema, BaseStyle->FontSize->16, Background->LightYellow]
				}
			}, BaselinePosition->Top, Alignment->Center],
			" ",
			Column[{
				Dynamic@Graphics[graficoEsercizio,ImageSize->300],
				Column@{
					Style["Inserisci la Soluzione", FontSize->20],
					Dynamic@Grid[formInputRisultati, BaseStyle->FontSize->16]
				}
			}, BaselinePosition->Top, Alignment->Center],
			" ",
			Column[{
				Dynamic@Column[{ (* La column mostra il bottone e le formule una sotto l'altra *)
					Style["Formule", FontSize->20],
					(* Flatten "rende piatto" l'array passato a Column: "formule" \[EGrave] un array di
						Rows (il perch\[EAcute] viene spiegato successivamente) e deve essere ridotto a "lista" di rows:
						es: {button, {Row@"a",Row@"b",Row@"c"}} deve diventare {button, Row@"a",Row@"b",Row@"c"} (senza graffe)
						*)
					Column@{
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
							BaseStyle->FontSize->14
						],
						Dynamic@Column@formule (* Celle successive all'interno della Column *)
					}
				}, Background->LightGreen, BaseStyle->FontSize->16], (* Fine column all'interno del panel *)
				Dynamic@Grid[
						Which[passiRisoluzione===Null,
						{{"Inserisci le soluzioni per visualizzare i passaggi\nper la risoluzione dell'esercizio"}},
						True,
						passiRisoluzione
						],
						BaseStyle->{FontSize -> 16},
						Frame -> All,
						ItemStyle-> Darker@Blue
					]
			}, BaselinePosition->Top, Alignment->Center]
		}],
		Background->LightBlue
	]
]


(* ElementiGrafici[tipoAngolo_, angolo_, posizioneCentro_] ritorna una lista di oggetti grafici contenente:
- La circonferenza goniometrica su cui vengono costruiti i triangoli
- Due ToolTips che mostrano le dimensioni dell'angolo al centro e di quello alla circonferenza, rispettivamente
- Il triangolo AOB dove O \[EGrave] il centro della circonferenza e A e B formano un angolo al centro che rispetta i criteri passati come parametri
- Il triangolo ABC inscritto nella circonferenza che insiste sullo stesso arco AB e rispetta i criteri passati come parametri.
Parametri:	
- \"tipoAngolo\" definisce il tipo di angolo che \"angle\" rappresenta: 1\[Rule]angolo al centro, 2\[Rule]angolo alla circonferenza
- \"angle\" rappresenta la grandezza dell'angolo specificato
- \"posizioneCentro\" specifica se il centro della circonferenza deve essere interno o esterno al triangolo \"ABC\": 1\[Rule]esterno , 2\[Rule]interno.
*)
ElementiGrafici[tipoAngolo_, angolo_, posizioneCentro_] := Module[{angoloA, angoloC, punti, alpha, angoloBAC},
	
	(* Calcola alpha partendo dai valori di tipoAngolo e angle passati dall'utente.
	alpha corrisponde all'angolo al centro in radianti ed \[EGrave] sempre minore o uguale a 180.
	Considerando che tipoAngolo \[EGrave] "1" se "angle" rappresenta un angolo al centro e "2" se rappresenta un angolo alla circonferenza,
	si calcola il resto tra l'angolo in radianti ed il massimo valore che l'angolo stesso pu\[OGrave] avere (180 se tipoAngolo = 1, 90 altrimenti).
	Calcolato il resto, se questo \[EGrave] zero si ritorna il massimo valore dell'angolo, altrimenti il resto.
	Il valore ottenuto viene moltiplicato per "tipoAngolo", in questo modo se "angle" rappresenta l'angolo alla circonferenza, 
	viene moltiplicato per 2, altrimenti viene "moltiplicato per 1" (cio\[EGrave] rimane invariato).*)
	alpha=tipoAngolo*Which[resto == 0, 180/(tipoAngolo*1.), True, resto]/.resto ->Mod[angolo,180/(tipoAngolo*1.)];
	
	(* Viene memorizzato l'angolo che identifica il punto "A".*)
	angoloA=RandomReal[{0, 180}];
	
	(* Calcola i primi due punti.
	Viene utilizzato "CirclePoints" per trovare il primo punto; viene poi costruita la matrice di rotazione ed applicata 
	("." rappresenta l'operazione di prodotto tra matrice e vettore) al punto A al fine di ottenere il punto B
	(che forma con A un angolo di alpha gradi). *)
	punti = {A, RotationMatrix[alpha Degree].A }/.A->CirclePoints[{1, angoloA Degree}, 1][[1]];
	
	(* Calcola l'angolo di rotazione da applicare al middlepoint per trovare il punto C coerentemente con la scelta dell'utente.
	Middlepoint \[EGrave] il punto medio dell'arco AB. *)
	angoloC=Which[
		posizioneCentro==1 (* centro della circonferenza esterno al triangolo ABC *),
		(* TODO RIFARE STO CALCOLO CON 2 AL POSTO DI 0.1 *)
		(* formula iniziale: RandomChoice[{-1, 1}]*(alpha/2 + 2 + RandomReal[{0, beta - 2}]) con beta = 180-alpha
		la formula sottostante equivale a quella sovrastante semplificata
		*)
		(RandomChoice[{-1, 1}]*RandomReal[{alpha+4,360-alpha}])/2,
		
		posizioneCentro==2 (* centro della circonferenza interno al triangolo ABC *),
		(* formula iniziale: alpha/2 + beta + RandomReal[{0, alpha}] con beta = 180-alpha
		la formula sottostante equivale a quella sovrastante semplificata*)
		180+RandomReal[{-alpha,alpha}]/2
	];
	(* Aggiunge il punto C alla lista dei punti.
	Viene calcolato moltiplicando la "RotationMatrix" derivata da angoloC applicata al punto medio
	(ricavato moltiplicando la "RotationMatrix" derivata da alpha/2 ad A)*)
	AppendTo[punti, RotationMatrix[angoloC Degree].middlePoint/.middlePoint->RotationMatrix[alpha/2 Degree].punti[[1]]];
	
	(* "angoloBAC" calcola l'angolo che definisce il punto di partenza per rappresentare graficamente l'angolo BAC *)
	angoloBAC= CustomRound@PlanarAngle[punti[[1]] -> {punti[[2]], punti[[3]]}]/Degree;
	
	Return[{
		angoloBAC,
		{
			Circle[centroCirconferenza, 1] ,
			Point@centroCirconferenza,
			(* Trasforma tutti i punti in elementi grafici *)
			Point/@punti,
			
			(* Crea due triangoli con opacit\[AGrave] del 30%: uno blu (costruito sui punti ABO (dove O \[EGrave] il centro della circonferenza)),
			un altro rosso costruito su ABC. *)
			{
				EdgeForm@Thin,
				Opacity@0.3,
				Blue,
				Triangle@{punti[[1]], punti[[2]], centroCirconferenza},
				Red, Triangle@punti
			},

			(* Permette di rappresentare graficamente gli angoli al centro e alla circonferenza.
			Aggiunge, inoltre, un tooltip che mostra esplicitamente l'ampienza dell'angolo in gradi quando il mouse viene posizionato sopra.
			 *)
			Tooltip[{EdgeForm@{Thickness@0.005,Red}, FaceForm@RGBColor[0,0,0,0], Disk[centroCirconferenza, 0.2, {angoloA Degree, (angoloA + alpha) Degree}]}, ToString@CustomRound@alpha <> "\[Degree]"],
			Tooltip[{EdgeForm@{Thickness@0.005,Blue}, FaceForm@RGBColor[0,0,0,0], Disk[punti[[3]], 0.2, {tempAngoloC Degree, (tempAngoloC + alpha/2) Degree}]}, ToString@CustomRound@(alpha/2.) <> "\[Degree]"],
			Tooltip[{EdgeForm@{Thickness@0.005,Darker@Green}, FaceForm@RGBColor[0,0,0,0], Disk[punti[[1]], 0.15, {tempAngoloA Degree, (tempAngoloA + angoloBAC) Degree}]}, ToString@CustomRound@angoloBAC <> "\[Degree]"],

			
			(* Etichetta i punti A, B, C, O visualizzati. *)
			MapIndexed[Text[FromCharacterCode@(#2[[1]] + 64) , #1, offset, BaseStyle->20]&, punti], 
			Text["O", centroCirconferenza, offset, BaseStyle->20]
		(* "tempAngoloC" calcola l'angolo che definisce il punto di partenza per rappresentare graficamente l'angolo alla circonferenza *)
		}/.{
		tempAngoloC -> PlanarAngle[punti[[3]] -> {{punti[[3]][[1]] + 1, punti[[3]][[2]]}, punti[[1]]}]/Degree,
		tempAngoloA -> PlanarAngle[punti[[1]] -> {{punti[[1]][[1]] + 1, punti[[1]][[2]]}, punti[[2]]}]/Degree,
		centroCirconferenza -> {0,0},
		offset->{-1.5,1.5}}
	}]
]


Soluzione[angoloAlCentro_, angoloAllaCirconferenza_, angoloA_] := Module[
	{
																							(* Teorema dei Seni := a:sin(angolo2)=b:sin(angolo1) *)
		formule={HoldForm@(2*r*Sin@\[Beta]), HoldForm@(l1+l2+l3), HoldForm@((l1*l2*Sin@angolo)/2), HoldForm@((Sin@angolo2*l1)/Sin@angolo1), HoldForm@(Pi-angolo1-angolo2)},
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
		areaABC,
		risultatiCorretti
	},

	(* Calcola AB sfruttando il teorema della corda: 2*radius*sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	AB = formule[[1]]/.{r->1, \[Beta]->angoloAllaCirconferenza Degree};
	releasedAB = CustomRound@ReleaseHold@AB;
	
	(* Perimetro AOB *)
	perimetroAOB=Which[angoloAlCentro == 180, Null, True, formule[[2]]/.{l1->1,l2->1,l3->releasedAB}];
	
	(* Calcola area = 1/2*OB*AO*sin(alpha) dove alpha \[EGrave] l'angolo al centro *)
	areaAOB=Which[angoloAlCentro == 180, Null, True, formule[[3]]/.{l1->1,l2->1,angolo->angoloAlCentro Degree}];

	(* Calcola BC sfruttando il teorema dei seni BC = (sin(BAC)*AB)/sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	BC = formule[[4]]/.{l1->releasedAB*1., angolo2->angoloA Degree, angolo1->angoloAllaCirconferenza Degree};
	releasedBC = CustomRound@ReleaseHold@BC;
	
	(* Calcola l'angolo ABC = 180 - angoloAllaCirconferenza - BAC *)
	ABC=formule[[5]]/.{angolo1->angoloAllaCirconferenza Degree, angolo2->angoloA Degree};
	releasedABC = CustomRound@ReleaseHold@ABC;
	
	(* Si riutilizza di nuovo il teorema dei seni e viene calcolato AC = (sin(ABC)*AB)/sin(beta) dove beta \[EGrave] l'angolo alla circonferenza *)
	AC = formule[[4]]/.{l1->releasedAB*1.,angolo2->releasedABC,angolo1->angoloAllaCirconferenza Degree};
	releasedAC = CustomRound@ReleaseHold@AC;
	
	(* Perimetro AOB *)
	perimetroABC=formule[[2]]/.{l1->releasedAC,l2->releasedBC,l3->releasedAB};
	
	(* Calcolo l'area = 1/2*BC*AC*sin(beta) *)
	areaABC=formule[[3]]/.{l1->releasedBC,l2->releasedAC,angolo->angoloAllaCirconferenza Degree};
	
	risultatiCorretti={
		ReleaseHold@perimetroABC,
		CustomRound@ReleaseHold@areaABC,
		ReleaseHold@perimetroAOB,
		CustomRound@ReleaseHold@areaAOB
		};
		
	(* Struttura di ritorno: Array di Array
		[Step_1,Step_2,...,Step_i]
		Step_1=[perimetroABC, areaABC, perimetroAOB, areaAOB]
		Step_i=[CosaTrovare, Teorema Utilizzato, Formula Simbolica, FormulaApplicata=Risultato] *)
	Return@{
		{ReleaseHold@perimetroABC, CustomRound@ReleaseHold@areaABC, ReleaseHold@perimetroAOB, CustomRound@ReleaseHold@areaAOB},
		{{"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", Beautify@formule[[1]], Beautify@AB<>" = "<>ToString@releasedAB},
		{"Perimetro \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[formule[[2]]/.{l1->"\!\(\*OverscriptBox[\(BO\),\(_\)]\)",l2->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Which[perimetroAOB === Null, "Il triangolo non \[EGrave] stato costruito", True, Beautify@perimetroAOB<>" = "<>ToString@risultatiCorretti[[3]]]},
		{"Area \!\(\*OverscriptBox[\(AOB\), \(\[EmptyUpTriangle]\)]\)", Beautify[formule[[3]]/.{l1->"\!\(\*OverscriptBox[\(AO\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BO\), \(_\)]\)",angolo->"\[Alpha]"}], Which[areaAOB === Null, "Il triangolo non \[EGrave] stato costruito", True, Beautify@areaAOB<>" = "<>ToString@risultatiCorretti[[4]]]},
		{"\!\(\*OverscriptBox[\(BC\), \(_\)]\)", Beautify[formule[[4]]/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)", angolo2->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)", angolo1-> "\[Beta]"}], Beautify@BC<>" = "<>ToString@releasedBC},
		{"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", Beautify[formule[[5]]/.{angolo1->"\[Beta]",angolo2->"\!\(\*OverscriptBox[\(BAC\), \(^\)]\)"}], Beautify@ABC<>" = "<>ToString@releasedABC},
		{"\!\(\*OverscriptBox[\(AC\), \(_\)]\)", Beautify[formule[[4]]/.{l1->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)",angolo2->"\!\(\*OverscriptBox[\(ABC\), \(^\)]\)", angolo1-> "\[Beta]" }], Beautify@AC<>" = "<>ToString@releasedAC},
		{"Perimetro \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[formule[[2]]/.{l1->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l3->"\!\(\*OverscriptBox[\(AB\), \(_\)]\)"}], Beautify@perimetroABC<>" = "<>ToString@risultatiCorretti[[1]]},
		{"Area \!\(\*OverscriptBox[\(ABC\), \(\[EmptyUpTriangle]\)]\)", Beautify[formule[[3]]/.{l1->"\!\(\*OverscriptBox[\(BC\), \(_\)]\)",l2->"\!\(\*OverscriptBox[\(AC\), \(_\)]\)",angolo->"\[Beta]"}], Beautify@areaABC<>" = "<>ToString@risultatiCorretti[[2]]}
	}}
]


(* Confronta i risultati immessi dall'utente con quelli corretti (calcolati dal programma) utilizzando MapThread, il quale prende la
lista dei risultati utente e quella dei risultati corretti e confronta che ogni elemento della prima sia uguale al corrispondente
all'interno dell'altra.
Restituisce una lista di colori, i valori possibili sono LightRed (il quale indica che il risultato utente \[EGrave] errato)
	e LightGreen (il quale indica che il risultato utente \[EGrave] corretto) *)
CheckSoluzioni[risultatiUtente_, risultatiCorretti_]:=MapThread[Which[#1 === #2, LightGreen, True, LightRed]&,{risultatiUtente, risultatiCorretti[[1;;Length@risultatiUtente]]}]


(* Controlla che l'angolo sia valido e che almeno un triangolo sia disegnabile *)
InputDisegnaValido[alpha_, tipoAngolo_, posizioneCentro_]:=And[
	AngoloValido[alpha, tipoAngolo],
	TriangoloDisegnabile[alpha, tipoAngolo, posizioneCentro]
]

(* Ritorna TRUE se il valore passato \[EGrave] non-nullo e compreso tra 0 (escluso) e 180, FALSE altrimenti *)
AngoloValido[alpha_, tipoAngolo_]:=And[alpha =!= Null, alpha*tipoAngolo > 0, alpha*tipoAngolo <= 180]

(* Ritorna TRUE se, con i parametri passati, almeno un triangolo \[EGrave] disegnabile. 
Questo controllo \[EGrave] necessario in quanto non \[EGrave] possibile disegnare un triangolo
rettangolo inscritto (ABC) dove il centro del cerchio non cade sull'ipotenusa (AB) *)
TriangoloDisegnabile[alpha_, tipoAngolo_, posizioneCentro_]:=Or[alpha*tipoAngolo < 180, posizioneCentro == 2]

(* Restituisce il numero di InputField in base al numero di triangoli disegnabili *)
NumeroInputField[alpha_, tipoAngolo_, posizioneCentro_]:=Which[alpha*tipoAngolo == 180 && posizioneCentro == 2, 2, True, 4]

(* Approssima ai centesimi. Tale implementazione \[EGrave] stata necessaria a causa di problematiche riscontrate con le built-in esistenti.
In particolare, la built-in Round, talvolta, effettua un'approssimazione ai decimi anzich\[EAcute] ai centesimi nonostante il valore passato
fosse approssimabile ai centesimi *)
CustomRound[number_]:=N@(Floor@(number*100+0.5)/100)

(* Prende una formula e restituisce una stringa in TraditionalForm. Lo StringReplace inibisce il percorso dei parametri non valutati
rimuovendo il path che li caratterizza *)
Beautify[formula_]:=StringReplace[ToString[formula//TraditionalForm],{"EsercizioTriAngoli`Private`"->""}]


End[]
EndPackage[]
