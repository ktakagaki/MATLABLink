(* ::Package:: *)

(* Mathematica Package *)

BeginPackage["MATLABLink`",{"JLink`"}]


MATLABConnect::usage="Connect to a MATLAB session.";
	Options[MATLABConnect]={Verbose->True};
MATLABDisconnect::usage="Disconnect from the current MATLAB session. If Exit->True, data will be lost.";
	Options[MATLABDisconnect]={Exit->True};

MATLABSet::usage="";
MATLABGet::usage="Gets a MATLAB variable and returns the contents as a Mathematica expression.";

MATLABEval::usage="Evaluates a MATLAB string. \
	If the output is not caught with a variable, returns the result as a Mathematica expression.";


MATLABSetDirectory::usage="Sets the current MATLAB directory. Equivalent to \"cd ...\".";


InstallJava[];
MATLABConnect[];


(* ::Section:: *)
(*Private*)


Begin["`Private`"]


General::invalidArgs="Function called with invalid arguments `1`.";
General::invalidOptionValue="Option argument `1` is invalid.";


proxyFactoryClass = LoadJavaClass["matlabcontrol.MatlabProxyFactory"];
proxyClass = LoadJavaClass["matlabcontrol.MatlabProxy"];
numericArrayClass = LoadJavaClass["matlabcontrol.extensions.MatlabNumericArray"];
typeConverterClass = LoadJavaClass["matlabcontrol.extensions.MatlabTypeConverter"];


MLCProxyFactory = JavaNew[proxyFactoryClass]; (*Internal buffer for MatlabProxyFactory*)
MLCProxy = Null; (*Internal buffer for MatlabProxy*)
MLCTypeConverter = Null; (*Internal buffer for MatlabTypeConverter*)


(* ::Subsection:: *)
(*Connect, Disconnect*)


MATLABConnect[OptionsPattern[]]:=
	Module[{},
		If[MLCProxy==Null,
			MLCProxy = MLCProxyFactory@getProxy[];
			MLCProxy@eval["disp('Hello, this MATLAB session will be used from Mathematica via MATLABLink.')"];
			MLCTypeConverter=JavaNew[typeConverterClass, MLCProxy],
			Message[MATLABConnect::alreadyConnected]
		];
		If[OptionValue[Verbose]==True,
			Print["MATLAB Version = " <> (MLCProxy@returningEval["version", 1])[[1]] ];
			Print["Java VM Version = " <> (MLCProxy@eval["version -java;"]; MLCProxy@getVariable["ans"]) ];
		];
	];

MATLABConnect::alreadyConnected="Already connected to a MATLAB instance!";
MATLABConnect[args___] := Message[MATLABConnect::invalidArgs, {args}];


MATLABDisconnect[OptionsPattern[]]:= 
	Module[{},
		MLCProxy@disconnect[];
		MLCProxy=Null;
		If[OptionValue[Exit], MLCProxy@exit[]];
	];

MATLABDisconnect[args___] := Message[MATLABDisconnect::invalidArgs, {args}];


(* ::Subsection:: *)
(*Set, Get*)


MATLABSet[variable_String, value_Real]:= MLCProxy@setVariable[variable, MakeJavaObject[value]];
MATLABSet[variable_String, value_Integer]:= MLCProxy@setVariable[variable, MakeJavaObject[value]];
MATLABSet[variable_String, value_Complex]:= 
	MLCProxy@eval[ variable <> " = " <> ToString[Re[value]] <> " + i * " <> ToString[Im[value]] <> ";"];
MATLABSet[variable_String, value_String]:= MLCProxy@setVariable[variable, MakeJavaObject[value]];


MATLABSet[variable_String, val_List]:= 
	Module[{arrayDepth, (*tempRe, tempIm,*) value},
		value=val;
		arrayDepth=ArrayDepth[value];
		If[arrayDepth==1, value={value}];
		If[arrayDepth>4,
			Message[MATLABSet::upTo4],
			If[arrayDepth+1 != Depth[val], Message[MATLABSet::ragged],
				If[ Position[value, _Complex, Infinity, 1] == {},
					MLCTypeConverter@setNumericArray[variable, 
						JavaNew[numericArrayClass, N[value], Null]
					],
					MLCTypeConverter@setNumericArray[variable, 
						JavaNew[numericArrayClass, N[Re[value]], N[Im[value]]]
					]
				] (*Complex?*)	
			]
		];
	];


MATLABSet[args___] := Message[MATLABConnect::invalidArgs, {args}];
MATLABSet::upTo4="MATLABLink only supports up to 4D arrays.";
MATLABSet::ragged="MATLABLink only supports non-ragged arrays.";


MATLABGet[variable_String]:=
	Module[{class, size, (*sizeLen,*) isReal, scalar=False, flatten=False, return},
		class = (MLCProxy@returningEval["class("<>variable<>")", 1])[[1]];
		size = (MLCProxy@returningEval["size("<>variable<>")", 1])[[1]];

		Switch[class,
			"char", return = MLCProxy@getVariable[variable],
			"double", 
				If[ size[[1]] == 1., 
				If[ size[[2]] == 1., scalar=True, flatten=True]];
				Switch[Length[size], 
					2, 
					isReal = (MLCProxy@returningEval["isreal("<>variable<>")",1])[[1, 1]];
					If[isReal,
						return = MLCTypeConverter@getNumericArray[variable]@getRealArray2D[],

						(*realvar = (MLCProxy@returningEval["genvarname('realvar')"])[[1]];
						imvar = (MLCProxy@returningEval["genvarname('imvar')"])[[1]];*)
						MLCProxy@eval["MLCProxyTempReal = real("<>variable<>");"];
						MLCProxy@eval["MLCProxyTempImag = imag("<>variable<>");"];
						return = MLCTypeConverter@getNumericArray["MLCProxyTempReal"]@getRealArray2D[]
							+ I*MLCTypeConverter@getNumericArray["MLCProxyTempImag"]@getRealArray2D[]
					](*If isReal*),

					3, 
					isReal = (MLCProxy@returningEval["isreal("<>variable<>")",1])[[1, 1]];
					If[isReal,
						return = MLCTypeConverter@getNumericArray[variable]@getRealArray3D[],

						(*realvar = (MLCProxy@returningEval["genvarname('realvar')"])[[1]];
						imvar = (MLCProxy@returningEval["genvarname('imvar')"])[[1]];*)
						MLCProxy@eval["MLCProxyTempReal = real("<>variable<>");"];
						MLCProxy@eval["MLCProxyTempImag = imag("<>variable<>");"];
						return = MLCTypeConverter@getNumericArray["MLCProxyTempReal"]@getRealArray3D[]
							+ I*MLCTypeConverter@getNumericArray["MLCProxyTempImag"]@getRealArray3D[]
					](*If isReal*),

					4, 
					isReal = (MLCProxy@returningEval["isreal("<>variable<>")",1])[[1, 1]];
					If[isReal,
						return = MLCTypeConverter@getNumericArray[variable]@getRealArray4D[],

						(*realvar = (MLCProxy@returningEval["genvarname('realvar')"])[[1]];
						imvar = (MLCProxy@returningEval["genvarname('imvar')"])[[1]];*)
						MLCProxy@eval["MLCProxyTempReal = real("<>variable<>");"];
						MLCProxy@eval["MLCProxyTempImag = imag("<>variable<>");"];
						return = MLCTypeConverter@getNumericArray["MLCProxyTempReal"]@getRealArray4D[]
							+ I*MLCTypeConverter@getNumericArray["MLCProxyTempImag"]@getRealArray4D[]
					](*If isReal*),
					_, 
					return = Null; Message[MATLABGet::sizeNotSupported, Length[size]]
				](*Switch*),
			_, return = Null; Message[MATLABGet::classNotSupported, class]		
		](*Switch class*);
		If[scalar, return[[1,1]],
			If[flatten, Flatten[return], return]
		]
	];


MATLABGet::sizeNotSupported="The return array size `1` is not supported.";
MATLABGet::classNotSupported="The return class type `1` is not supported.";
MATLABGet[args___] := Message[MATLABGet::invalidArgs, {args}];


(* ::Subsection:: *)
(*Eval*)


MATLABEval[command_String]:= 
	Module[{},
		If[  StringMatchQ[command,___~~";"~~ x:WhitespaceCharacter...] (*Has semicolon at very end*),
			MLCProxy@eval[command],
			MLCProxy@eval[command]; MATLABGet["ans"]
		]
	];

MATLABEval[args___] := Message[MATLABEval::invalidArgs, {args}];


(* ::Subsection:: *)
(*Set, Get*)


MATLABSetDirectory[directory_String]:= MATLABEval["cd '"<>directory<>"'"];
MATLABSetDirectory[args___] := Message[MATLABSetDirectory::invalidArgs, {args}];


(* ::Section:: *)
(*End*)


MATLABConnect[];


End[]

EndPackage[]
