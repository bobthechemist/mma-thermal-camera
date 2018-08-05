(* ::Thermal Camera Interface:: *)

BeginPackage["tc`"];

$tcVersion = "0.1.0";
$tcDevice = Null;
$tcPort = "COM6";
$tcCurrentImage = Null;
$tcImages = Association[];
$tcTask = Null;
$tcRunning = False;

(* Public functions *)
openCamera::usage = "";
openInterface::usage = "";
closeCamera::usage = "";
captureImage::usage = "";
clearImages::usage = "";

Begin["`Private`"];

(* Private variables *)
$tcImageCounter = 1;

(* Open the camera *)
openCamera[] := openCamera[$tcPort];
openCamera[port_String] := (
	$tcDevice = DeviceOpen["Serial",{port, "BaudRate" -> 115200}];
);

(* Close the camera *)
closeCamera[] := DeviceClose[$tcDevice];

(* Capture an image *)
Options[captureImage] = {"fix64"->False,"collect"->False};
captureImage[OptionsPattern[]] := Module[{},
	(* Current iteration of Arduino code sends image on any message containing a newline from Serial line *)
	DeviceWrite[$tcDevice, "\n"];
  (* Maximum refresh rate for the camera is 10 Hz *)
	Pause[0.1];
	(* Arduino returns a bunch of ASCII codes which must be converted into characters.  Next, the string (space delimited) is split, converted into numbers and partitioned into an 8x8 array.  The last few characters in the string are junk so have been removed. *)
	$tcCurrentImage = Partition[
		ToExpression@StringSplit@StringTake[FromCharacterCode@
			DeviceReadBuffer[$tcDevice], {1, -4}], 8];
	If[OptionValue["fix64"]===True,
			$tcCurrentImage[[8,8]] = 
				Mean[{$tcCurrentImage[[8,7]],$tcCurrentImage[[7,8]]}];
	];
	If[OptionValue["collect"]===True,
		$tcImages[$tcImageCounter++] = $tcCurrentImage;
	];
]

clearImages[] := Module[{},
	$tcImages = Association[];
	$tcImageCounter = 1;
]

openInterface[] := Dynamic@Row@{
   ListDensityPlot[$tcCurrentImage, 
    PlotLabel -> Mean@Flatten@$tcCurrentImage, 
    InterpolationOrder -> 2, ColorFunction -> "TemperatureMap", 
    PlotLegends -> Automatic, PlotRange -> All, ImageSize -> Medium, 
    Frame -> False, ClippingStyle -> Automatic],
   Button["Snap", If[$tcRunning == False, captureImage[]]],
   Button[
    If[$tcRunning, "Stop", "Start"],
    If[$tcRunning,
     (TaskRemove@$tcTask; $tcRunning = False;),
     ($tcTask = 
       SessionSubmit[ScheduledTask[captureImage[], 0.2]]; $tcRunning =
        True;)
     ]
    ]
}




End[];

EndPackage[];

