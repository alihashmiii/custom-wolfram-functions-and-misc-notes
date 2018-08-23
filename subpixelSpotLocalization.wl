(* for detecting spots/single molecules at subpixel resolution. Used in SMTrack.m
Note: This is a custom function. The last If statement needs to be modified for usage elsewhere.
*)

(* modelFit[image_, mask_, shape_, box_] := Block[{pixelpos, pixelval, img, data, data3D, a, b, mx, my, sx, sy, x, y, fm, loc},
   pixelpos = mask["NonzeroPositions"];
   pixelval = PixelValue[image, pixelpos];
   img = ReplacePixelValue[shape, Thread[PixelValuePositions[shape, 1] -> pixelval]];
   data = ImageData@ImagePad[img, 2];
   data3D = Flatten[MapIndexed[{#2[[1]], #2[[2]], #1} &, data, {2}], 1];
   fm = NonlinearModelFit[data3D, a E^(-(((-my + y) Cos[b] - (-mx + x) Sin[b])^2/(2 sy^2)) - ((-mx + x) Cos[b] + (-my + y) Sin[
               b])^2/(2 sx^2)), {a, b, mx, my, sx, sy}, {x, y}];
   {a, b, mx, my, sx, sy} = {a, b, mx, my, sx, sy} /. fm["BestFitParameters"];
   loc = Mean /@ Transpose@box + {mx, my} - (Dimensions@data) / 2.0
   ]; *)
   
modelFit[image_, mask_, shape_, box_] := Block[{pixelpos,pixelval,img,data,a,b,weights,data3D,
 mx,my,sx,sy,x,y,fm,dx,dy,cent,background,bestfit,intensityGuess,brightest,
 dimMask = Dimensions@mask},
 pixelpos = mask["NonzeroPositions"];
 pixelval = PixelValue[image, pixelpos];
 brightest = Max@pixelval;
 img = ReplacePixelValue[shape, Thread[PixelValuePositions[shape, 1] -> pixelval]];
 data = ImageData@ImagePad[img, 1];
 data3D = Flatten[MapIndexed[{First@#2, Last@#2, #1} &, data, {2}], 1];
 {dx,dy}= Dimensions@data;
 cent = N[(dx + dy)/2];
 intensityGuess = Max@data;
 fm = NonlinearModelFit[data3D,
  background+(a * Exp[-((x-mx)^2/(2*sx^2))-((y-my)^2/(2*sy^2))]),
 {{background,Automatic},{a,intensityGuess},{mx,cent/2},{my,cent/2},{sx,cent/4},{sy,cent/4}},
 {x, y}];
 bestfit = fm["BestFitParameters"];
 (* if any param is less than 0 then we run a constrained fit *)
 If[Length[Position[bestfit[[All,2]],x_/;x<0]]>0,
  fm = NonlinearModelFit[data3D,
 {background + (a * Exp[-((x-mx)^2/(2*sx^2)) - ((y-my)^2/(2*sy^2))]),
 {background > 0, a > 0, mx > 0, my > 0, sx > 0, sy > 0}},
 {{background,Automatic},{a,intensityGuess},{mx,cent/2},{my,cent/2},{sx,cent/4},{sy,cent/4}},
 {x,y}
 ];
];
 bestfit = fm["BestFitParameters"];
 If[brightest>0.9,
 weights =  data3D[[All,3]];
 weights = weights/.{x_/; x<0.90 -> 1.0,x_/;x<1.0 -> 0.0};
 (* set all intensities > 0.90 to zero weights *)
 fm = NonlinearModelFit[data3D,
 {background + (a*Exp[-((x-mx)^2/(2*sx^2)) - ((y-my)^2/(2*sy^2))]),
 {background > 0, a > 0 ,mx > 0, my > 0, sx > 0, sy > 0}},
 {{background,Automatic},{a,intensityGuess},{mx,cent/2},{my,cent/2},{sx,cent/4},{sy,cent/4}},
 {x,y}, Weights->weights
 ];
];
 bestfit = fm["BestFitParameters"];
 {background,a,mx,my,sx,sy} = {background,a,mx,my,sx,sy} /. fm["BestFitParameters"];
 (* {(Mean/@Transpose@box + ({mx,my} -{dx,dy}/2.0)),background,a,mx,my,sx,sy} *)
 If[AnyTrue[#,(# < 0 || # > First@dimMask || # > Last@dimMask)&],
   {Values@ComponentMeasurements[mask,"Centroid"],0,0,0,0,0,0}~FlattenAt~1,
   {#,background,a,mx,my,sx,sy}]&[(Mean/@Transpose@box+({mx,my}-{dx,dy}/2.0))]
];
