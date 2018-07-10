(* autocorrelation for a 1D vector *)
autoCorrelation1D[vector_]:= CorrelationFunction[vector,{Length@vector - 1}];
   
(* computing 2D auto/cross-correlations using ImageCorrelate (output is the same dimension as the input) and ListCorrelate (output size is
larger than the input) *)

(* different methods can be used for ImageCorrelate: Dot, EuclideanDistance, SquaredEuclideanDistance, NormalizedSquaredEuclideanDistance,
CosineDistance etc. *)

imageCorrelation[input_Image]:= ImageAdjust@ImageCorrelate[input,input,PerformanceGoal -> "Quality"];

autoCorr2D[input:{{_?NumberQ, _?NumberQ}..} | _Image]:= Module[{mat},
mat = If[Head@input === Image, ImageData@input, input];
ListCorrelate[mat,mat,{-1,1},0]
];
