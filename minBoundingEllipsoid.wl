(* Minimum Bounding Ellipsoid: as given on the Wolfram Documentation:
https://www.wolfram.com/language/12/convex-optimization/minimal-bounding-ellipsoid.html?product=mathematica*)

n = 3; (* dim *)

im = image3D (*input Image3D here *);
pixpos = PixelValuePositions[im,1]; (* find positions where pixel = 1*)
meanpos = N@Mean[pixpos];
convexHull = ConvexHullMesh[# - meanpos & /@ pixpos, BoxRatios -> {1, 1, 1}];
mesh = DiscretizeRegion[convexHull,BoxRatios -> {1,1,1},ImageSize -> Tiny]

points = data = MeshPrimitives[mesh, 0] /. Point -> Sequence;

{minval, rules} = NMinimize[{-Log[Det[a]], {a\!\(\*UnderscriptBox[\(\[VectorGreaterEqual]\), 
TemplateBox[{},"SemidefiniteConeString"]]\) 0, Table[Norm[a.xi + b] <= 1, {xi, points}]}}, {a, b \[Element] Vectors[n]}];
   
ainv = Inverse[a /. rules];
center = -ainv.(b /. rules);
\[CapitalSigma] = ainv.ainv;
RegionMeasure[ellipsoidLJ = Ellipsoid[center, \[CapitalSigma]]]

Graphics3D[{PointSize[0.01], Black, Point[points], Opacity[.2], Blue, ellipsoidLJ}, Axes -> True, BoxRatios -> {1, 1, 1}, 
ImageSize -> Medium]
 
{U,\[Lambda],U2} = SingularValueDecomposition[\[CapitalSigma]];
{Sqrt@Diagonal[\[Lambda]],Sqrt@Eigensystem[\[CapitalSigma]][[1]]}
