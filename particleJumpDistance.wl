
(* The script below determines the per frame jump distance for particle/spot tracking in high density regimes. The functions were
incorporated in SMTrack i.e. (SingleMolecule)Track *)

(* mean separation between detected particles *)
meanParticleDist[centroids_]:= Module[{vertices,dist},
 vertices = MeshPrimitives[DelaunayMesh@centroids, 1]/.Line[x_]:> x;
 dist = Map[EuclideanDistance@@#&,vertices];
 {Mean@#,StandardDeviation@#}&@dist
];


maxJumpDistance[centPrev_,centNew_,distDelaunay_]:= Module[{nearestFunc,rec,pts,\[ScriptCapitalA]},
 nearestFunc = Nearest@centPrev;
 rec = Flatten[Table[{i, Length@nearestFunc[#,{All, i}]},{i, 0, distDelaunay, 1}]&/@centNew, 1];
 pts = Cases[rec, {_,_?(# <= 1 &)}];
 \[ScriptCapitalA] = WeightedData[Keys@#,Exp[Values@#]]&@Counts[Part[pts,All,1]];
 N@*Mean@\[ScriptCapitalA]
];(* where distDelaunay is the meanParticleDist@centroids *)
