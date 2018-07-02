Clear@bondOrientationalOrder;
bondOrientationalOrder[pts_?(Length@# > 1 &)] :=  Block[{delMesh, vertexcoords, vertexconn, cellNeighCoords, angles,
    anglesC, poly, rules, q, regionmember, colourVM, keys, regMemQ, pos, polyOrdered, ptsinfaces, polyOrderedfiltered},
   delMesh = DelaunayMesh@pts;
   vertexcoords = <| delMesh["VertexCoordinateRules"] |>;
   vertexconn = delMesh["VertexVertexConnectivityRules"];
   cellNeighCoords = With[{vertexpts = vertexcoords},
     FlattenAt[{Lookup[vertexpts, Keys@#], Lookup[vertexpts, Values@#]}, {2}] & /@ vertexconn
     ];
   angles = (Abs[(Plus @@ Exp[6.0 I #])/Length@#])&/@ Map[Map[x \[Function] VectorAngle[{1, 0}, First[#] - x ], 
        Rest@#] &, cellNeighCoords];
   anglesC = ColorData["TemperatureMap", #] & /@ angles;
   poly = MeshPrimitives[VoronoiMesh@pts, 2];
   rules = MapIndexed[First@#2 -> #1 &, VoronoiMesh[pts]["PointInFaces"]];
   q = Lookup[rules, VoronoiMesh[pts]["BoundaryFaces"]];
   regMemQ = RegionMember /@ poly;
   pos = Position[Through[regMemQ[#]], True] & /@ pts;
   polyOrdered = Extract[poly, pos];
   regionmember = MapIndexed[First@#2 -> RegionMember@#1 &, Flatten[polyOrdered, 1]];
   keys = Partition[Sort[First @@@ Function[x, Select[regionmember, Last[#]@x &]] /@ q], 1];
   polyOrderedfiltered = Delete[polyOrdered, keys];
   colourVM = MapThread[{#2, #1} &, {Thread[{EdgeForm[Black], polyOrderedfiltered}], Delete[anglesC, keys]}];
   ptsinfaces = Flatten[Function[x, Select[pts, x@# &]] /@ (RegionMember/@Flatten[polyOrderedfiltered, 1]), 1];
   Graphics[{colourVM, Point@ptsinfaces}, PlotRange -> {{0, 550}, {0, 1024}}, ImageSize -> Large, Frame -> True]
   ];
