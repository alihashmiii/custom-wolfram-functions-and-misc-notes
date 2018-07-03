Clear@bondOrientationalOrder;
bondOrientationalOrder[pts_?(Length@# > 1 &)] := Block[{delMesh, vertexcoords, vertexconn, cellNeighCoords, angles,
    anglesC, poly, pos, polyOrdered, colourVM, regMemQ},
   delMesh = DelaunayMesh@pts;
   vertexcoords = <| delMesh["VertexCoordinateRules"] |>;
   vertexconn = delMesh["VertexVertexConnectivityRules"];
   cellNeighCoords = With[{vertexpts = vertexcoords},
     FlattenAt[{Lookup[vertexpts, Keys@#], Lookup[vertexpts, Values@#]}, {2}] & /@ vertexconn
     ];
   angles = (Abs[(Plus @@ Exp[6.0 I #])/Length@#]) &/@ Map[Map[x \[Function] VectorAngle[{1, 0}, First[#] - x ], 
        Rest@#] &, cellNeighCoords];
   anglesC = ColorData["TemperatureMap", #] & /@ angles;
   poly = MeshPrimitives[VoronoiMesh[pts, None], 2];
   regMemQ = RegionMember /@ poly;
   pos = Position[Through[regMemQ[#]], True] & /@ pts;
   polyOrdered = Extract[poly, pos];
   colourVM = MapThread[{#2, #1} &, {Thread[{EdgeForm[Black], polyOrdered}], anglesC}];
   Graphics[{colourVM, Point@pts}, PlotRange -> {{0, 550}, {0, 1024}}, ImageSize -> Large]
   ];
