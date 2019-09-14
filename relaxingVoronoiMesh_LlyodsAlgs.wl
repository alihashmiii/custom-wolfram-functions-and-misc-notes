(* works on 3D as well *)

nIter = 100; npts = 1000;
vmesh = VoronoiMesh[RandomReal[{-1, 1}, {npts, 2}]];
Do[pts = PropertyValue[{vmesh, 2}, MeshCellCentroid];
vmesh = VoronoiMesh[pts, {{-1, 1}, {-1, 1}}] , nIter];

