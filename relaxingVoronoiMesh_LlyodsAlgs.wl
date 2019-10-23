(* works on 3D as well *)

nIter = 100; npts = 1000;
vmesh = VoronoiMesh[RandomReal[{-1, 1}, {npts, 2}]];
Do[pts = PropertyValue[{vmesh, 2}, MeshCellCentroid];
vmesh = VoronoiMesh[pts, {{-1, 1}, {-1, 1}}] , nIter];

________________________________________________________________________________________________________________________________
(*
3D voronoi - from Chip Hurst -- A builtin version will be incorporated in the language soon. The version below is
not thoroughly tested
*)

pad[\[Delta]_][{min_, max_}] := {min,max} + \[Delta] (max - min) {-1, 1}

VoronoiCells[pts_] /; 
  MatrixQ[pts, NumericQ] && 2 <= Last[Dimensions[pts]] <= 3 := 
 Block[{bds, dm, conn, adj, lc, pc, cpts, hpts, hns, hp, vcells}, 
  bds = pad[.1] /@ MinMax /@ Transpose[pts];
  dm = DelaunayMesh[pts];
  conn = dm["ConnectivityMatrix"[0, 1]];
  adj = conn.Transpose[conn];
  lc = conn["MatrixColumns"];
  pc = adj["MatrixColumns"];
  cpts = MeshCoordinates[dm];
  vcells = 
   Table[hpts = PropertyValue[{dm, {1, lc[[i]]}}, MeshCellCentroid];
    hns = 
     Transpose[Transpose[cpts[[DeleteCases[pc[[i]], i]]]] - cpts[[i]]];
    hp = MapThread[HalfSpace, {hns, hpts}];
    BoundaryDiscretizeGraphics[#, PlotRange -> bds] & /@ hp, {i, 
     MeshCellCount[dm, 0]}];
  AssociationThread[cpts, RegionIntersection @@@ vcells]];
  
SeedRandom[10000];
pts = RandomReal[{-1, 1}, {100, 3}];
vc = VoronoiCells[pts];

Do[vc = Values[vc];
  pts = Map[First@PropertyValue[{#, 3}, MeshCellCentroid] &, vc];
  vc = VoronoiCells[pts],
  5];
  
  Show@MapIndexed[BoundaryMeshRegion[#,MeshCellStyle->{1->{Black,Thick},2->{Opacity[0.2],ColorData[112][First[#2]]}}]&,Values[vc]]
