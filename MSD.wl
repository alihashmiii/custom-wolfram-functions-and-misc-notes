Clear@MSD;
MSD[pos_, t_: 50] := With[{timesteps = t},
   Module[{transposedlist, length},
    transposedlist = Table[
      Thread[{pos, RotateLeft[pos, i]}][[;; -(i + 1)]], {i,timesteps}];
    length = (Dimensions /@ transposedlist);
    Mean /@ Table[
      Module[{listreshape, lr},
       listreshape = ArrayReshape[transposedlist[[i]], length[[i]]];
       lr = Map[Function[x, Power[#, 2] &@(EuclideanDistance @@ Reverse@x)], listreshape[[1 ;; All]]]
      ], {i, timesteps}]
     ]
   ];
