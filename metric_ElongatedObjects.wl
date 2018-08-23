elongationMetric[mask_] := Block[{\[CapitalPi], ē, \[Eta], \[Alpha], \[Epsilon]},
   \[CapitalPi] = First @@Values@ComponentMeasurements[MorphologicalPerimeter[mask], "PerimeterPositions"];
   ē = Partition[\[CapitalPi], 2, 1, 1];
   \[Eta] = Apply[EuclideanDistance] /@ ē;
   \[Alpha] = VectorAngle[Last@# - First@#, {1, 0}] & /@ ē;
   \[Epsilon] = (Total@\[Eta] + Sqrt[((\[Eta].Cos[2 \[Alpha]])^2) + ((\[Eta].Sin[
         2 \[Alpha]])^2) ])/(Total@\[Eta] - Sqrt[((\[Eta]. Cos[2 \[Alpha]])^2) + ((\[Eta]. 
        Sin[2 \[Alpha]])^2)])
   ];
