(* for detecting spots at subpixel resolution *)

segmentImage[image_Image, LoGkernel_, thresh_] := MorphologicalComponents[
   FillingTransform@MorphologicalBinarize[ColorNegate@ImageAdjust@LaplacianGaussianFilter[image, LoGkernel], thresh]
   ];

modelFit[image_, mask_, shape_, box_] := Block[{pixelpos, pixelval, img, data, data3D, a, b, mx, my, sx, sy, x, y, fm, loc},
   pixelpos = mask["NonzeroPositions"];
   pixelval = PixelValue[image, pixelpos];
   img = ReplacePixelValue[shape, Thread[PixelValuePositions[shape, 1] -> pixelval]];
   data = ImageData@ImagePad[img, 2];
   data3D = Flatten[MapIndexed[{#2[[1]], #2[[2]], #1} &, data, {2}], 1];
   fm = NonlinearModelFit[data3D, a E^(-(((-my + y) Cos[b] - (-mx + x) Sin[b])^2/(2 sy^2)) - ((-mx + x) Cos[b] + (-my + y) Sin[
               b])^2/(2 sx^2)), {a, b, mx, my, sx, sy}, {x, y}];
   {a, b, mx, my, sx, sy} = {a, b, mx, my, sx, sy} /. fm["BestFitParameters"];
   loc = Mean /@ Transpose@box + {mx, my} - (Dimensions@data) / 2.0
   ];

subPixelLocalization[image_Image, LoGkernel_: 2, thresh_: 0.75] := Block[{segmentedImage, masks, boundingboxes, shapes, pts, img},
  segmentedImage = segmentImage[image, LoGkernel, thresh];
  {masks, shapes, boundingboxes} = Values@ComponentMeasurements[segmentedImage, {"Mask", "Shape", "BoundingBox"}]\[Transpose];
  pts = MapThread[modelFit[image, ##] &, {masks, shapes, boundingboxes}];
  (* HighlightImage[image, {PointSize[0.008], Point@pts}] *)
  ]
