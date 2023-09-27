(* ::Package:: *)

(*sample the desired final concentration vector*)
sampleConcentrations[
  nSamples_:96,
  minTotalAmphiphileConc_:0.1, maxTotalAmphiphileConc_:50.,
  cMax_:{50,50,50,50,50,15,10}/10. (*remember; max 20 uL in final 200 uL*)
  ]:=With[
	(*define the allowed region satisfying the constraints*)
	{region =ImplicitRegion[
		minTotalAmphiphileConc<=c1+c2+c3+c4+c5+c6+c7<=maxTotalAmphiphileConc,
		{{c1,0,cMax[[1]]},{c2,0,cMax[[2]]},{c3,0,cMax[[3]]},
		 {c4,0,cMax[[4]]},{c5,0,cMax[[5]]},{c6,0,cMax[[6]]},{c7,0,cMax[[7]]}}]},
	(*sample uniformly in the allowed region*)
	RandomPoint[region, nSamples]
]

(*compute a single dispense volume*)
computeVolume[
  ci_?NumericQ,
  stocks_:{50, 10, 2},(*available stock solutions in mM *)
  maxMinVol_:{{4,20},{4,20},{0,20}},(*allowed dispense volumes range in uL*)
  finalVol_:200 (* final volume in uL *)
  ]:=Module[{proposedVolumes,allowedStocks},
	proposedVolumes = ci*finalVol/stocks;
	allowedStocks=MapThread[ Boole@Between[#1]@#2&, {maxMinVol,proposedVolumes}];
	allowedStocks=Enclose@ConfirmBy[allowedStocks, Total[#]==1&]; (*only one dispense*)
	allowedStocks*proposedVolumes (*return dispense quantity*)
]

(*compute dispense volumes for the whole sample*)
computeVolume[
  c_?VectorQ,
  stocks_:{{50,10,2},{50,10,2},{50,10,2},{50,10,2},{50,10,2},{15,3},{10,2}},
  maxMinVol_:{{{4,20},{4,20},{0,20}},{{4,20},{4,20},{0,20}},{{4,20},{4,20},{0,20}},{{4,20},{4,20},{0,20}},{{4,20},{4,20},{0,20}},{{4,20},{0,20}},{{4,20},{0,20}}},
  finalVol_:200]:=
	MapThread[ computeVolume[#1,#2,#3,finalVol]&, {c, stocks, maxMinVol}]//Flatten
	
(* discretize volumes to an interval *)
discretizeVolume[increment_][values_]:=Round[values, increment]

