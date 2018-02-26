f = method()
f List := costList ->( 
costMatrix = matrix(costList);
costMatrix_{0..#costList#0-2}|id_(RR^#costList)|costMatrix_{#costList#0-1}); --Quick way to add slacks. We can make z=1, instead of -1 and negate the other coefficients to balance it.

//Test//--These are random lists just to test the adding of slacks. These won't be used for the actual simplex method.
f{{2,3,4},{5,6,7},{2,3,1}}
f{{2,3,8},{4,5,7}}
f{{5,6,7},{1,3,8},{9,1,5},{4,12,15}}
f{{10,5,20,8},{66,3,12,32},{82,100,34,73}}
f{{1,4,6,8,6},{17,15,57,88,45},{96,85,25,36,0}}
M = id_(RR^3)
viewHelp mutableMatrix
L = {{2,3,4},{5,6,7},{2,3,1}}
l = last L
l = -1*l
L = drop(L, -1)|{l}
M = mutableMatrix L
M = matrix rowMult(M,numRows(M)-1,-1)
