C=matrix{{1,1}}
C=sub(C,RR)
X=matrix{{1,2,1,0,6},{2,1,0,1,6}}
A=submatrix(X,{0..numgens target X-1})
A=sub(A,RR)
B=X_{numgens source X-1}
B=sub(B,RR)
AA=A^(-1)
AA*B
C*AA*B