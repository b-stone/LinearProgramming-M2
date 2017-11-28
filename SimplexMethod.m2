-- Write function contract!!
myMethod = method()
myMethod(Matrix,Matrix) := (C,X) -> (
    C=sub(C,RR);
    A=submatrix(X,{0..numgens target X-1});
    A=sub(A,RR);
    B=X_{numgens source X-1};
    B=sub(B,RR);
    AA=A^(-1);
    
    return{AA*B, C*AA*B};
    )

-- Write function contract!!
myOptsMethod = method(Options => {MyOption => true}) -- you don't always need an option
myOptsMethod(Matrix,Matrix) := opts -> (C,X) -> (
    
    if opts.MyOption then return "It was True!" else return "It was not true.";
    
    )

end

restart
path = {"~/GitHub/LinearProgramming-M2/"}|path
load"SimplexMethod.m2"

C=matrix{{1,1}}
X=matrix{{1,2,1,0,6},{2,1,0,1,6}};
myMethod(C,X)
myOptsMethod(C,X)
myOptsMethod(C,X,MyOption => false)

C=matrix{{1,-2,3}}
C=sub(C,QQ)
X=matrix{{}}
Simplex=method()--Makes a method
Simplex(Number):= x->(
    for y from 1 to numgens target C-1 do ( --Needs to be fixed. Syntax issues: Makes y numbers 0 to number of rows
    	if C_(y,x)/(C_(y,numgens source C-1))<C_(y-1,x)/(C_(y-1,numgens source C-1))then return y;--Obtains ratio between solution and coefficient of a variable
	)
)	
for i from 0 to numgens source C-1 do (--Syntax problems: Makes i numbers 0 to number of columns  
    if C_(0,i)<0 then Y_i = Simplex(i);--Syntax; Runs the method Simplex with number i.
    print i;
    )
peek Y
Simplex 2
oo
for i from 0 to 2 do print C_(0,i);
v = flatten entries X^{0} -- makes row 0 into a list
position(v,l -> l==2) -- finds the first occurence of equal to 2
X_1 -- column 
