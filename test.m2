<<<<<<< HEAD
lpSimplex = method()
lpSimplex(Matrix) := matrix1 -> (
    -- hi there
  matrix1 = sub(matrix1, RR);
 matrix1 = mutableMatrix(matrix1);
=======
<<<<<<< HEAD
restart
matrix1 =mutableMatrix{{1/1,3,2,1,0,0,10},{1,5,1,0,1,0,8},{-8,-10,-7,0,0,1,0}} -- had to make a number rational so it stays in the rationals ring
=======
lex = method()
lex(Matrix) := matrix1 -> (
>>>>>>> bfd7fef451057e20adf2a53ad29c8583c7311911
    
 numberofRows = numRows(matrix1);
isThereANeg=true;
while isThereANeg 
do(
    lastrow=matrix1^{numberofRows-1};
    listoflast=flatten(entries(lastrow));
    isThereANeg=false;
    for i from 0 to #listoflast-1 
    do(if listoflast#i<0 then isThereANeg=true);
    if isThereANeg==false then break;
smallest=min(listoflast);
colnum=position(listoflast,i-> i == smallest);
pivcol=(matrix(matrix1))_(colnum);
listofpivotcol=flatten(entries(pivcol));
listofpivotcol=remove(listofpivotcol,length(listofpivotcol)-1);
listoflastcol=flatten(entries((matrix(matrix1))_(numColumns(matrix1)-1)));
listoflastcol=remove(listoflastcol,length(listoflastcol)-1);
listofdividends=apply(listoflastcol,listofpivotcol,(i,j)->i/j);
smallestrow=min(listofdividends);
rownum=position(listofdividends,i->i==smallestrow);
matrix1=rowMult(matrix1,rownum,(1/(listofpivotcol#rownum)));
listofpivotcol=flatten(entries((matrix(matrix1))_(colnum)));
for i from 0 to #listofpivotcol-1 do (if listofpivotcol#i!=1 or 
    listofpivotcol#i!=0 then rowAdd(matrix1,i,-listofpivotcol#i,rownum));
);
    
    
    
   return {matrix(matrix1), matrix1_(numRows(matrix1)-1,numColumns(matrix1)-1)} ;
    )
end


restart 
load"test.m2" 

<<<<<<< HEAD
M =matrix{{1,3,2,1,0,0,10},{1,5,1,0,1,0,8},{-8,-10,-7,0,0,1,0}} -- had to make a number rational so it stays in the rationals ring
lpSimplex M
=======
M =mutableMatrix{{1/1,3,2,1,0,0,10},{1,5,1,0,1,0,8},{-8,-10,-7,0,0,1,0}} -- had to make a number rational so it stays in the rationals ring
lex M

>>>>>>> 6d49419d6cb900863d37ab8b1ab33aa2c8b8fefe
numberofRows = numRows(matrix1)
isThereANeg=true
while isThereANeg do(
lastrow=matrix1^{numberofRows-1};
listoflast=flatten(entries(lastrow));
isThereANeg=false;
for i from 0 to #listoflast-1 do(if listoflast#i<0 then isThereANeg=true);
if isThereANeg==false then break;
smallest=min(listoflast);
colnum=position(listoflast,i-> i == smallest);
pivcol=(matrix(matrix1))_(colnum);
listofpivotcol=flatten(entries(pivcol));
listofpivotcol=remove(listofpivotcol,length(listofpivotcol)-1);
listoflastcol=flatten(entries((matrix(matrix1))_(numColumns(matrix1)-1)));
listoflastcol=remove(listoflastcol,length(listoflastcol)-1);
listofdividends=apply(listoflastcol,listofpivotcol,(i,j)->i/j);
smallestrow=min(listofdividends);
rownum=position(listofdividends,i->i==smallestrow);
matrix1=rowMult(matrix1,rownum,(1/(listofpivotcol#rownum)));
listofpivotcol=flatten(entries((matrix(matrix1))_(colnum)));
for i from 0 to #listofpivotcol-1 do (if listofpivotcol#i!=1 or 
    listofpivotcol#i!=0 then rowAdd(matrix1,i,-listofpivotcol#i,rownum));
)
>>>>>>> bfd7fef451057e20adf2a53ad29c8583c7311911


