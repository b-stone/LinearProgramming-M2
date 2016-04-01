lex = method()
lex(Matrix) := matrix1 -> (
    
    
    
    
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
    
    
    
   return matrix1; 
    )
end


restart 
path= path|{"home/sarthak/LinearProgramming/","/sarthak/LinearProgramming/"}
load"test.m2" 

M =mutableMatrix{{1/1,3,2,1,0,0,10},{1,5,1,0,1,0,8},{-8,-10,-7,0,0,1,0}} -- had to make a number rational so it stays in the rationals ring
lex M

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
matrix1


