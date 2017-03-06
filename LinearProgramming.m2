---------------------------------------------------------------------------
-- PURPOSE : LinearPrograming is a package for Macaulay2 and is
-- an implementation of various linear programing methods.
--
-- Copyright (C) 2016 Branden Stone and Tom Enkosky
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License version 2
-- as published by the Free Software Foundation.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--------------------------------------------------------------------------


newPackage(
	"LinearProgramming",
    	Version => "0.1", 
    	Date => "March 25, 2016",
    	Authors => {       
	     {Name => "Branden Stone", Email => "bstone@adelphi.edu", HomePage => "http://math.adelpi.edu/~bstone/"},
     	     {Name => "Thomas Enkosky", Email => "tenkosky@gmail.com"},
	     {Name => "Harmit Minhas", Email => "harmitminhas@mail.adelphi.edu"}	     
	     },
    	Headline => "LinearProgramming",
    	DebuggingMode => false
--	PackageExports => {"Graphs", "Posets", "SimplicialComplexes"},
--	AuxiliaryFiles => true,
--	Configuration => {"DefaultPath" => null } 
    	)

export {
    
    -- Options
     "Optimize",
     "Max",
     "Min",
     "Slack",
    
    -- Methods
     "simplexProc",
     "getMaxCoordinates",
     "getMinCoordinates",
     "rref",
     "simplexMethod",
     "reduceAtPivot",
     "addSlack"
}


------------------------------------------------------------
-- Global Variables
------------------------------------------------------------




------------------------------------------------------------
-- METHODS
------------------------------------------------------------


-- Input:  Matrix 

-- Output: Matrix 

-- Description:
-- Given a matrix that is in the order (restraint functions coefficients|slack variables for restraints|Constants)
--    	      	      	      	       (cost function coeeficients      |slack variable for cost       |   0     )
-- This method applies the simplex method to that matrix
--
-- The simplex method selects the largest(or smallest) entry in the last row.
-- The column with that entry is the pivot column.
-- Then take the list of entries in last column/entry in pivot column.
-- The row with the min ratio is the pivot row.

simplexProc=method()
simplexProc(Matrix) :=  matrix1  -> (
    local lastrow;
    local listofpivotcol;
    local listoflastcol;
    local rownum;
    local matrix1;
    local biggest;
    local listofdividends;
    local colnum;

    --Forces the matrix to be in the reals
    matrix1=sub(matrix1,RR);	   
    --numberofRows = numRows(matrix1);
    	
    -- lastrow is the cost function
    lastrow=flatten(entries(matrix1^{numRows(matrix1)-1}));

    -- Get biggest entry in cost function.  
    biggest=max(lastrow);

    -- if there are no negatives in the list, then we are done
    while biggest > 0 do(

    	-- the index of the pivot column
    	colnum=position(lastrow,i-> i == biggest);
 
        -- remove last entry of pivot column (it's from the cost function)
    	listofpivotcol=flatten(entries((matrix1)_(colnum)));
     	listofpivotcol=remove(listofpivotcol,length(listofpivotcol)-1);	   

    	-- remove last entry of the last column
        listoflastcol=flatten(entries((matrix1)_(numColumns(matrix1)-1)));   
        listoflastcol=remove(listoflastcol,length(listoflastcol)-1);
    
        -- the ratio of entries in last column/entries in pivot column
    	listofdividends=apply(listoflastcol,listofpivotcol,(i,j)->(
		if j==0 then infinity else i/j));	   

       	-- This is the row we select for our row operations
    	rownum=position(listofdividends,i->i==min(listofdividends));    

    	-- row reduce at this pivot
        matrix1=reduceAtPivot(matrix1,rownum,colnum);

        -- Find the new biggest entry in the last row
        lastrow=flatten(entries(matrix1^{numRows(matrix1)-1}));
    	biggest=max(lastrow);
    );
return matrix1;
)




--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- Input:  Matrix
-- Output: List 
-- Description: 
-- Given a matrix that had the simplex method applied to it
-- for maximization, this will return the coefficients of the
-- cost function, in order to maxmize the cost value.
getMaxCoordinates=method()
getMaxCoordinates(Matrix):= matrix1 -> (
    local count; 
    local numOfVars;
    local listofcol;
    local rowpos;
    local coordinates;
    local listoflastcol; 

    -- This is the last column of the matrix
   listoflastcol=flatten(entries(matrix1_(numColumns(matrix1)-1)));

    -- This list will be the coordinates of the solution
   coordinates=new BasicList;

   -- Figure out the number of variables not including the slacks
   numOfVars=numColumns(matrix1)-numRows(matrix1)-1;    
   
   -- If a column for the variable has more than one coefficient for it, 
   -- that variable is set to 0, otherwise it is given the value in the r
   -- espective row
   
   -- The variable i corresponds to the columns with original variables 
   for i from 0 to numOfVars-1 do(
       
       -- count is the number of nonzero entries in column i
       count=0;
       
       -- this is column i
       listofcol=flatten(entries(matrix1_i));

    	-- The variable j is the index of the row
       for j from 0 to #listoflastcol-1 do(
	   -- if there's a nonzero entry increase count
	   if listofcol#j!=0 then count=count+1);
       	   
	   -- If there is one nonzero entry in a column,
	   -- the corresponding variable should have the value
	   -- in the last column of its row
           if count==1 then (
	       
	       -- get the row of the nonzero entry
    	       rowpos = position(listofcol,i-> i != 0);
	       
	       -- add the value of x_i to the list of the solution
    	       coordinates=append(coordinates,listoflastcol#rowpos);
	   );
    
    -- If there is not exactly one nonzero entry in column i,
    -- then variable x_i=0.   
    if count!=1 then coordinates=append(coordinates,0);
    );
return coordinates;
)

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- Input: Matrix
-- Output: List of numbers
-- Description: 
-- Given a matrix that had the simplex method applied to it
-- for minimizationn, this will return the coefficients of the
-- cost function, in order to minimize the cost value.
--
-- There's no way this works.  it looks like it's for a specific example
getMinCoordinates=method()
getMinCoordinates(MutableMatrix):= matrix1 ->(
local coordinates; local numOfVars; local lastrow; local loopstop;

coordinates=new BasicList;
numOfVars=numColumns(matrix1)-numRows(matrix1)-1;    --Figures out the number of variables, not including slacks
lastrow=flatten(entries(matrix1^{numRows(matrix1)-1}));	   
loopstop=numColumns(matrix1)-3;	   
for i from numOfVars to loopstop do(coordinates=append(coordinates,lastrow#i); );    --The numbers at these locations are the coordinates for minimizing the cost
return coordinates;
)

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- Input:  Matrix

-- Output:  Matrix

-- Description: 
-- This method applies Gauss-Jordan focused on the pivot at
-- location (rowi,colj)

reduceAtPivot=method()
reduceAtPivot(Matrix,ZZ,ZZ) :=  (matrix1,rowi,colj)  -> (
    local matrix1;
    local rowi;
    local colj;
    local selectedCol;  
        
    	-- convert matrix to have real entries
        matrix1=sub(matrix1,RR);	   
    
        -- This is the column of index colj
    	selectedCol=flatten(entries((matrix matrix1)_(colj)));

    	-- Make sure the rowi,colj entry is not 0
    	if selectedCol#rowi == 0 then error "there is a 0 at this pivot";
	
    	-- Multiply rowi by 1/(rowi,colj) entry
	matrix1=rowMult(mutableMatrix(matrix1),rowi,(1/(selectedCol#rowi)));
    	
        -- Reduce other rows around the pivotcolumn
    	for i from 0 to #selectedCol-1 do (
	    if selectedCol#i!=1 or 
    	    selectedCol#i!=0 then rowAdd(matrix1,i,-selectedCol#i,rowi));
	
	-- convert back to matrix
	matrix1=matrix(matrix1);

return matrix1;
)
  
--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- Input: Mutable Matrix

-- Output: Mutable Matrix

-- Description: 
-- This method will put the given matrix
-- into row reduced echelon form.
rref=method()
rref(Matrix) :=  matrix2  -> (
    local count;
    local row;
    local matrix2;
    local changerow;    
    
count=numRows(matrix2)-1;
for j from 0 to count do(
    row=flatten(entries(matrix2^{j}));
    if row#j != 0 then(
    matrix2=rowMult(matrix2,j,1/row#j);	   --Divideds the row by the pivot value
    row=flatten(entries(matrix2^{j}));	  
    for i from 0 to count do(
	if j!=i then(
	    changerow=flatten(entries(matrix2^{i}));	--Gets the next row that needs to be reduced around the pivot
	    matrix2=rowAdd(matrix2,i,-changerow#j/row#j,j);
	   );
	);
    );
);
return matrix2;
)
  

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- Input: A Matrix

-- Output: A Matrix

-- Description: 
-- This adds slack variables to the matrix

-- Additional/Necessary Information:
-- The matrix should have the order: restraints followed by cost function.
-- A list inside the list should be: coefficient 1, coefficient 2, etc, restraint constant. 
-- Cost functions should be set to 0.
-- All the variables should be restrained by 0. 
-- Restraint functions should be set to be greater than or equal to a constant for minimization.
-- Restraint functions should be set to be less than or equal to a constant for maximization.

--addSlack=method()
--addSlack(Matrix) := matrix1  -> (   
  --  local newList; 
  --  local tempList; 
  --  local tempElement; 
  --  local indexLastRow; 
  --  local matrix1; 
  --  local list1; 
  --  local matrix1;
     
--    newList=new List;

--   indexLastRow=#entries(matrix1)-1;
	    
    -- i ranges over the rows of the matrix
--   for i from 0 to indexLastRow do(
	
 	-- get row i of the matrix
--    	tempList=flatten(entries(matrix1^{i}));   
	
	-- temporarily remove the constant at the end
--     	tempElement=tempList#(#tempList-1);    
--     	tempList=remove(tempList,#tempList-1);
	
	-- j ranges over the number of rows
--     	for j from 0 to indexLastRow  do(
	    
	    -- cost function gets -1 added
--	    if j==indexLastRow and i==j then tempList=append(tempList,-1);
	    
	    -- row i gets 1 added i steps to right, otherwise 0 added
--	    if j==i and j!=indexLastRow then tempList= append(tempList,1);
--	    if j!=i then tempList= append(tempList,0);
--	    );
--       	tempList=append(tempList,tempElement);
--       	newList=append(newList,tempList);
--       	);

--return matrix newList;
--)

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- Input: A Matrix

-- Output: A Matrix

-- Description: 
-- This adds slack variables to the matrix

-- Additional/Necessary Information:
-- The matrix should have the order: restraints followed by cost function.
-- The input matrix should have a preface column to determine less than or greater
-- than for the constraint fucntions -1 for less than and 1 for greater than.
-- For example take the set of equations
--x+2y<5
--2x+3y>4
--z=x+y
--would correspond to the matrix -1 0 1 2 5
--                                1 0 2 3 4
--                                0 1 -1 -1 0

addSlack=method()
addSlack(Matrix) := matrix1  -> (   
--    local newList; 
    local tempList; 
    local tempElement; 
    local prefaceRow;
    local indexLastRow;
    local indexFirstRow; 
    local matrix1; 
    local list1; 
    local matrix1;
    local constants;
    
     
    newList=new List;

    indexLastRow=#entries(matrix1)-1;
    indexFirstRow=0;
   
  --Takes out the first row that cooresponds to < / >
    prefaceRow = flatten(matrix1_[indexFirstRow]);
  --Removes it
    remove(matrix1_[indexFirstRow]);
           

    contrants=flatten(matrix1_[indexLastRow]);
    remove(matrix1_[indexLastRow]);
	-- j ranges over the number of rows
       	for j from 0 to indexLastRow  do(
	    
	    
	    -- cost function gets -1 added
	    if j==indexLastRow and i==j then tempList=append(tempList,-1);
	    
	    -- row i gets 1 added i steps to right, otherwise 0 added
	    if j==i and j!=indexLastRow then tempList= append(tempList,1);
	    if j!=i then tempList= append(tempList,0);
	    );
       	tempList=append(tempList,tempElement);
       	newList=append(newList,tempList);
       	);

return matrix newList;
)




--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- Input: A Matrix

-- Output: A Matrix, List of Numbers (The values of the variables to optimize), and A Number (The max/min cost value)

-- Description: 
-- This applies the simplex method to a given list of lists. It will minimize or maximize, depending
-- on what the user opts to do.

-- Additional/Necessary Information:
-- The list of list should have the order: restraints followed by cost function.
-- A list inside the list should be: coefficient 1, coefficient 2, etc, restraint constant. 
-- Cost functions should be set to 0.
-- All the variables should be restrained by 0. 
-- Restraint functions should be set to be greater than or equal to a constant for minimization.
-- Restraint functions should be set to be less than or equal to a constant for maximization.

simplexMethod=method(Options=> {Optimize=>Max,Slack=>false})
simplexMethod(Matrix) := opts-> matrix1  -> (   
    local newList; 
    local tempList; 
    local tempElement; 
    local count; 
    local matrix1; 
    local coordinates; 
    local list1; 
    local optimizedCost;
    local matrix1;
    
     -- To minimize, take transpose
    if opts.Optimize==Min then(matrix1 = transpose(matrix1););
    if opts.Slack==false then matrix1=addSlack(matrix1);    
    
    --matrix1=matrix(rowMult(mutableMatrix(matrix1),numRows(matrix1)-1,-1));
    matrix1=simplexProc(matrix1);
    
    --Coordinates are found depending on goal of our optimiziation
    if opts.Optimize==Max then coordinates = getMaxCoordinates(matrix1);
    if opts.Optimize==Min then coordinates = getMinCoordinates(matrix1);
    optimizedCost=matrix1_(numRows(matrix1)-1,numColumns(matrix1)-1);
    return {matrix1,coordinates,optimizedCost};
 )



--------------------------------------------------
-- DOCUMENTATION
--------------------------------------------------





-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------

end

-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------
-------------------------------------------------------------------------------------------

-----------------------------
-----------------------------
-- Tests
-----------------------------
-----------------------------


-- harmit

restart
loadPackage "LinearProgramming"

--Sample maximization problems
maxSample = {{1,3,2,10},{1,5,1,8},{8,10,7,0}}
maxSample = {{2,1,1,14},{4,2,3,28},{2,5,5,30},{1,2,-1,0}}

maxSample = matrix({{1,4,5,2,1},{3,1,5,2,6},{4,2,-3,-3,0}})
simplexProc(maxSample)

simplexMethod(maxSample,Optimize=>Max)


--Sample minimization problem
minSample = {{3,2,2},{5,1,3},{29,10,0}}
minSample = {{60,60,300},{12,6,36},{10,30,90},{.12,.15,0}}

simplexMethod(minSample,Optimize=>Min)


--Sample rref problems
matrix2=mutableMatrix(sub(matrix{{1,1,1,-1},{1,2,4,3},{1,3,9,3}},QQ))
matrix2=mutableMatrix(sub(matrix{{1,2,1},{-2,-3,1},{3,5,0}},QQ))
	    
rref(matrix2)
rank matrix2	    




-- tom

restart
loadPackage"LinearProgramming"
M = matrix {{2,3,1,1,0,0,0,5},{4,1,2,0,1,0,0,11},{3,4,2,0,0,1,0,8},{5,4,3,0,0,0,-1,0}}
addSlack M
simplexMethod (M,Slack=>true)

MyTest = matrix {{2,3,1,5},{4,1,2,11},{3,4,2,8},{5,4,3,0}}
addSlack MyTest
simplexMethod MyTest

minSample = matrix {{3,2,2},{5,1,3},{29,10,0}}
addSlack minSample

minSample
simplexMethod minSample

anotherExample = matrix {{3,2,2},{5,1,3},{29,10,0},{1,2,4},{1,1,1},{4,4,4}}
addSlack anotherExample
entries(M)
#entries(M)

flatten entries M^{1}

Mess = matrix {{0,2,0,1,1,0,0,5},{0,4,0,2,0,1,0,11},{0,3,4,2,0,0,1,8},{1,-5,-4,-3,0,0,0,0}}
N = simplexProc M
Ness = simplexProc Mess
getMaxCoordinates N
getMaxCoordinates Ness
reduceAtPivot(M,1,3)
simplexMethod M

flatten M
entries M
transpose M
simplexMethod entries M
-- tom