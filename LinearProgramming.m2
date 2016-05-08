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
    
    -- Methods
     "SimplexProc",
     "getMaxCoordinates",
     "getMinCoordinates",
     "rref",
     "simplex"

}


------------------------------------------------------------
-- Global Variables
------------------------------------------------------------




------------------------------------------------------------
-- METHODS
------------------------------------------------------------


-- Input: Mutable Matrix 

-- Output: Mutable Matrix 

-- Description:
-- Given a matrix that is in the order (restraint functions coefficients|slack variables for restraints|Constants)
--    	      	      	      	       (cost function coeeficients      |slack variable for cost       |   0     )
-- This method applies the simplex method to that matrix

SimplexProc=method()
SimplexProc(Matrix) :=  matrix1  -> (
    local lastrow;
    local listofpivotcol;
    local listoflastcol;
    local rownum;
    local matrix1;
    local smallest;
    local listofdividends;
    local colnum;

    --Forces the matrix to be in the reals
    matrix1=sub(matrix1,RR);	   
    --numberofRows = numRows(matrix1);
    	
    -- initialize to smallest entry of cost function
    lastrow=flatten(entries(matrix1^{numRows(matrix1)-1}));
    smallest=min(lastrow);

    -- if there are no negatives in the list, then we are done
    while smallest < 0 do(
-- %%%%%%%%%%%%%%%%
-- This section first finds which row we apply row reduction to
	     
    	-- pivcol is the column of the matrix with the smallest entry in the last row
    	colnum=position(lastrow,i-> i == smallest);
 
        -- Comparing the pivotcolumn with the last column to see which row we reduce around
    	listofpivotcol=flatten(entries((matrix1)_(colnum)));
     	listofpivotcol=remove(listofpivotcol,length(listofpivotcol)-1);	   
        listoflastcol=flatten(entries((matrix1)_(numColumns(matrix1)-1)));   
        listoflastcol=remove(listoflastcol,length(listoflastcol)-1);
    
        -- Finding the ratios between the last columns entries and respective pivot column entries
    	listofdividends=apply(listoflastcol,listofpivotcol,(i,j)->(
		if j==0 then infinity else i/j));	   

       	-- This is the row we select for our row operations
    	rownum=position(listofdividends,i->i==min(listofdividends));    
-- %%%%%%%%%%%%%%%%%%%
 
        -- Normalize the row about the pivot
    	matrix1=rowMult(mutableMatrix(matrix1),rownum,(1/(listofpivotcol#rownum)));
    	listofpivotcol=flatten(entries((matrix matrix1)_(colnum)));

        -- Reduce other rows around the pivotcolumn
    	for i from 0 to #listofpivotcol-1 do (
	    if listofpivotcol#i!=1 or 
    	    listofpivotcol#i!=0 then rowAdd(matrix1,i,-listofpivotcol#i,rownum));
	
	matrix1=matrix(matrix1);
	
        -- Find the new smallest entry in the last row
        lastrow=flatten(entries(matrix1^{numRows(matrix1)-1}));
    	smallest=min(lastrow);
    );
return matrix1;
)




--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

-- Input:  Matrix
-- Output: List of numbers
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

   count=0; 
   listoflastcol=flatten(entries(matrix1_(numColumns(matrix1)-1)));
   coordinates=new BasicList;

   -- Figure out the number of variables not including the slacks
   numOfVars=numColumns(matrix1)-numRows(matrix1)-1;    
   
   --If a column for the variable has more than one coefficient for it, that variable is set to 0, otherwise it is given the value in the respective row
   for i from 0 to numOfVars-1 do(
       count=0;
       listofcol=flatten(entries(matrix1_i));
       for j from 0 to #listoflastcol-1 do(
	   if listofcol#j!=0 then count=count+1);
       if count==1 then (
    	   rowpos = position(listofcol,i-> i != 0);
	   listoflastcol=flatten(entries(matrix1_(numColumns(matrix1)-1)));
    	   coordinates=append(coordinates,listoflastcol#rowpos);
	);
    if count!=1 then coordinates=append(coordinates,0);
    );
return coordinates;
)

--++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


-- Input: Mutable Matrix
-- Output: List of numbers
-- Description: 
-- Given a matrix that had the simplex method applied to it
-- for minimizationn, this will return the coefficients of the
-- cost function, in order to minimize the cost value.
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

-- Input: Mutable Matrix

-- Output: Mutable Matrix

-- Description: 
-- This method will put the given matrix
-- into row reduced echelon form.
rref=method()
rref(MutableMatrix) :=  matrix2  -> (
local count;local row;local matrix2;local changerow;    
    
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


-- Input: A List of Lists and Option

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

simplex=method(Options=> {Optimize=>Max})
simplex(List) := opts-> list1  -> (   
local newList; local tempList; local tempElement; 
local count; local matrix1; local coordinates; local list1; local optimizedCost;


    
    if opts.Optimize==Min then(list1 = entries(transpose(matrix(list1))););    --If we want to minimize, we must do the dual and therefore need the transpose of the coefficients we are given
    newList=new List;
    for i from 0 to #list1-1 do(
       	tempList=list1#i;    --Gets the list we wish to add extra slack variables
       	tempElement=tempList#(#tempList-1);    --The constant for the cost function is placed after the slacks
       	tempList=remove(tempList,#tempList-1);
       	count=#list1-1;
	--Slacks are added as an identity in between the non-slack variables and their respective constant restraints
       	for j from 0 to count  do(
	    if j==count and i==j then tempList=append(tempList,-1);
	    if j==i and j!=count then tempList= append(tempList,1);
	    if j!=i then tempList= append(tempList,0);
	    );
       	tempList=append(tempList,tempElement);
       	newList=append(newList,tempList);
       	);
    
    --The simplex procedure is done on the matrix
    matrix1=matrix(newList); -- T$ change to get rid of mutableMatrix now matrix
    matrix1=rowMult(matrix1,numRows(matrix1)-1,-1);
    matrix1=SimplexProc(matrix1);
    
    --Coordinates are found depending on goal of our optimiziation
    if opts.Optimize==Max then coordinates = getMaxCoordinates(matrix1);
    if opts.Optimize==Min then coordinates = getMinCoordinates(matrix1);
    optimizedCost=matrix1_(numRows(matrix1)-1,numColumns(matrix1)-1);
    return {matrix(matrix1),coordinates,optimizedCost};
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

maxSample = matrix({{1,4,5,2,1},{3,1,5,2,6},{4,2,-3,-3,-6}})
SimplexProc(matrix maxSample)

simplex(maxSample,Optimize=>Max)


--Sample minimization problem
minSample = {{3,2,2},{5,1,3},{29,10,0}}
minSample = {{60,60,300},{12,6,36},{10,30,90},{.12,.15,0}}

simplex(minSample,Optimize=>Min)


--Sample rref problems
matrix2=mutableMatrix(sub(matrix{{1,1,1,-1},{1,2,4,3},{1,3,9,3}},QQ))
matrix2=mutableMatrix(sub(matrix{{1,2,1},{-2,-3,1},{3,5,0}},QQ))
	    
rref(matrix2)
rank matrix2	    




-- branden

restart
loadPackage"LinearProgramming"
M = {{0,2,3,1,1,0,0,5},{0,4,1,2,0,1,0,11},{0,3,4,2,0,0,1,8},{-1,5,4,3,0,0,0,0}}
simplex M



-- tom