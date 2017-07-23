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
	     {Name => "Harmit Minhas", Email => "harmitminhas@mail.adelphi.edu"},
	     {Name => "Kyle Murray", Email => "kylemurray@mail.adelphi.edu"}	     
	     },
    	Headline => "LinearProgramming",
    	DebuggingMode => true
--	PackageExports => {"Graphs", "Posets", "SimplicialComplexes"},
--	AuxiliaryFiles => true,
--	Configuration => {"DefaultPath" => null } 
    	)

export {
    
    -- Options
--     "Optimize",
--     "Max",
--     "Min",
     "Slack",
    
    -- Methods
     "simplexProc",
     "getMaxCoordinates",
 --     "getMinCoordinates",
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
{*
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

*}
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
    local changerow;    
    matrix2 = mutableMatrix(sub(matrix2,QQ));
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
matrix2 = matrix matrix2;
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

<<<<<<< HEAD
=======
addSlack=method()
addSlack(Matrix) := matrix1  -> (   
    local newList; 
    local tempList; 
    local tempElement; 
    local indexLastRow; 
    local matrix1; 
    local list1; 
    local matrix1;
     
    newList=new List;

   indexLastRow=#entries(matrix1)-1;
	    
    -- i ranges over the rows of the matrix
    for i from 0 to indexLastRow do(
	
 	-- get row i of the matrix
    	tempList=flatten(entries(matrix1^{i}));   
	
	
	-- temporarily remove the constant at the end
     	    
     	remove(tempList,#tempList-2);
	
	
	-- temporarily remove the constant at the end
     	tempElement=tempList#(#tempList-1);    
     	tempList=remove(tempList,#tempList-1);
	
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
>>>>>>> 779fbe93813d005cfb06cecdd97e5a718a42ace3







TEST///
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
///

{*
addSlack=method()
addSlack(Matrix) := matrix1  -> (   
    local newList; 
    local tempList; 
    local tempElement; 
<<<<<<< HEAD
    local indexLastRow; 
    local matrix1; 
    local list1; 
    local matrix1;
    local ZColElement;
     
     indexLastRow=#entries(matrix1)-1;
     
     newList=new List;
    
=======
    local prefaceRow;
    local indexLastRow;
    local indexFirstRow; 
--    local matrix1; 
    local list1; 
--    local matrix1;
    local constants;
    
     
    newList=new List;

    indexLastRow=#entries(matrix1)-1;
    indexFirstRow=0;
   
  --Takes out the first row that cooresponds to < / >
    prefaceRow = flatten(matrix1_[indexFirstRow]);
  --Removes it
    remove(matrix1_[indexFirstRow]);
           

    constants=flatten(matrix1_[indexLastRow]);
    remove(matrix1_[indexLastRow]);
	-- j ranges over the number of rows
       	for j from 0 to indexLastRow  do(
	    
>>>>>>> 779fbe93813d005cfb06cecdd97e5a718a42ace3
	    
    -- i ranges over the rows of the matrix
   for i from 0 to indexLastRow do(
	
 	-- get row i of the matrix
    	tempList=flatten(entries(matrix1^{i}));   
	
	-- temporarily remove the constant at the end
    	tempElement=tempList#(#tempList-1);    
     	tempList=remove(tempList,#tempList-1);
	
	--also remove column before last
	
	ZColElement=tempList#(#tempList-1);
	tempList=remove(tempList,#tempList-1);
	
	
	
	
	-- j ranges over the number of columns
	--adds slack vars based on number of columns
    	for j from 0 to indexLastRow-1  do(
	    

	   
	  --   row i gets 1 added i steps to right, otherwise 0 added
	     if j==i then tempList= append(tempList,1);
	     if j!=i then tempList= append(tempList,0);
	   );
     	tempList=append(tempList,ZColElement);
       	tempList=append(tempList,tempElement);
       	newList=append(newList,tempList);
     	);
    
    	
	

 return matrix newList;
)
<<<<<<< HEAD
=======

*}


>>>>>>> 779fbe93813d005cfb06cecdd97e5a718a42ace3
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

simplexMethod=method(Options=> {Slack=>false})
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
     --  if opts.Optimize==Min then(matrix1 = transpose(matrix1););
      if opts.Slack==false then matrix1=addSlack(matrix1);    
    
   -- matrix1=matrix(rowMult(mutableMatrix(matrix1),numRows(matrix1)-1,-1));
    matrix1=simplexProc(matrix1);
    
    --Coordinates are found depending on goal of our optimiziation
    coordinates = getMaxCoordinates(matrix1);
--    if opts.Optimize==Max then coordinates = getMaxCoordinates(matrix1);
--    if opts.Optimize==Min then coordinates = getMinCoordinates(matrix1);
    optimizedCost=matrix1_(numRows(matrix1)-1,numColumns(matrix1)-1);
    return {matrix1,coordinates,optimizedCost};
 )



--------------------------------------------------
-- DOCUMENTATION
--------------------------------------------------
beginDocumentation()

doc ///
    Key
    	LinearProgramming
    Headline
        A package full of linear programming tools
    Description
        Text
	    Really cool stuff is coming stay tuned.

///

doc ///
    Key
    	addSlack
	(addSlack,Matrix)
    Headline
        Adds slack variables to a matrix problem.
    Usage
    	N = addSlack M
    Inputs
    	M:Matrix
	    Does not contain slack variables.
    Outputs
    	N:Matrix 
	    Containing slack variables.
    Description
        Text
	    This method is responsible for puting slack variables into the matrix. Slack variables
	    are variables that are used to get rid of the inequalities in the contraint equations.
	    There are some assumptions associated with this method. First we assume that all constraint
	    equations are $\leq$. The other assumption is that the function is set up for maximization 
	    and the z value is negative. The cost function must also be the last row with the z value in
	    the last column. 
	    Lookin at an example:
	    
	    Min
	   $z=-2x + 3y $ , this is the cost function.
	    
	   $x - 3y + 2z \leq 3$
	    
	   $ -x +2y \geq 2$
	    
	    The matrix representation of this is:
	   
	Example
    	     P = matrix {{1,-3,2,0,3},{1,-2,0,0,-2},{-2,3,0,-1,0}}
	Text     
	    Notice the cost function was moved to the bottom and the z column is placed before the constants.
	    In the z column all values should be 0 execpt in the cost funtion row. We need to then comply with
	    our assumptions and change the $\geq$ to a $\leq$ simply by multiplying the row by -1.
	    We also consider -z instead of z and change the row accordingly depending on whether we are minimizing 
	    or maximizing. Remeber we will change everything to be always maximizing.
	   
	Example   
	    R = matrix {{1,-3,2,0,3},{1,-2,0,0,-2},{-2,3,0,-1,0}}
	Text    
	    Notice the change in the second row, the constraint vars and the constant got negated.
	    Also notice the z was changed to -z and nothing else in the row was affected.
	    If however the problem was to maximize you would instead negate the entire row.
	    

	Example
	    M = matrix {{1,-3,2,0,3},{1,-2,0,0,-2},{-2,3,0,-1,0}}
	    N = addSlack M
	Text 
	    
            Now the slack variables have be added to all of your constraint rows and they
	    their entries will  always be 1 due to making all of the contraints $\leq.$

///
---------------------------------------------------------------------------------------

doc ///
    Key
    	rref
	(rref,Matrix)
    Headline
        Reduced a matrix to a matrix in reduced row echelon form.
    Usage
    	N = rref  M
    Inputs
    	M:Matrix
	    Not in reduced row echelon form.
    Outputs
    	N:Matrix 
	    In reduced row echelon form.
    Description
        Text
             Row echelon form follows two conditions, all zero rows belong at the bottom of the matix 
             and the pivot of a nonzero row is to the right of the pivot above it. Reduced row echelon form
	     is a matrix in row echelon form and the pivots are one and the only nonzero entry in the column. 
    
        Example
             M = matrix {{3,9,8,3},{1,4,2,4},{7,4,2,9},{3,7,1,4}}
             N = rref M
///


---------------------------------------------------------------------------
doc ///
    Key
    	simplexProc
	(simplexProc,Matrix)
    Headline
        Given a matrix in order with slack variables added, this method applies the simplex method to it.
    Usage
    	N = simplexProc M
    Inputs
    	M:Matrix
	   
    Outputs
    	N:Matrix 
	    With the simplex method applied to it.
    Description
        Text
	   This function selects the largest or smallest entry of the last row. The function then selects the column assocaited with that value.
	   This column is the pivot column. It then finds the ratio of the last column and pivot column. The smallest of these ratios determines the 
	   pivot at which to do row reduction. 
	      
	Example
    	   M = matrix {{1,4,5,2,1},{3,1,5,2,6},{4,2,-3,-3,0}}
	   N = simplexProc M  
	   

///
-------------------------------------------------------------------------
doc ///
    Key
    	getMaxCoordinates
     	(getMaxCoordinates,Matrix)
    Headline
        This method returns the coefficients of the cost function.
    Usage
    	N = getMaxCoordinates M
    Inputs
    	M:Matrix
	   
    Outputs
    	N:List 
	    List of coefficients for the cost function.
    Description
        Text
	    This function is used for producing the overall answer of the simplex problem.
	    The simplex problem is based off of finding the max or min for cost functions. 
	    It takes the output matrix from the simplexproc method, and gives back the
	    coefficients for the cost function. 
	      
	Example
    	   M = matrix {{1,2,0,2,0,-1,0,2},{0,-5,0,-2,1,0,0,1},{0,-1,1,-3,0,2,0,1},{0,-3,0,-1,0,-1,-1,-13}}
	   N = getMaxCoordinates M  
	   

///

-----------------------------------------------------------------------------------------
doc ///
    Key
    	reduceAtPivot
     	(reduceAtPivot,Matrix,ZZ,ZZ)
    Headline
        This method applies Gaussian-Jordan Elimination on the pivot at the row,column provided.
    Usage
    	N = reduceAtPivot M
    Inputs
    	M:Matrix
	I:ZZ
	J:ZZ   
    Outputs
    	N:Matrix 
	    Reduced around pivot (row,column)
    Description
        Text
	    This method will use the two integer parameters as the indicies for row and column.
	    It will use the element of the ith row and the jth column and use that as the pivot.
	    The ith row will be divided by the pivot value making the pivot 1. Then, with the pivot
	    being 1 it will use row operations to make the values above and below the pivot 0.
	Example
    	    M = matrix {{3,4,1},{-1,3,5},{5,3,7}}
	    N = reduceAtPivot(M,0,0)
///
-------------------------------------------------------------------------------------------
doc ///
    Key
    	simplexMethod
     	(simplexMethod,Matrix)
    Headline
        This method ties it all together and completes the simplex method.
    Usage
    	N = simplexMethod M
    Inputs
    	M:Matrix
    Outputs
    	N:Matrix
	L:List
	P:ZZ 
	   
    Description
        Text
	   Before using this method you need to make sure the problem is set up correctly. Every 
	   problem used by this package must be set up for maximization. However if your problem 
	   uses minimization it is a simple process to convert it to maximization. 
	   $z=-2x + 3y $ , this is the cost function.
	    
	   $x - 3y + 2z \leq 3$
	    
	   $ -x +2y \geq 2$

	   The top equation with z is the cost equation. This example is set up for minimzation. 
	   This provides the following matrix:
	 Example 
	   P = matrix {{1,-3,2,0,3},{1,-2,0,0,-2},{-2,3,0,-1,0}}
	 Text
	    Notice the cost function was moved to the bottom and the z column is placed before the constants.
	    In the z column all values should be 0 execpt in the cost funtion row. We need to then comply with
	    our assumptions and change the $\geq$ to a $\leq$ simply by multiplying the row by -1.
	    We also consider -z instead of z and change the row accordingly depending on whether we are minimizing 
	    or maximizing. Remeber we will change everything to be always maximizing.
	   
	Example   
	    R = matrix {{1,-3,2,0,3},{1,-2,0,0,-2},{-2,3,0,-1,0}}
	Text	
	   This method will first check the slack option and use the addslack mehthod if necessary.
	Example   
	   M = addSlack R
	Text  
	   This example used the addslack method and now has the matrix with slack variables included.
	   It then uses the simplexProc method on the matrix.
	   The outputs are a list of the variables to optimize, a marix and a number which is the max cost
	   value.
--	Example
	   N = simplexMethod M
	   
    	   
///

--------------------------------------------------------------------------------------
{*
doc ///
    Key
    	[(simplexMethod),Slack]
    Headline
    	An option to add slack if it hasn't been added already.
    Usage
    	simplexMethod(M,Slack=>false)
    Inputs
    	M:Matrix
    Outputs
    	N:Matrix
	L:List
	C:ZZ
    Description	
    	Text
     	   blah 
 ///
*}


-------------------------------------------------------------------------
doc ///
    Key
    	Slack
    Headline
    	An option to add slack if it hasn't been added already.
    Description	
    	Text
     	   If this option is true then slack variabes have already been added and it will not 
	   call the addSlack method. If it is false it will use the addSlack method to add the 
	   slack variables.
 ///

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

--kyle
restart 
uninstallPackage "LinearProgramming"
restart
installPackage "LinearProgramming"
viewHelp LinearProgramming

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
restart 
uninstallPackage "LinearProgramming"
installPackage "LinearProgramming"
restart
loadPackage "LinearProgramming"
matrix1 = matrix{{1,1,1,-1},{1,2,4,3},{1,3,9,3}}
--matrix2=mutableMatrix(sub(matrix{{1,1,1,-1},{1,2,4,3},{1,3,9,3}},QQ))
--matrix2=mutableMatrix(sub(matrix{{1,2,1},{-2,-3,1},{3,5,0}},QQ))
--matrix2=matrix matrix2	 
--matrix2   
rref(matrix1)
rank matrix1	    




-- tom

restart
loadPackage"LinearProgramming"
M = matrix {{2,3,1,1,0,0,0,5},{4,1,2,0,1,0,0,11},{3,4,2,0,0,1,0,8},{5,4,3,0,0,0,-1,0}}
--addSlack M
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
reduceAtPivot(Mess,1,1)
simplexMethod M

flatten M
entries M
transpose M
simplexMethod entries M
-- tom