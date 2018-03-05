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
	"VincentSimplex",
    	Version => "0.1", 
    	Date => "March 25, 2016",
    	Authors => {       
	     {Name => "Branden Stone", Email => "bstone@adelphi.edu", HomePage => "http://math.adelpi.edu/~bstone/"},
	     },
    	Headline => "VincentSimplex",
    	DebuggingMode => true
    	)

export {
    
    -- Options
    
    -- Methods
     "addSlack",
     "findPivot",
     "rowPivot"
}


------------------------------------------------------------
-- Global Variables
------------------------------------------------------------




------------------------------------------------------------
-- METHODS
------------------------------------------------------------


-- Input: 

-- Output:

-- Description:

addSlack=method() --Input: List, Output: Proper Matrix for simplex method usage, Function: Adds slack variables before last column. Negates cost entries and z=1.
addSlack(List) := costList ->( 
    local costMatrix;
    
    costList = drop(costList, -1)|{(-1)*last costList};
    costMatrix = matrix(costList);
    costMatrix = costMatrix_{0..#costList#0-2}|id_(RR^#costList)|costMatrix_{#costList#0-1};
    
    return costMatrix;
    )
 
findPivot=method() -- Input: Matrix, Output: Integer for pivot row, Function: Finds pivot row.
findPivot(Matrix) := costMatrix ->(
    local pivotRow;
    local pivotElement;
    local pivotList;
    local costRow;
    
    --costMatrix = matrix{{2,3,1,0,0,4},{5,6,0,1,0,7},{(-1),(-5),0,0,1,0}}
    costRow = costMatrix^{numgens target costMatrix-1}_{0..numgens source costMatrix-2};
    pivotElement = position(flatten entries costRow, g -> g < 0);
    pivotList = {};
    for i from 0 to numgens target costMatrix-2 when i < numgens target costMatrix-1 do (
       	pivotList = append(pivotList, costMatrix_(i, numgens source costMatrix-1)/costMatrix_(i, pivotElement));
    );
    pivotRow = position(pivotList, x -> x == min pivotList);   
  
    return {pivotRow, pivotElement};   
    )

rowPivot = method() -- Input: Integer for row, Output: Matrix after pivot, Function: Carries out the row pivot.
rowPivot(List, Matrix) := (pivotList, pivotMatrix) -> (
    local pivotedMatrix;
    local multiplier;
    

    --pivotMatrix = sub(matrix{{2,3,1,0,0,4},{5,6,0,1,0,7},{(-2),(-3),0,0,1,0}}, RR)
    pivotMatrix = mutableMatrix pivotMatrix;
    for i from 0 to numRows(pivotMatrix)-1 when i<numRows(pivotMatrix) do (
	if i != pivotList#0 then (
    	    multiplier = (-1)*pivotMatrix_(i, pivotList#1)/pivotMatrix_(pivotList#0, pivotList#1); 
	    pivotMatrix = rowAdd(pivotMatrix, i, multiplier, pivotList#0);
    	);    
    );
    pivotedMatrix = matrix pivotMatrix;
    
    return pivotedMatrix;
    )

-----------------------------------------
-----------------------------------------
-----------------------------------------
end
-----------------------------------------
-----------------------------------------
-----------------------------------------

--test

restart
needsPackage"VincentSimplex"
costList = {{2,3,4},{5,6,7},{2,3,0}}
slack = addSlack costList
pivotList = findPivot slack
rowPivot(pivotList, slack) 
viewHelp scan
