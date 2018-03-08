---------------------------------------------------------------------------
-- PURPOSE : LinearPrograming is a package for Macaulay2 and is
-- an implementation of various linear programing methods.
--
-- Copyright (C) 2018 Branden Stone and Vincent Schinina
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
    	Date => "March 7, 2018",
    	Authors => {       
	     {Name => "Branden Stone", Email => "bstone@adelphi.edu", HomePage => "http://math.adelpi.edu/~bstone/"},
	     Name => "Vincent Schinina", Email => "vincentschinina@mail.adelphi.edu"},
    	Headline => "VincentSimplex",
    	DebuggingMode => true
    	)

export {
    
    -- Options
    
    -- Methods
     "addSlack",
     "findPivot",
     "rowPivot",
     "simplexMethod"
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

addSlack = method() --Input: List, Output: Proper Matrix for simplex method usage, Function: Adds slack variables before last column. Negates cost entries and z=1.
addSlack(List) := costList -> ( 
    local costMatrix;
    
    costList = drop(costList, -1)|{(-1)*last costList};
    costMatrix = matrix(costList);
    costMatrix = costMatrix_{0..#costList#0-2}|id_(RR^#costList)|costMatrix_{#costList#0-1};
    
    return costMatrix;
    )
 
findPivot = method() -- Input: Matrix, Output: Integer for pivot row, Function: Finds pivot row.
findPivot(Matrix) := costMatrix -> (
    local pivotRow;
    local pivotElement;
    local pivotList;
    local costRow;
    
    costRow = costMatrix^{numRows(costMatrix)-1}_{0..numcols(costMatrix)-2};
    pivotElement = position(flatten entries costRow, g -> g < 0);
    if pivotElement === null then return null;
    pivotList = {};
    for i from 0 to numRows(costMatrix)-2 when i < numRows(costMatrix)-1 do (
	if costMatrix_(i, numcols(costMatrix)-1) >= 0 and costMatrix_(i, pivotElement) > 0 then (
       	    	pivotList = append(pivotList, costMatrix_(i, numcols(costMatrix)-1)/costMatrix_(i, pivotElement));
	)
	else pivotList = append(pivotList, -1);	    
    );
    pivotRow = position(pivotList, x -> x == min(select(pivotList, y -> y >= 0)));
     
    return {pivotRow, pivotElement};   
    )

rowPivot = method() -- Input: Integer for row, Output: Matrix after pivot, Function: Carries out the row pivot.
rowPivot(List, Matrix) := (pivotList, pivotMatrix) -> (
    local pivotedMatrix;
    local multiplier;    
    local pivotMMatrix;
    
    pivotMMatrix = mutableMatrix pivotMatrix;
    for i from 0 to numRows(pivotMMatrix)-1 when i<numRows(pivotMMatrix) do (
	if i != pivotList#0 then (
    	    multiplier = (-1)*pivotMMatrix_(i, pivotList#1)/pivotMMatrix_(pivotList#0, pivotList#1); 
	    pivotMMatrix = rowAdd(pivotMMatrix, i, multiplier, pivotList#0);
	);    
    );
    multiplier = 1/pivotMMatrix_(pivotList#0,pivotList#1);
    pivotMMatrix = rowMult(pivotMMatrix, pivotList#0, multiplier);
    pivotedMatrix = matrix pivotMMatrix;
    
    return pivotedMatrix;
    )

simplexMethod = method() -- Input: costList, Output: The solution for the problem, Function: Combines all the previous method to run the simplex method.
simplexMethod(List) := costList -> (
    local costMatrix;
    local solutionMatrix;
   
    costMatrix = addSlack costList;
    while findPivot(costMatrix) =!= null do (
	costMatrix = rowPivot(findPivot(costMatrix), costMatrix);
    );
    solutionMatrix = costMatrix;
   
    return solutionMatrix;
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
simplexMethod {{2,1,4},{1,2,3},{1,1,0}}
simplexMethod {{2,1,4},{1,2,3},{1,1/2,0}}
simplexMethod {{3,1,6},{1,(-1),2},{0,1,3},{2,1,0}}
simplexMethod {{1,2,6},{2,1,6},{1,1,0}}
simplexMethod {{1,6,2,8},{1,0,3,9},{(-7),1,3,0}} 
simplexMethod {{1,1,1,6},{5,3,6,15},{5,3,1,0}}
simplexMethod {{2,5,1},{1,3,4},{6,7,0}}
simplexMethod {{2,3,1,5},{4,1,2,11},{3,4,2,8},{5,4,3,0}}
simplexMethod {{3,2,2},{5,1,3},{29,10,0}}
viewHelp scan
