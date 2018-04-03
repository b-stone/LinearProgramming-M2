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
	     {Name => "Vincent Schinina", Email => "vincentschinina@mail.adelphi.edu"}
	     },
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
    local pivotCol;
    local pivotList;
    local costRow;
    
    costRow = costMatrix^{numRows(costMatrix)-1}_{0..numcols(costMatrix)-2};
    pivotCol = position(flatten entries costRow, g -> g < 0);
    if pivotCol === null then return null;
    pivotList = {};
    for i from 0 to numRows(costMatrix)-2 when i < numRows(costMatrix)-1 do (
	if costMatrix_(i, numcols(costMatrix)-1) >= 0 and costMatrix_(i, pivotCol) > 0 then (
       	    	pivotList = append(pivotList, costMatrix_(i, numcols(costMatrix)-1)/costMatrix_(i, pivotCol));
	)
	else pivotList = append(pivotList, -1);	    
    );
    pivotRow = position(pivotList, x -> x == min(select(pivotList, y -> y >= 0)));
     
    return {pivotRow, pivotCol};   
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

-----------------------------------------------------------------------------------------------------------------
--TESTS
-----------------------------------------------------------------------------------------------------------------

TEST/// --check_0
S = simplexMethod {{2,1,4},{1,2,3},{1,1,0}}
M = matrix {{1, 0, .666667, -.333333, 0, 1.66667}, {0, 1, -.333333, .666667, 0, .666667}, {0, 0, .333333, .333333, 1, 2.33333}}
--M = matrix {{1.0, 0, 2/3, -1/3, 0, 5/3}, {0, 1, -1/3, 2/3, 0, 2/3}, {0, 0, 1/3, 1/3, 1, 7/3}}
S = flatten entries S
M = flatten entries M
scan(#S, l -> assert(toString S#l == toString M#l))
scan(#M, l -> assert(toString S#l == toString M#l))
///

-----------------------------------------------------------------------------------------------------------------
--DOCUMENTATION
-----------------------------------------------------------------------------------------------------------------
beginDocumentation()

doc ///
    Key 
        VincentSimplex
    Headline
    	Simplex Method
    Description
    	Text
	    A package that runs the simplex method!
///

doc ///
    Key
    	addSlack
	(addSlack, List)
    Headline
    	Adds the slack varibles to the problem.
    Usage
    	slackMatrix = addSlack inputList
    Inputs
    	inputList:List
	    A list containing the constraints of the problem and the function that will be
	    maximized or minimized. The list is should be entered in the following order:
	    {contraints, function} which means all the constraints are entered first and the
	    function is put in last.
    Outputs
    	slackMatrix:Matrix
	    The list that was inputted was converted to a matrix with the addition of slack
	    variables.
    Description
    	Text
	    The entirity of this method is to simply add slack varibles. The given constraints of
	    a problem contain inequalities. To deal with these inequalties, they get converted to
	    equalities and slack varibles are added in their place to retain the value of the
	    problem. The function that is being maximized or minimized is set equal to a value
	    which can be called $z$. Then all the varibles are moved to one side of the equation
	    to allow it to be equal to a value of 0. This change is how the constraints and the function 
	    will look in the outputted matrix. Let's look at an example:

--	    Maximize $x_1 + x_2$ with the following contraints:
--		$2x_1 + x_2 \leq 4$
--		$x_1 + 2x_2 \leq 3$,
--    	    for $x_1, x_2 \geq 0$.

	    Given the constraints, the coefficients of each term are taken and put into the list. For
	    the first constraint, the listed terms look like: {2,1,4}. For the second constraint, the
	    listed terms look like: {1,2,3}
	    
	    Given the function above, the coefficients of each term is what's entered into the list. Keep
	    in mind that since the constraints have one more term than the function, a 0 is added at the end. 
	    It looks like this: {1,1,0}.
	    
	    Now that all the lists are known, they are put into the following form: {constraints, function}.
	    This is the proper way to input the list into the method. For the given problem it looks like this:
	    {{2,1,4},{1,2,3},{1,1,0}}.
	   
        Text
	    Let's examine what happens when the previously given list is inputted into this method:   	
     	 
        Example
	    inputList = {{2,1,4},{1,2,3},{1,1,0}}
	    slackMatrix = addSlack inputList
	 
	Text
	    It can be seen that the inputted list was changed to a matrix with two slack variables
	    added that take the form of a 2x2 identity matrix placed before the last column. 
	    In addition, the the function was set equal to a value z and each term was negated.
	    The coefficient of z is also 1 and was added in the last row, which overall makes
	    the matrix have a 3x3 identity matrix within placed before the last column. 
///

-----------------------------------------------------------------------------------------------------------------

doc ///
    Key
    	findPivot
	(findPivot, Matrix)
    Headline
    	Finds the row and column of the pivot element.
    Usage
    	pivotList = findPivot slackMatrix
    Inputs
    	slackMatrix:Matrix
	    This is the matrix that takes the form of the outputted matrix from the method addSlack.
    Outputs
    	pivotList:List
	    This is a list the contains the row and column of the pivot element. It is outputted in the form 
	    {pivotRow,pivotColumn}.
    Description
    	Text
	    This method iterates through the last column of slackMatrix until to finds the first negative entry. This
	    denotes the column of the pivotElement. Then starting with the first row of slackMatrix (row index 0), the method takes
	    last entry of the row and divides it by the entry corresponding to the column of the pivotElement. It does
	    this for every row except the last one and filters out negative entries along with any division by zero.
	    Each quotient is entered into a list and any of the divisions that were filtered out get a default value of
	    -1. At the end of the process, the method finds the position of the smallest positive quotient and this
	    position corresponds to the row of the pivotElement. The method finally outputs the position of the
	    pivotElement in the form of the list: {pivotRow, pivotColumn}. Using the same maximization problem, the
	    following example is worked out:
	Example
	    slackMatrix = {{2,1,1,0,0,4},{1,2,0,1,0,3},{(-1),(-1),0,0,1,0}}
	    -- pivotList = findPivot slackMatrix
	Text
	    Looking at the slackMatrix it is clear that the first negative element in the last row is located in the first
	    column (column index 0). Using this, the lowest ratio is computed and it can be see that it is 2, which is located
	    in the first row(row index 0). Therefore, the method outputs the list {0,0}.
///
	          
-----------------------------------------------------------------------------------------------------------------------------------------

doc ///
    Key
    	rowPivot
	(rowPivot, List, Matrix)
    Headline
    	Carries out row operarions with respect to the specific pivot element to reduce the matrix to reduced row echlon form.
    Usage 	    	 
	pivotedMatrix = rowPivot(pivotList, slackMatrix)
    Inputs
    	pivotList:List
	slackMatrix:Matrix
    	    This method takes two inputs. One being the pivotList outputted from the findPivot method and the other is the slackMatrix
	    outputted from the addSlack method.
    Outputs
    	pivotedMatrix:Matrix
	    This method outputs a modified slackMatrix that has undergone row operations.
    Description
    	Text	
    	    Using the inputted list, this method finds the pivot element and zeros out the other elements in the pivot element's corresponding column
	    Then, the method multiples the row of the pivot element by the multiplicative inverse of the pivot element. Keep in mind that it'll never
	    pick a pivot element of 0 because that number is filtered out in the findPivot method. Let's look at an example using the same problem from
	    before.
	Example
	    pivotList = {0,0}
	    slackMatrix = {{2,1,1,0,0,4},{1,2,0,1,0,3},{(-1),(-1),0,0,1,0}}
	    --pivotedMatrix(pivotList, slackMatrix)
	Text
	    From the example, it can be seen that the row 0, which contains the pivot element, has been altered by a multiplication of (1/2). In addition, the
	    entries in column 0, not including the pivot element, have become 0 entries by a simple row operation.
///

-------------------------------------------------------------------------------------------------------------------------------------------------------------------

doc ///
    Key
    	simplexMethod
	(simplexMethod, List)
    Headline
    	Combines all the methods together into one method, containing a while loop, which fully conducts the simplex method.
    Usage
    	simplexMethod inputList
    Inputs
    	inputList:List
    	    A the same discussed in the explanation for the addSlack method. Takes the form of {contraints, function}.
    Outputs
    	solutionMatrix:Matrix
	    This matrix is represented in reduced row echlon form and contains the solutions to the given problem. 
    Description
    	Text
	    This method runs the addSlack method first then takes its output and inputs it into a while loop. This loop contains
	    the findPivot and rowPivot methods. The while loop is exited once the last row of the matrix has no more negative entries.
	    At this point, the matrix present is the solutionMatrix and is outputted as the value of the simplexMethod. Let's look at
	    the same problem fully worked out.
	Example
            inputList = {{2,1,4},{1,2,3},{1,1,0}}
	    simplexMethod inputList
	Text
	    From this example, it can be seen that the outputted matrix is in reduced echlon form. Each entry that has a value of 1 is
	    the variable which the solution in the last column corresponds to. In this case, $x_1 = 2, x_2 = 2/3$ and the maximum value, z,
	    is 7/3.
///
    
-----------------------------------------
-----------------------------------------
-----------------------------------------
end
-----------------------------------------
-----------------------------------------
-----------------------------------------

--test

restart
uninstallPackage"VincentSimplex"
restart
installPackage"VincentSimplex"
check "VincentSimplex"
viewHelp VincentSimplex
viewHelp check

restart
needsPackage"VincentSimplex"
viewHelp assert
toString simplexMethod {{2,1,4},{1,2,3},{1,1,0}} 
simplexMethod {{2,1,4},{1,2,3},{1,1/2,0}}
simplexMethod {{3,1,6},{1,(-1),2},{0,1,3},{2,1,0}}
simplexMethod {{1,2,6},{2,1,6},{1,1,0}}
simplexMethod {{1,6,2,8},{1,0,3,9},{(-7),1,3,0}} 
simplexMethod {{1,1,1,6},{5,3,6,15},{5,3,1,0}}
simplexMethod {{2,5,1},{1,3,4},{6,7,0}}
simplexMethod {{2,3,1,5},{4,1,2,11},{3,4,2,8},{5,4,3,0}}
simplexMethod {{3,2,2},{5,1,3},{29,10,0}}

simplexMethod {{2,1,6},{5,3,7},{-1,-4,0}}
viewHelp scan

