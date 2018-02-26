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
     "addSlack"
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

addSlack=method()
addSlack(List) := costList ->( 
    local costMatrix;
    
    costMatrix = matrix(costList);
    costMatrix = costMatrix_{0..#costList#0-2}|id_(RR^#costList)|costMatrix_{#costList#0-1};
    
    return costMatrix;
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
addSlack{{2,3,4},{5,6,7},{2,3,1}}
