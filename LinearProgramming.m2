---------------------------------------------------------------------------
-- PURPOSE : Visualize package for Macaulay2 provides the ability to 
-- visualize various algebraic objects in java script using a 
-- modern browser.
--
-- Copyright (C) 2013 Branden Stone and Jim Vallandingham
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
	     {Name => "Branden Stone", Email => "bstone@adelphi.edu", HomePage => "http://math.adelpi.edu/~bstone/"}
	     },
    	Headline => "LinearProgramming",
    	DebuggingMode => false,
--	PackageExports => {"Graphs", "Posets", "SimplicialComplexes"},
--	AuxiliaryFiles => true,
--	Configuration => {"DefaultPath" => null } 
    	)

export {
    
    -- Options

    
    -- Methods
     "getCurrPath"


}


------------------------------------------------------------
-- Global Variables
------------------------------------------------------------




------------------------------------------------------------
-- METHODS
------------------------------------------------------------

-- Input: None.
-- Output: String containing current path.

getCurrPath = method()
installMethod(getCurrPath, () -> (local currPath; currPath = get "!pwd"; substring(currPath,0,(length currPath)-1)|"/"))

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







-- sarthak








-- branden


