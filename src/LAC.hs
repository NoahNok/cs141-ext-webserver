--------------------------------------------------------------------------------
-- Functional Programming (CS141)                                             --
-- Coursework 1: Large Arithmetic Collider                                    --
--------------------------------------------------------------------------------

module LAC where 
import Data.Either
import Data.List
--------------------------------------------------------------------------------

-- | Represents different actions (including their parameters) that a cell can 
-- have on a row or column total.
data Action 
    = Add Int 
    | Sub Int 
    deriving (Eq, Ord, Show)

-- | Represents a cell including whether it is enabled and its action.
data Cell = MkCell Bool Action
    deriving (Eq, Show)

-- | A row has a target number and consists of zero or more cells.
data Row = MkRow Int [Cell]
    deriving (Eq, Show)

-- | A grid is comprised of the target numbers for all columns and the rows.
data Grid = MkGrid [Int] [Row]
    deriving (Eq, Show)

-- | Enumerates directions in which lists can be rotated.
data Direction = L | R
    deriving (Eq, Show)

--------------------------------------------------------------------------------

-- | `eval` @action total@ applies @action@ to the running @total@. 
-- For example:
--
-- >>> eval (Add 5) 3
-- 8
--
-- >>> eval (Sub 1) 3
-- 2
--
---------------------------------------
-- Action has two types that we can evaluate
-- For add the order of x and y doesn't matter as x + y === y + x
-- For subtraction we want to sub from the last number so we must use y - x
-- We pattern match Add and Sub each as Action can only take these types/value as assigned above
---------------------------------------
eval :: Action -> Int -> Int 
eval (Add x) y = x + y
eval (Sub x) y = y - x

-- | `apply` @cell total@ applies the action of @cell@ to the running @total@ 
-- if @cell@ is enabled. For example:
--
-- >>> apply (MkCell True (Add 5)) 3
-- 8
--
-- >>> apply (MkCell False (Add 5)) 3
-- 3
--
---------------------------------------
-- If the boolean provided is True we want to apply the action, if not we just return the value
-- provided to operate on
-- To do this we make use of gards.
-- If bool is true then we evaluate the action onto the provided value x
-- Otherwise we just return x preventing the action being applied
---------------------------------------
apply :: Cell -> Int -> Int 
apply (MkCell bool action) x
    | bool = eval action x
    | otherwise = x

-- | `result` @cells@ calculates the total produced by the actions of all 
-- enabled cells in @cells@ starting from 0. For example:
--
-- >>> result []
-- 0
--
-- >>> result [MkCell True (Add 5), MkCell False (Add 5), MkCell True (Sub 1)]
-- 4
--
---------------------------------------
-- An empty list should return 0 as specified
-- We make use of folder to turn original what was:
-- result (x:xs) = apply x (result xs)
-- And instead tell haskell to go over the list right to left applying 'apply' to each item in the 
-- list to the accumulator, starting with it at 0
---------------------------------------
result :: [Cell] -> Int 
result [] = 0
result x = foldr apply 0 x

-- | `states` @cell@ is a function which returns a list with _exactly_ two
-- elements that represent the two different states @cell@ can be in. For
-- example:
--
-- >>> states (MkCell False (Add 5))
-- [MkCell True (Add 5), MkCell False (Add 5)]
--
---------------------------------------
-- We simply need to return a list containing the True and False state for the given cell
-- We use '_' to allow any value for the boolean to be given, and act to pass the action
-- We hard code MkCell True/False and then provide act for the action needed
-- Hard coding in this case is fine as we only have two desired outcomes
-- No point doing states (MkCell True act) and again with false as this is a lot of excess code
---------------------------------------
states :: Cell -> [Cell]
states (MkCell _ act) = [MkCell True act, MkCell False act]

-- | `candidates` @cells@ is a function which, given a list of cells in a row,
-- produces all possible combinations of states for those cells. For example:
-- 
-- >>> candidates [MkCell False (Add 5), MkCell False (Sub 1)]
-- [ [MkCell False (Add 5), MkCell False (Sub 1)]
-- , [MkCell False (Add 5), MkCell True (Sub 1)]
-- , [MkCell True (Add 5), MkCell False (Sub 1)]
-- , [MkCell True (Add 5), MkCell True (Sub 1)]
-- ]
--
---------------------------------------
-- To get all the candidates, we need to get all the possible combinations fo the list (is the 
-- same order given)
-- We make use of the 'sequence' function to produce these sequences for us. It accepts an array of
-- arrays ([[]]). Producing all the combinations of the arrays inside. For example: 
-- sequence [[True, False], [True, False]] = [[True,True],[True,False],[False,True],[False,False]]
-- The output is the 4 possible combinations. We could use our own method of list comprehension, but 
-- scaling this ourselves requires us to essentially recreate what sequence does, and there is no
-- point re-inventing the wheel
--
-- To get the sequences we need to provide a array of arrays to sequence. In this case we can pass
-- an array containing arrays that contain the states of each of the input arrays cells:
-- [MkCell False (Add 5), MkCell False (Sub 1)] -> 
-- [[MkCell True (Add 5), MkCell False (Add 5)], [MkCell True (Sub 1), MkCell False (Sub 1)]]
-- We can make this hold for any amount of cells in the given row by using another function to 
-- produce this array.
--
-- To solve this I created a new function called 'stateAll'. Originally it was written as:
-- stateAll [] = []
-- stateAll (x:xs) = (states x : stateAll xs) 
-- Which would generate the states of the first element of the array and then append the result of
-- 'stateAll' again and again with whats left of the array until it's empty, returning the array
-- of states for use by sequence
--
-- However, using 'foldr' we can simplify the code.
-- Foldr applys ((:) . states) to each element of the array starting with a stored empty array
-- ((:) . states) works by applying states to each element of the array and then appending the
-- resulting array to the stored array. If the first case this will be stateResultArray : []
-- repeating again and again producing the required array of arrays
--
-- After we feed the result to sequence giving us all the possible candidate combinations for the
-- row
---------------------------------------
candidates :: [Cell] -> [[Cell]]
candidates [] = [[]]
candidates xs = sequence (stateAll xs)

-- sequence [states (MkCell True (Add 20)), states (MkCell False (Sub 3)), states (MkCell False (Add 40))]
-- Take input cells, state each of them and sequence them all

-- Function to state them all
stateAll :: [Cell] -> [[Cell]]
stateAll = foldr ((:) . states) [] 
-- stateAll [] = []
-- stateAll (x:xs) = (states x : stateAll xs) 

-- | `solveRow` @row@ finds solutions for @row@. For example:
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Sub 1)])
-- [[MkCell True (Add 5), MkCell False (Sub 1)]]
--
-- >>> solveRow (MkRow 5 [MkCell False (Add 5), MkCell False (Add 5)])
-- [ MkRow 5 [MkCell True (Add 5), MkCell False (Add 5)] 
-- , MkRow 5 [MkCell False (Add 5), MkCell True (Add 5)]
-- ]
--
---------------------------------------
-- We make use of a new function first called 'validCandidates'. This function takes in all the
-- candidates (Array of arrays of Cells) for the row and returns a array of the candidate rows
-- (array) that total the target. or an empty list if there aren't any.
--
-- Then with this result we can use list comprehension to take the valid rows and turn them into
-- the format of MkRow target row, that is expected as the functions return
---------------------------------------
solveRow :: Row -> [Row]
solveRow (MkRow target row) = [MkRow target validRow | validRow <- validCandidates target (candidates row)]

-- Ooooooh it's magic *sparkles*
validCandidates :: Int -> [[Cell]] -> [[Cell]]
validCandidates _ [] = []
validCandidates target (x:xs)
    | target == result x = x : validCandidates target xs
    | otherwise = validCandidates target xs



-- | `solve` @grid@ finds all solutions for @grid@. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> solve (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell True (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell True (Add 2), MkCell True (Add 2)]
--                ]
-- ]
--
---------------------------------------
-- Please See explanations above each function for details
--
-- To get all the grids we use list comprehension to make a list of valid grids, where the rows in 
-- each grid 'p' is the result of 'processGridCombinations' with the target colum numbers 'numArr'
-- And all of the possible row combinations within that grid
--
-- How it all comes together:
-- It works by getting all the possible valid row combinations using solveRow to get all the
-- possible row combos, then we get all the valid combinations of rows for each grid.
-- Then we can calculate the columns for each grid and check if the columns total their target based 
-- on if the cell is on or off based on the rows. We don't need to iterate over different column
-- combinations as changing the columns cell truths would alter the rows values as well. So we can
-- Just check if the columns match the required amount, if they all do then the grid works if not,
-- then we move on top the next combination, until either one works or none do
--
--
-- This part gave me great pain and an actual headache for using so much raw brain power :(
---------------------------------------
-- Calls getGrids with the output of 'processGridCombinations' that takes in the column targets
-- and all the grid row combinations from 'getAllGridRowCombinations'
solve :: Grid -> [Grid]
solve (MkGrid [] []) = [MkGrid [] []]
--OLD solve (MkGrid numArr rowArr) = [MkGrid numArr []]
--OLD solve (MkGrid numArr rowArr) = [(MkGrid numArr p) | p <- [processGridCombinations numArr (getAllGridRowCombinations rowArr)]]
solve (MkGrid numArr rowArr) = getGrids numArr (processGridCombinations numArr (getAllGridRowCombinations rowArr))

-- This took so long to implement, and fried my brain... 

-- GetGrids takes the output of 'processGridCombinations' and packs them back into the Grid format,
-- for each valid set of rows that make the grid work we add the mto the array. Then we return the
-- array of grids as this is what the return of solve requires
getGrids :: [Int] -> [[Row]] -> [Grid]
getGrids _ [] = []
getGrids colNumbs (row : rows) = MkGrid colNumbs row : getGrids colNumbs rows


-- We take in the colum targets and all the possible row combinations and return a array of arrays
-- of Rows that is all the arrays of rows that work for the grid
-- We use guards to check if solveCols returns true for the solved Cols from the solved rows
-- If True we add the rows to the array and then continue down the list, if its false we just
-- continue without adding those rows
processGridCombinations :: [Int] -> [[Row]] -> [[Row]]
processGridCombinations _ [] = []
processGridCombinations colTargets (row : rows)
    | solveCols (getColsForSolving colTargets row) = row : processGridCombinations colTargets rows
    | otherwise = processGridCombinations colTargets rows

-- valid row combos: sequence (solveRowsToMultipleOptions [row1, row2, ...])

-- Takes the given rows and returns an array of arrays of rows that are all the valid options for 
-- each row that result in the target being met
-- From the commented out lines you can see how we went from a basic implementation to using 'foldr'
-- to apply 'solveRow' and appending the result to using 'map' to apply solveRow to each item in 
-- the array, resulting in the result being collected in to a array of arrays of Rows
-- Using map results in the process also being around 0.8s quicker on average 
solveRowsToMultipleOptions :: [Row] -> [[Row]]
solveRowsToMultipleOptions = map solveRow
--solveRowsToMultipleOptions [] = [] -- []
--solveRowsToMultipleOptions (row : rows) = solveRow row : solveRowsToMultipleOptions rows
--solveRowsToMultipleOptions rows = foldr ((:) . solveRow) [[]] rows

-- We can get all the possible row combinations from the given rows by performing 'sequence' on the
-- result of 'solveRowsToMultipleOptions'.
-- The end result is a array of arrays of rows that are all the possible combinations
-- We do this as if the row given has a row like:
-- MkRow 5 [MkCell False 5, MkCell False 5] 
-- There are two possibilities in which the first cell or the last cell will be active but not both
-- When we have another row with it we need to check all the possible grids combinations as one
-- may work in the end whilst the other doesn't
getAllGridRowCombinations :: [Row] -> [[Row]]
getAllGridRowCombinations row = sequence (solveRowsToMultipleOptions row) 


-- SolveCols loops through all the cols (actually rows, but calling them cols for the sake of it)
-- And then checks each col equals its target.
-- If they all equal the target then the function evaluates to True, if one or more don't work then
-- It evaluates to False as True && False is False, regardless of how many True you have
-- We make use of guards here as it looks nice than an if/else
solveCols :: [Row] -> Bool
solveCols [] = True
solveCols ((MkRow target cells):rows)
    | result cells == target = solveCols rows
    | otherwise = False

-- Links together multiple of the below functions to get the required Columns from the given 'rows' 
-- to then be solved by 'solveCols' above
getColsForSolving :: [Int] -> [Row] -> [Row]
getColsForSolving colTargets rows = getColsAsMkRows colTargets (getCols (rowsToCells rows)) 

-- Simply extracts the cell array from within the Row data type
rowsToCells :: [Row] -> [[Cell]]
rowsToCells [] = []
rowsToCells (MkRow _ cells : nr) = cells : rowsToCells nr

-- Simply this takes the column results and combines them with the desired column totals as
-- MkRow data types.
-- We do so by recursively going through the target numbers and columns at the same time creating a
-- array, ending when we have an empty row/target
-- We can ignore the check for different length targets and rows since they will always be the same
-- length as the program wil never provide array of arrays of cells of length 3 and only 2 targets as
-- this is not how the game can function
getColsAsMkRows :: [Int] -> [[Cell]] -> [Row]
getColsAsMkRows _ [] = []
getColsAsMkRows [] _ = []
getColsAsMkRows (target:nextT) (x:xs) = MkRow target x : getColsAsMkRows nextT xs

-- Imagine forcing this method to work for NxN grids by being naive, and then realising you'r an
-- idiot later on when it doesn't work and you've spent an hour to figure it out to realise whilst
-- eating a chinese takeaway :/
--
-- Anyway,
-- We construct an array  of arrays of cells that are equivalent to the vertical rows of the given grid
-- We do this by doing a list comprehension for the length from 0 to the length of one row 
-- (as gotten by using head to get the first element and calling length on it)
-- Then for each we call getCol with 'p' and xs, where p is the column we want to get and xs being
-- the rows
--
-- The end result is the columns
getCols :: [[Cell]] -> [[Cell]]
getCols = transpose
--getCols [] = [[]] 
--getCols xs = [getCol p xs | p <- [0..(length (head xs)-1)]]

-- Computes a singular column at a given index
-- Each time we look at a row and then get the element at index, by repeating for all rows we get
-- The column E.g:
-- [ x , y ]
-- [ p , q ]
-- Will give us two new rows [x,p] and [y,q] which are the columns
getCol :: Int -> [[Cell]] -> [Cell]
getCol _ [] = []
getCol index (x:xs) = x !! index : getCol index xs 

-- Calculate the valid rows first, then convert the rows into columns and evaluate any that work
-- Then using cols calculate working columns and see if the produced columns list contains the valid
-- cols

-- | `rotate` @direction list@ rotates the items in @list@ to the left or
-- right depending on the value of @direction@. For example:
--
-- >>> rotate L [1,2,3]
-- [2,3,1]
--
-- >>> rotate R [1,2,3]
-- [3,1,2]
--
-- >>> rotate L []
-- []
--
-- >>> rotate R [1]
-- [1]
--
---------------------------------------
-- To start with if we are given an empty list then we return an empty list as we don't need to
-- rotate nothing
-- If we rotate right we can just take the last element and move it to the start. We do so by using
-- 'last' to get the last element and then attach it to the result of 'init' which returns every
-- element but the last
-- If we rotate left we want to take the first element and move it to the end of the list
-- We do so by reversing the list and rotating it Right, then we can reverse the resulting list
-- which gives the same affect rotating Left
---------------------------------------
rotate :: Direction -> [a] -> [a]
rotate _ [] = []
rotate direction list
    | direction == R = last list : init list
    | otherwise = reverse (rotate R (reverse list))

-- | `rotations` @grid@ returns a list of grids containing all possible ways 
-- to rotate @grid@. This means the resulting list should normally have 
-- rows + columns many elements. For example:
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
-- >>> rotations (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5,2] [ MkRow 3 [MkCell False (Add 5), MkCell False (Add 3)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 2), MkCell False (Add 5)]
--                , MkRow 4 [MkCell False (Add 3), MkCell False (Add 2)]
--                ]
-- , MkGrid [5,2] [ MkRow 3 [MkCell False (Add 3), MkCell False (Add 2)]
--                , MkRow 4 [MkCell False (Add 2), MkCell False (Add 5)]
--                ]
-- ]
--
---------------------------------------
-- [Add your explanation of how your implementation works here]
---------------------------------------
rotations :: Grid -> [Grid]
rotations (MkGrid colTargets rows) = turnBackToGrids colTargets (rotateGridRows rows ++ rotateGridCols rows)

-- This simply takes list of list of rows and turns them back into grids with the specified column 
-- targets specified. It does this each time for each list of rows until it receives an empty list
-- at which point the function returns
turnBackToGrids :: [Int] -> [[Row]] -> [Grid]
turnBackToGrids _ [] = []
turnBackToGrids colTargets (row:rows) = MkGrid colTargets row : turnBackToGrids colTargets rows

-- ROW ROTATION

-- This rotates the rows of the given list of rows returning a list of list of rows in which each
-- list of rows has one row rotated
-- We do this by calling 'rotateGivenRowOfGrid' with the index to rotate and the rows to perform it
-- on. We generate the index by using 0 to the length of the rows -1. As for x rows we need to 
-- produce x lists of rows where in each list the x'th row has been rotated
rotateGridRows :: [Row] -> [[Row]]
rotateGridRows rows = [rotateGivenRowOfGrid index rows | index <- [0..(length rows -1)]]

-- This takes a list of rows and the row we want to rotate
-- If the index given is 0 or less then we rotate the given row (first in list) and then attach the
-- rest of ist to the end and return it
-- if not we return the current row and the result of 'rotateGivenRowOfGrid' with index-1 and the
-- left over rows.
-- We repeat until the list is empty or we rotate the desired row
rotateGivenRowOfGrid :: Int -> [Row] -> [Row]
rotateGivenRowOfGrid _ [] = []
rotateGivenRowOfGrid rowIndex (row:rows)
    | rowIndex <= 0 = rotateRow row : rows
    | otherwise = row : rotateGivenRowOfGrid (rowIndex-1) rows

-- This simply rotates the cells Left within a given row
rotateRow :: Row -> Row
rotateRow (MkRow target cells) = MkRow target (rotate L cells)

-- COLUMN ROTATION

-- This returns the rotated grids where the columns have been rotated, by calling 'rotateGridRows'
-- to rotate the columns from 'rowsToCols' the result is then turnedBack into rows using the targets
-- from getRowTargets via 'turnRotatedColsBackToRows'
rotateGridCols :: [Row] -> [[Row]]
rotateGridCols rows = turnRotatedColsBackToRows (getRowTargets rows) (rotateGridRows (rowsToCols rows))

-- This takes the rotated columns and turns them back into rows using getColsForSolving
-- This works as calling 'getColsForSolving' will turn rows to cols and calling ti again will turn
-- those new cols ( now rows) into cols again which reverts the original transformation
-- We repeatedly call this for every list of rows on the list setting the row targets back to their
-- original values
turnRotatedColsBackToRows :: [Int] -> [[Row]] -> [[Row]]
turnRotatedColsBackToRows _ [] = []
turnRotatedColsBackToRows rowTargets (row:rows) = getColsForSolving rowTargets row : turnRotatedColsBackToRows rowTargets rows

-- This converts the given rows to a column representation using 'getColsForSolving' from the
-- 'solve' method 
-- We specify the column targets to be 0,1,2.. etc for the length of the first row,
-- they don't need to be the actual target as this is irrelevant for the rotated cols after we return
-- them back to rows with the proper targets
rowsToCols :: [Row] -> [Row]
rowsToCols [] = []
rowsToCols rows = getColsForSolving [0..(getRowLength (head rows) - 1)] rows

-- Returns the length of the cells in a given row
getRowLength :: Row -> Int
getRowLength (MkRow _ cells) = length cells

-- This takes a list of rows and returns a list of the rows targets by abstracting the row number 
-- from each row
getRowTargets :: [Row] -> [Int]
getRowTargets [] = []
getRowTargets (MkRow target _:rows) = target : getRowTargets rows


-- | `steps` @grid@ finds the sequence of rotations that lead to a solution 
-- for @grid@ in the fewest number of rotations. The resulting list includes 
-- the solution as the last element. You may assume that this function will
-- never be called on a @grid@ for which there are solutions returned by
-- `solve`. The states of intermediate grids in the resulting list
-- are irrelevant - only the ones of the final grid need to be set correctly.
--
-- >>> let row0 = MkRow 3 [MkCell False (Add 2), MkCell False (Add 3)]
-- >>> let row1 = MkRow 4 [MkCell False (Add 5), MkCell False (Add 2)]
-- >>> steps (MkGrid [5, 2] [row0, row1])
-- [ MkGrid [5, 2] [ MkRow 3 [ MkCell False (Add 5), MkCell False (Add 3)] 
--                 , MkRow 4 [ MkCell False (Add 2), MkCell False (Add 2)]
--                 ]  
-- , MkGrid [5, 2] [ MkRow 3 [ MkCell True (Add 3), MkCell False (Add 5)] 
--                 , MkRow 4 [ MkCell True (Add 2), MkCell True (Add 2)]
--                 ] 
-- ]
--
---------------------------------------
-- My implementation work by using a tree and looking at it in a breadth-first way
-- We look at the rotations produced for each row and check if any of them result in a correct grid,
-- if they do we return the grid and the previous grids that lead to it. If none of them work we
-- generate the next row and repeat (basically a tree)
--
-- Doing this as described works but would cause a lot of unnecessary checking as we may produce a
-- grid on the next row that we have already seen. There is not point generating it's rotations as
-- they will of already been generated and checked, the same for all the rotations grids and so on.
-- So I'm passing a array of grids that we have encountered before. This way we can skip ones we 
-- have seen before reducing the amount of calls to solve, and preventing a pointless endless cycle
-- of grid rotations that wastes time. As for a 2x2 rotating the first row twice results in the
-- original grid, and if we didn't ignore this we would constantly repeat this every other row, so
-- by checking against known grids we prevent this, it also means that the function will actually
-- terminate if we have no solutions. Otherwise it would repeat forever producing the same rows over
-- and over again. By removing them we will eventually end up with an empty list of grids returned
--
-- To store the history I created a new data type called StepRow which stores the previous grid
-- rotations and then a array of the current rotations from the previous grid
-- This way once a grid solves we can return the history and the solved grid out as required
--
-- Please see each function for how they work
---------------------------------------
-- A StepRow containing the history and the grids, managed to code this in one run and then apply
-- two quick fixes to get it to work :)
data StepRow = MkStepRow [Grid] [Grid]
    deriving (Eq, Show)

-- Returns the list of valid grids, or an empty list if it can't be solved. We use drop 1 list to
-- remove the first element as in the specification the solved grids shouldn't include the input
-- grid, and 'iterateLevelsUntilSolved' includes the input grid
steps :: Grid -> [Grid]
steps grid = drop 1 (iterateLevelsUntilSolved (rotateStepRows [MkStepRow [] [grid]]) [grid])

-- To iterate each level with first get all the valid StepRow's from 'validateStepRows'
-- The return of 'validateStepRows' is of type Either, we store the left result in 'left' as we use
-- it multiple times and it saves us calling 'fromLeft' multiple times
-- Then we see if level is Right or Left, if it's Right then we have found steps to the solution
-- and we return then by getting the Right value from level using fromRight, otherwise we repeat
-- using the left content (new StepRows without any known grids in them) to call 'iterateLevels...'
-- with the new row of grids from 'rotateStepRows' and the known grids which include the current 
-- known grids and the grids produced from the current level in 'left'.
-- We repeat this until either a solution is found or the given StepRows is an empty list, meaning
-- their is no solution
-- Starting stepRow, known grids -> solved steps
iterateLevelsUntilSolved :: [StepRow] -> [Grid] -> [Grid]
iterateLevelsUntilSolved [] _ = [MkGrid [] []]
iterateLevelsUntilSolved stepRows known = do
    let level = validateStepRows known stepRows
    let left = fromLeft [] level
    if isRight level then fromRight [] level else iterateLevelsUntilSolved (rotateStepRows left) (known ++ getGridsFromStepRows left)

-- This simply extracts the grids from within each StepRow in a list of StepRows and returns all the
-- grids
getGridsFromStepRows :: [StepRow] -> [Grid]
getGridsFromStepRows [] = []
getGridsFromStepRows ((MkStepRow _ grids):stepRows) = grids ++ getGridsFromStepRows stepRows 

-- The takes a list of StepRows and returns a list of new StepRows where the new list contains the
-- next level of the tree that result from the rotations of each grid within the previous StepRows
-- The resulting list will contain alot of elements, but will be thinned out by 'validateStepRows'
-- in 'iterateLevelsUntilSolved'
--
-- The commented out function bodies from bottom to top show how I reduced the method to a simpler
-- form using the Haskell system hints
-- Both folder methods are the same, as with haskell if both sides are functions that take in the
-- same variable then we can ommit it and haskell will work it out for us
-- The foldr implementation apply's 'rotateStepRow' to each element of the provided list,
-- and then adds them to the given starting list [] using '++' from right to left
-- We can then simplify this to use 'concatMap' as this does what our foldr is doing. It works by
-- mapping the function 'rotateStepRow' to each element of the given list and then appending all the
-- results
-- The result of jumping to foldr and then concatMap also made the method slightly quicker
rotateStepRows :: [StepRow] -> [StepRow]
rotateStepRows = concatMap rotateStepRow
--rotateStepRows = foldr ((++) . rotateStepRow) [] 
--
--rotateStepRows stepRows = foldr ((++) . rotateStepRow) [] stepRows
--
--rotateStepRows [] = []
--rotateStepRows (stepRow:stepRows) = rotateStepRow stepRow ++ rotateStepRows stepRows

-- This takes a StepRow and returns a list of new StepRows where each new StepRow contains a list
-- of grids that are the rotations of each grid within the provided StepRow
rotateStepRow :: StepRow -> [StepRow]
rotateStepRow (MkStepRow history grids) = [MkStepRow (history ++ [grid]) (rotations grid) | grid <- grids]

-- This method determines wether the current tree row of StepRows has a valid solution.
-- First we remove any known grids by calling 'removeKnownFromStepRows' to prevent unnecessarily
-- checking the same grids over and over storing the result in 'removed'
-- Then we check if any of the rows grid have a solution by calling 'checkAnyStepRowSolves' with the
-- list of stepRows in 'removed', storing the result in 'solved'
-- Then if solved is not empty (solution found) we return 'Right solved' which uses the Either type
-- to allow us to return two different things from one function. Right in this case is the solution
-- If it is empty then we return 'Left removed' which returns the StepRows to be used to create 
-- the next row of the tree
-- 
-- Known Grids -> Current StepRows-> Validated step rows
validateStepRows :: [Grid] -> [StepRow] -> Either [StepRow] [Grid]    
validateStepRows knownGrids stepRows = do
    let removed = removeKnownFromStepRows knownGrids stepRows
    let solved = checkAnyStepRowSolves removed
    if solved /= [] then Right solved else Left removed

-- This function checks all the grids in a given StepRow to see if they work, if they do we return
-- the list of all the rotations that lead to it, if it doesn't we return an empty list
-- To check each grid we call 'checkAnyGridSolves' with 'grids' and store the result in solves
-- If solves is not 'Nothing' then we return the stored 'history' with the solved grid from 
-- 'fromJust'. We call this as 'checkAnyGridSolves' returns a type of Maybe which can be Nothing or
-- Just x, in this case we need to get x out from the Just x, so 'fromJust' does this for us
-- If it's Nothing then we repeat with the next StepRow's grids, until one works or none work and 
-- we return an empty list
checkAnyStepRowSolves :: [StepRow] -> [Grid]
checkAnyStepRowSolves [] = []
checkAnyStepRowSolves ((MkStepRow history grids):stepRows) = do
    let solves = checkAnyGridSolves grids
    if solves /= Nothing then history ++ [fromJust solves] else checkAnyStepRowSolves stepRows


-- This simply extracts the Grid from the result of 'checkAnyGridSolves' if it is Just
-- This will never return an empty grid as we never pass in 'Nothing', but we have to specify it to 
-- satisfy the 'Maybe' type as it has two possible outcomes of 'Nothing' or 'Just x' 
fromJust :: Maybe Grid -> Grid
fromJust (Just g) = g
fromJust Nothing = MkGrid [] []
    
-- Takes a list of grids and determines if any of them can be solved
-- We do so by calling 'solve' on each grid and then if the result is not an empty list we return ]
-- the first grid (some grids may have multiple solutions). But to return we call 'Just'
-- as the return type of the method is 'Maybe Grid' we can have a grid that works or nothing
-- so we return 'Just grid' (where grid is the first grid in the solved list), or if none of them
-- solve we return 'Nothing' which can then be handled by the function calling this function
checkAnyGridSolves :: [Grid] -> Maybe Grid
checkAnyGridSolves [] =  Nothing
checkAnyGridSolves (grid:grids) = do
    let solved = solve grid
    if solved /= [] then Just (head solved) else checkAnyGridSolves grids

-- This simply iterates over each StepRow and removes all the known grids from each of the StepRows
-- grid stores
-- We do so by calling 'removeKnownGrids' with the current stepRow and the known grids to remove
removeKnownFromStepRows :: [Grid] -> [StepRow] -> [StepRow]
removeKnownFromStepRows _ [] = []
removeKnownFromStepRows knownGrids (stepRow:stepRows) = removeKnownGrids knownGrids stepRow : removeKnownFromStepRows knownGrids stepRows

-- This takes the given step row and known grids and calls 'remove' with each of the known grids and 
-- the current grids in the step row
-- Each time we return a new StepRow with the grid removed, and repeat until we have processed then
-- removal of all the grids in the known list fro mthe step row
removeKnownGrids :: [Grid] -> StepRow -> StepRow
removeKnownGrids [] sr = sr 
removeKnownGrids (knownGrid : kGrids) (MkStepRow history grids) = removeKnownGrids kGrids (MkStepRow history (remove knownGrid grids))

-- This removes the given element from the given list if present
-- This is the simplified result of the answer by Juan Carlos Kuri Pinto here:
-- https://stackoverflow.com/questions/2097501/learning-haskell-how-to-remove-an-item-from-a-list-in-haskell
remove :: Eq a => a -> [a] -> [a]
remove element = filter (/=element)

--------------------------------------------------------------------------------
