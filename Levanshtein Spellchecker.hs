--Abhinav R. Pandey 
--CS330 Assignmtn 09 -Fall 2017 

import System.IO
import Data.Char
import System.Environment 
import Data.List 
import Data.Function 


--Implementation of a function that finds the Levensthein distance between two words:
--Based on the 'A Recursive Solution' portion of the website listed in the .README file. 
levDist :: (Eq a) => [a] -> [a] -> Int
levDist [] [] = 0
levDist [] l2 = length l2
levDist l1 [] = length l1
levDist l1 l2
	| head l1 == head l2 = levDist (tail l1) (tail l2)
	| otherwise = minimum [1+ levDist (tail l1) l2, 1 + levDist l1 (tail l2), 1 + levDist (tail l1) (tail l2)]

--Helper function to filter out punctuation. 
wordPure :: String -> String 
wordPure s1 = [if isAlpha m == True then toLower m else ' '| m <- s1]

--Function that sees if a word is spelled correctly by checking if it is in the dictionary provided. 
presentInDict :: String -> [String] -> Bool 
presentInDict word [] = False
presentInDict word (x:xs)
	| word /= x = presentInDict word xs 
	| word == x = True 

--Function that sorts a list of tupules based on the second value. 
sortList :: Ord b => [(a,b)] -> [(a,b)]
sortList = reverse.sortBy(flip compare `on` snd)

--Main function that takes the arguments in the format: Dictionay List, Word File, Results File. 
main = do
	args <- getArgs
	--Opening the files, and filtering out the punctuation associated with each word, as well converting all words to lowercase. 
	inputDict <- readFile (args!!0)
	inputFile <- readFile (args!!1)
	let outputFile = args!!2
	let filteredInput = wordPure inputFile
	let filteredDict = wordPure inputDict 
	--Creating the list of words and the dictionary from the filtered input files.  
	let wordList = words(filteredInput)
	let (x:xs) = filter (/= "") wordList
	let (w:ws) = words(filteredDict)
	--Defining a local function that recursively finds 10 words with the least distance. Words with the same distance are printed in alphabetical order. 
	topTen (x:xs) (w:ws) outputFile where 
		topTen [] _ _ = putStrLn "Substitutions generated."
		topTen (x:xs) (y:ys) outputFile = do
			if (presentInDict x (y:ys) == True) 
				then do 
					topTen xs (y:ys) outputFile
			else do 
				let distanceList = [(dw, levDist cw dw) | let cw = x , dw <- (y:ys)]
				let firstTen = take 10 (sortList(distanceList))
				let unzipped = (fst(unzip firstTen))
				appendFile outputFile (id(x) ++ " " ++ ":" ++ " " ++ (unwords unzipped) ++ "\n") 
				topTen xs (y:ys) outputFile 





	 
	
