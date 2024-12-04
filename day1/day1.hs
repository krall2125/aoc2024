import Data.Bool
import Data.List

strsplitactual :: String -> String -> Char -> Int -> [String]
strsplitactual str buf c i = if i >= length str then
		if length buf == 0 then
			[]
		else
			[buf]
	else
		case head (drop i str) of
			x | x == c -> buf : strsplitactual str "" c (i + 1)
			_ -> strsplitactual str (buf ++ [head (drop i str)]) c (i + 1)

strsplit :: Char -> String -> [String]
strsplit c str =
	strsplitactual str "" c 0

parseLists :: [String] -> Int -> Bool -> ([Int], [Int])
parseLists str iter parsingSecond =
	if iter >= length str then
		([], [])
	else
		let item = read (head (drop iter str)) :: Int in
		let lists =
			if parsingSecond then
				([], [item])
			else
				([item], []) in
		let nextlists = parseLists str (iter + 1) (not parsingSecond) in
		(fst lists ++ fst nextlists, snd lists ++ snd nextlists)

count :: (Eq a) => [a] -> Int -> a -> Int
count list iter elem =
	if iter >= length list then
		0
	else
		if head (drop iter list) /= elem then
			count list (iter + 1) elem
		else
			1 + (count list (iter + 1) elem)

flatten :: [[a]] -> [a]
flatten [[]] = []
flatten [[a]] = [a]
flatten (x : xs) = x ++ (flatten xs)

pairLists :: ([Int], [Int]) -> ([Int], [Int])
pairLists lists = (sort (fst lists), sort (snd lists))

distanceLists :: ([Int], [Int]) -> Int -> [Int]
distanceLists lists iter =
	if iter >= length (fst lists) then
		[]
	else
		let first = head (drop iter (fst lists)) in
		let second = head (drop iter (snd lists)) in
			[(max first second) - (min first second)] ++
			distanceLists lists (iter + 1)

multiplyStuff :: [Int] -> [Int] -> [Int]
multiplyStuff [] _ = []
multiplyStuff _ [] = []
multiplyStuff (x : xs) (y : ys) = [x * y] ++ multiplyStuff xs ys

main :: IO ()
main = do
	input <- readFile "input.txt"

	let splited = filter (\x -> length x > 0) (strsplit ' ' input)
	let nums = flatten (map (strsplit '\n') splited)
	let parsed = parseLists nums 0 False


	let counts = map (count (snd parsed) 0) (fst parsed)

	let multiplied = multiplyStuff (fst parsed) counts

	let total = foldl (+) 0 multiplied
	putStrLn (show total)
	-- let paired = pairLists parsed
	-- let distances = distanceLists paired 0
	-- putStrLn (show total)
		-- let lists = parseLists input 0 False
		-- let paired = pairLists lists
		-- let distances = distanceLists paired 0
		-- let total = foldl (+) 0 distances
		-- putStrLn (show paired)
		-- putStrLn (show distances)
		-- putStrLn (show total)
