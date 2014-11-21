-- solve ["004005700","000009400","360000008","720060000","000402000","000080093","400000056","005300000","006100900"]
type Matrix a = [Row a]
type Row a = [a]
type Grid = Matrix Digit
type Digit = Char
digits :: [Char]
digits = ['1' .. '9']

blank :: Digit -> Bool
blank = (== '0')

solve :: Grid -> [Grid]
solve = filter valid . completions

completions :: Grid -> [Grid]
completions = expand . many prune . choices

prune :: Matrix [Digit] -> Matrix [Digit]
prune = pruneBy boxs . pruneBy cols . pruneBy rows

pruneRow :: Row [Digit] -> Row [Digit]
pruneRow row = map (remove fixed) row
            where fixed = [d | [d] <- row]

pruneBy f = f . map pruneRow . f

many :: (Ea a) => (a -> a) -> a -> a
many f x = if x == y then x else many f y
         where y = f x

remove :: [Digit] -> [Digit] -> [Digit]
remove ds [x] = [x]
remove ds xs = filter (`notElem` ds) xs

choices :: Grid -> Matrix [Digit]
choices = map (map choice)

choice d = if blank d then digits else [d]

cp :: [[a]] -> [[a]]
cp [] = [[]]
cp (xs : xss) = [x:ys | x <- xs, ys <- yss]
                where yss = cp xss

expand :: Matrix[Digit] -> [Grid]
expand = cp . map cp

valid :: Grid -> Bool
valid g = all nodups (rows g) &&
		all nodups (cols g) &&
		all nodups (boxs g)

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x : xs) = all (/=x) xs && nodups xs

rows :: Matrix a -> Matrix a
rows = id

cols :: Matrix a -> Matrix a
cols [xs]  =[[x] | x <- xs]
cols (xs : xss) = zipWith (:) xs (cols xss)

boxs :: Matrix a -> Matrix a
boxs = map ungroup . ungroup .
       map cols .
       group . map group

group :: [a] -> [[a]]
group [] = []
group xs = take 3 xs : group (drop 3 xs)

ungroup :: [[a]] -> [a]
ungroup = concat

