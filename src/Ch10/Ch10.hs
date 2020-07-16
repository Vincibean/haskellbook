module Ch10.Ch10 where

import           Data.Bool
import           Data.Time

-- Exercises: Understanding Folds
-- 1. foldr (*) 1 [1..5]
--    will return the same result as which of the following:
--    b) foldl (flip (*)) 1 [1..5]
--    c) foldl (*) 1 [1..5]

-- 2. Write out the evaluation steps for
--    foldl (flip (*)) 1 [1..3]

--    foldl f z [1, 2, 3]
--       f ~ (flip (*)); z ~ 1
--       (((z `f` 1) `f` 2) `f` 3)
--    f = flip (*)
--    (((1 `f` 1) `f` 2) `f` 3)
--    ((1 `f` 2) `f` 3)
--    (2 `f` 3)
--    6

-- 3. One difference between foldr and foldl is:
--    c) foldr, but not foldl, associates to the right

-- 4. Folds are catamorphisms, which means they are generally used to
--    a) reduce structure

-- 5. The following are simple folds very similar to what you’ve already seen, but each has at least one error. Please fix them and test in your REPL:
a = foldr (++) "" ["woot", "WOOT", "woot"]
b = foldr max (minBound :: Char) "fear is the little death"
c = foldr (&&) True [False, True]
d = foldr (||) False [False, True]
e = foldl (++) "" $ map show [1..5]
e' = foldl (flip $ (++) . show) "" [1..5]
f = foldr (flip const) 'a' [1..5]
g = foldr (flip const) 0 "tacos"
h = foldl const 0 "burritos"
i = foldl const 'z' [1..5]

-- Database Processing
data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
              , DbNumber 9001
              , DbString "Hello, world!"
              , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
              ]

-- 1. Write a function that filters for DbDate values and returns a list of the UTCTime values inside them.
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr date []
  where date (DbDate utcTime) acc = utcTime : acc
        date _ acc                = acc

-- 2. Write a function that filters for DbNumber values and returns a list of the Integer values inside them.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr num []
  where num (DbNumber i) acc = i : acc
        num _ acc            = acc

-- 3. Write a function that gets the most recent date.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

-- 4. Write a function that sums all of the DbNumber values.
sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

-- 5. Write a function that gets the average of the DbNumber values.
--    You'll probably need to use fromIntegral
--    to get from Integer to Double.
avgDb :: [DatabaseItem] -> Double
avgDb is = sum / count
  where sum = fromIntegral . sumDb $ is
        count = fromIntegral . length . filterDbNumber $ is

avgDb' :: [DatabaseItem] -> Double
avgDb' is = (fromIntegral sum) / count
  where (sum, count) = foldr avg (0, 0) is
        avg (DbNumber i) (s,c) = (s + i, c + 1)
        avg _ acc              = acc

-- Scans Exercises
-- 1. Modify your fibs function to only return the first 20 Fibonacci numbers.
fibs = 1 : scanl (+) 1 fibs
se1 = take 20 fibs

-- 2. Modify fibs to return the Fibonacci numbers that are less than 100.
se2 = takeWhile (< 100) fibs

-- 3. Try to write the factorial function from Recursion as a scan.
factorial i = scanl (*) 1 [1..] !! i

-- Chapter Exercises
-- Warm-up and review

-- 1. Given the following sets of consonants and vowels
--    a) Write a function that takes inputs from stops and vowels and makes 3-tuples of all possible stop-vowel-stop combinations.

stops = "pbtdkg"
vowels = "aeiou"

stopVowelStops :: [(Char, Char, Char)]
stopVowelStops = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops]

-- 1. b) Modify that function so that it only returns the combinations that begin with a p.
stopVowelStops' :: [(Char, Char, Char)]
stopVowelStops' = [(s1, v, s2) | s1 <- stops, s1 == 'p', v <- vowels, s2 <- stops]

stopVowelStops'' :: [(Char, Char, Char)]
stopVowelStops'' = [('p', v, s2) | v <- vowels, s2 <- stops]

-- 1. c) Now set up lists of nouns and verbs (instead of stops and vowels) and modify the function to make tuples representing possible noun-verb-noun sentences.
nouns = ["Actor", "Gold", "Painting", "Advertisement", "Grass", "Parrot", "Afternoon", "Greece", "Pencil", "Airport", "Guitar", "Piano", "Ambulance", "Hair", "Pillow", "Animal", "Hamburger", "Pizza", "Answer", "Helicopter", "Planet", "Apple", "Helmet", "Plastic", "Army", "Holiday", "Portugal", "Australia", "Honey", "Potato", "Balloon", "Horse", "Queen", "Banana", "Hospital", "Quill", "Battery", "House", "Rain", "Beach", "Hydrogen", "Rainbow", "Beard", "Ice", "Raincoat", "Bed", "Insect", "Refrigerator", "Belgium", "Insurance", "Restaurant", "Boy", "Iron", "River", "Branch", "Island", "Rocket", "Breakfast", "Jackal", "Room", "Brother", "Jelly", "Rose", "Camera", "Jewellery", "Russia", "Candle", "Jordan", "Sandwich", "Car", "Juice", "School", "Caravan", "Kangaroo", "Scooter", "Carpet", "King", "Shampoo", "Cartoon", "Kitchen", "Shoe", "China", "Kite", "Soccer", "Church", "Knife", "Spoon", "Crayon", "Lamp", "Stone", "Crowd", "Lawyer", "Sugar", "Daughter", "Leather", "Sweden", "Death", "Library", "Teacher", "Denmark", "Lighter", "Telephone", "Diamond", "Lion", "Television", "Dinner", "Lizard", "Tent", "Disease", "Lock", "Thailand", "Doctor", "London", "Tomato", "Dog", "Lunch", "Toothbrush", "Dream", "Machine", "Traffic", "Dress", "Magazine", "Train", "Easter", "Magician", "Truck", "Egg", "Manchester", "Uganda", "Eggplant", "Market", "Umbrella", "Egypt", "Match", "Van", "Elephant", "Microphone", "Vase", "Energy", "Monkey", "Vegetable", "Engine", "Morning", "Vulture", "England", "Motorcycle", "Wall", "Evening", "Nail", "Whale", "Eye", "Napkin", "Window", "Family", "Needle", "Wire", "Finland", "Nest", "Xylophone", "Fish", "Nigeria", "Yacht", "Flag", "Night", "Yak", "Flower", "Notebook", "Zebra", "Football", "Ocean", "Zoo", "Forest", "Oil", "Garden", "Fountain", "Orange", "Gas", "France", "Oxygen", "Girl", "Furniture", "Oyster", "Glass", "Garage"]
verbs = ["Accept", "Guess", "Achieve", "Harass", "Add", "Hate", "Admire", "Hear", "Admit", "Help", "Adopt", "Hit", "Advise", "Hope", "Agree", "Identify", "Allow", "Interrupt", "Announce", "Introduce", "Appreciate", "Irritate", "Approve", "Jump", "Argue", "Keep", "Arrive", "Kick", "Ask", "Kiss", "Assist", "Laugh", "Attack", "Learn", "Bake", "Leave", "Bathe", "Lend", "Be", "Lie", "Beat", "Like", "Become", "Listen", "Beg", "Lose", "Behave", "Love", "Bet", "Make", "Boast", "Marry", "Boil", "Measure", "Borrow", "Meet", "Breathe", "Move", "Bring", "Murder", "Build", "Obey", "Burn", "Offend", "Bury", "Offer", "Buy", "Open", "Call", "Paint", "Catch", "Pay", "Challenge", "Pick", "Change", "Play", "Cheat", "Pray", "Chew", "Print", "Choose", "Pull", "Clap", "Punch", "Clean", "Punish", "Collect", "Purchase", "Compare", "Push", "Complain", "Quit", "Confess", "Race", "Confuse", "Read", "Construct", "Relax", "Control", "Remember", "Copy", "Reply", "Count", "Retire", "Create", "Rub", "Cry", "See", "Damage", "Select", "Dance", "Sell", "Deliver", "Send", "Destroy", "Sing", "Disagree", "Snore", "Drag", "Stand", "Drive", "Stare", "Drop", "Start", "Earn", "Stink", "Eat", "Study", "Employ", "Sweep", "Encourage", "Swim", "Enjoy", "Take", "Establish", "Talk", "Estimate", "Teach", "Exercise", "Tear", "Expand", "Tell", "Explain", "Thank", "Fear", "Travel", "Feel", "Type", "Fight", "Understand", "Find", "Use", "Fly", "Visit", "Forget", "Wait", "Forgive", "Walk", "Fry", "Want", "Gather", "Warn", "Get", "Wed", "Give", "Weep", "Glow", "Wink", "Greet", "Worry", "Grow", "Write", "Yell"]

nounVerbNoun :: [(String, String, String)]
nounVerbNoun = [(s1, v, s2) | s1 <- nouns, v <- verbs, s2 <- nouns]

-- 2. What does the following mystery function do? What is its type?
--    It gives back the average length of a word contained in the input string.
--    seekritFunc :: String -> Int
seekritFunc x = div (sum (map length (words x))) (length (words x))

-- 3. We’d really like the answer to be more precise. Can you rewrite that using fractional division?
seekritFunc' :: Fractional a => String -> a
seekritFunc' x = lengthAllWords / numWords
  where allWords = words x
        numWords = fromIntegral . length $ allWords
        lengthAllWords = sum $ map (fromIntegral . length) allWords

-- Rewriting functions using folds
-- 1. myOr returns True if any Bool in the list is True.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2. myAny returns True if a -> Bool applied to any of the values in the list returns True.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr ((||) . f) False

-- 3. Write two versions of myElem. One version should use folding and the other should use any.
myElem :: Eq a => a -> [a] -> Bool
myElem a = foldr ((||) . (== a)) False

myElem' :: Eq a => a -> [a] -> Bool
myElem' a = any (== a)

-- 4. Implement myReverse, don’t worry about trying to make it lazy.
myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

-- 5. Write myMap in terms of foldr. It should have the same behavior as the built-in map.
myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr ((:) . f) []

-- 6. Write myFilter in terms of foldr. It should have the same behavior as the built-in filter.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr filtr []
  where filtr el acc = bool acc (el:acc) (f el)

myFilter' f = foldr (\a b -> bool [] [a] (f a) ++ b) []
myFilter'' f = foldr (\a -> bool id (a:) (f a)) []

-- 7. squish flattens a list of lists into a list
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8. squishMap maps a function over a list and concatenates the results.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr ((++) . f) []

-- 9. squishAgain flattens a list of lists into a list. This time re-use the squishMap function.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- 10. myMaximumBy takes a comparison function and a list and returns the greatest element of the list based on the last value that the comparison returned GT for.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [] = error "failing like Prelude"
myMaximumBy f (a:as) = foldr mmax a (reverse as)
  where mmax el max = bool el max (f max el == GT)

-- 11. myMinimumBy takes a comparison function and a list and returns the least element of the list based on the last value that the comparison returned LT for.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [] = error "failing like Prelude"
myMinimumBy f (a:as) = foldr mmin a (reverse as)
  where mmin el min = bool el min (f min el == LT)
