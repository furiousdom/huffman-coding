-- Full name: Dominik Tabak
-- AAU e-mail: dtabak19@student.aau.dk

import Data.List

-- This is a recursive datatype that is used to create the binary
-- tree for the prefix code.
data BTree = BLeaf (Char, Int) | BBranch (Int, BTree, BTree)
                                                        deriving Show

-- This is a "main" function which chains together expressions and is used
-- as the entry point of this program.
main = do
    putStrLn "Enter text to be compressed, please."
    textToEncode <- getLine
    let charTuples = countOccurences textToEncode (uniqChars textToEncode)
    let leaves = makeLeaves charTuples
    let sortedLeaves = sortOn getOccuranceFrequency leaves
    let tree = makeTree sortedLeaves
    let encodedText = encodeText textToEncode tree
    let deocdedText = decode encodedText tree
    putStrLn ("Original message: " ++ textToEncode)
    putStrLn ("Bit stream: " ++ encodedText)
    putStrLn ("Decoded message: " ++ deocdedText)

-- Function which acts as a calling interface. It calls the
-- "uniqCharsHelper" function but adds an empty list as an
-- additional parameter.
uniqChars :: [Char] -> [Char]
uniqChars text = uniqCharsHelper text []

-- This function returns a list of all the characters that appear in a
-- given string. No matter the number of occurances the character is
-- added only once to the "acc" variable
uniqCharsHelper :: [Char] -> [Char] -> [Char]
uniqCharsHelper [] acc = acc
uniqCharsHelper text acc = if any (\x -> x == (head text)) acc
                            then
                                uniqCharsHelper (tail text) acc
                            else
                                uniqCharsHelper (tail text) (acc ++ [head text])

-- Function which acts as a calling interface. It calls the
-- "countOccurencesHelper" function with an empty list as an
-- additional paramter.
countOccurences :: [Char] -> [Char] -> [(Char, Int)]
countOccurences text chars = countOccurencesHelper text chars []

-- If given a string "text" and a list of characters "chars" that appear
-- in the text, this function counts the number of appearances of every
-- character in the "chars" list. Then it creates a tuple of the
-- character and its number of occurences in the text and adds that to
-- the list of tuples "acc"
countOccurencesHelper :: [Char] -> [Char] -> [(Char, Int)] -> [(Char, Int)]
countOccurencesHelper text [] acc = acc
countOccurencesHelper text chars acc = let
                                        occurences =  length (filter (\x -> x == (head chars)) text)
                                        in
                                            if occurences /= 0
                                                then
                                                    countOccurencesHelper text (tail chars) acc ++ [((head chars), occurences)]
                                                else
                                                    countOccurencesHelper text (tail chars) acc

-- This function creates a list of BTree values from a list of tuples
-- using the BLeaf term constructor.
makeLeaves :: [(Char, Int)] -> [BTree]
makeLeaves [] = []
makeLeaves (x:xs) = BLeaf (fst x, snd x) : makeLeaves xs

-- This function given either a BTree value returns the integer, that
-- represents the number of occurences.
getOccuranceFrequency :: BTree -> Int
getOccuranceFrequency (BLeaf (a, b)) = b
getOccuranceFrequency (BBranch (b, c1, c2)) = b

-- Given two BTree values creates another BTree value using BBranch
-- term constructor.
makeBranch :: BTree -> BTree -> BTree
makeBranch (BLeaf (a0, b0)) (BLeaf (a1, b1)) = BBranch (b0 + b1, BLeaf (a0, b0), BLeaf (a1, b1))
makeBranch (BLeaf (a0, b0)) (BBranch (b1, c0, c1)) = BBranch (b0 + b1, BLeaf (a0, b0), (BBranch (b1, c0, c1)))
makeBranch (BBranch (b0, c0, c1)) (BLeaf (a1, b1)) = BBranch (b0 + b1, BBranch (b0, c0, c1), BLeaf (a1, b1))
makeBranch (BBranch (b0, c0, c1)) (BBranch (b1, c2, c3)) = BBranch (b0 + b1, BBranch (b0, c0, c1), BBranch (b1, c2, c3))

-- This function sorts a list of BTree values by their integer value
-- which represents frequency of occurences.
sortTreeList :: [BTree] -> [BTree]
sortTreeList list = sortOn getOccuranceFrequency list

-- Given a list of BTree values this function creates a binary tree
-- structure that represented by values of the recursive datatype, BTree.
makeTree :: [BTree] -> BTree
makeTree (x1:x2:[]) = makeBranch x1 x2
makeTree (x1:x2:xs) = makeTree (sortTreeList ((makeBranch x1 x2) : xs))

-- For every character of a string the getEncoding function is called.
-- The bit sequences for every character are appended and that string
-- is returned at when there are no more characters to encode.
encodeText :: [Char] -> BTree -> [Char]
encodeText [] tree = ""
encodeText (x:xs) tree = (getEncoding x tree "") ++ encodeText xs tree

-- This functions returns the encoding of a single character by
-- traversing the tree structure and treating left child of a
-- branch as a "0" and right side as a "1".
getEncoding :: Char -> BTree -> [Char] -> [Char]
getEncoding sign (BLeaf (a, b)) encoding = if a /= sign
                                        then
                                            ""
                                        else
                                            encoding
getEncoding sign (BBranch (b, c, d)) encoding = let
                                                    tempEncoding = getEncoding sign c (encoding ++ "0")
                                                in
                                                    if tempEncoding /= ""
                                                        then
                                                            tempEncoding
                                                        else
                                                            getEncoding sign d (encoding ++ "1")

-- Function which acts as a calling interface. It calls the
-- "decodeHelper" function with a duplicate "treeRoot" parameter.
decode :: [Char] -> BTree -> [Char]
decode encoding treeRoot = decodeHelper encoding treeRoot treeRoot

-- This function returns the message that was encoded by taking
-- a bit sequence and removing a single constituent in every function
-- call and traversing the tree structure until it reaches a
-- tree leaf. It then restarts the whole process from the top of the
-- tree structure until there are no more bits to decode.
decodeHelper :: [Char] -> BTree -> BTree -> [Char]
decodeHelper [] (BLeaf (a, b)) root = a : []
decodeHelper encoding (BLeaf (a, b)) root = a : (decodeHelper encoding root root)
decodeHelper encoding (BBranch (b, c, d)) root = if (head encoding) == '0'
                                        then
                                            decodeHelper (tail encoding) c root
                                        else
                                            decodeHelper (tail encoding) d root
