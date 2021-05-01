module PE4 where

import Data.Maybe -- up to you if you want to use it or not

-- Generic DictTree definition with two type arguments
data DictTree k v = Node [(k, DictTree k v)] | Leaf v deriving Show

-- Lightweight Char wrapper as a 'safe' Digit type
newtype Digit = Digit Char deriving (Show, Eq, Ord) -- derive equality and comparison too!

-- Type aliases
type DigitTree = DictTree Digit String
type PhoneNumber = [Digit]


---------------------------------------------------------------------------------------------
------------------------- DO NOT CHANGE ABOVE OR FUNCTION SIGNATURES-------------------------
--------------- DUMMY IMPLEMENTATIONS ARE GIVEN TO PROVIDE A COMPILABLE TEMPLATE ------------
--------------------- REPLACE THEM WITH YOUR COMPILABLE IMPLEMENTATIONS ---------------------
---------------------------------------------------------------------------------------------


----------
-- Part I:
-- Some Maybe fun! Basic practice with an existing custom datatype.

-- toDigit: Safely convert a character to a digit
toDigit :: Char -> Maybe Digit
toDigit char
    | char == '0' || char == '1' || char == '2' || char == '3' || char == '4' = Just (Digit char)
    | char == '5' || char == '6' || char == '7' || char == '8' || char == '9' = Just (Digit char)
    | otherwise = Nothing

-- toDigits: Safely convert a bunch of characters to a list of digits.
--           Particularly, an empty string should fail.
toDigits :: String -> Maybe PhoneNumber
toDigits str
    | str == "" = Nothing
    | otherwise = toDigits_helper str []

toDigits_helper str digits
    | str == "" && digits == [] = Nothing
    | str == "" && digits /= [] = Just digits
    | toDigit (head str) == Nothing = Nothing
    | otherwise = toDigits_helper (tail str) (digits ++ [Digit (head str)])

-----------
-- Part II:
-- Some phonebook business.

-- numContacts: Count the number of contacts in the phonebook...
numContacts :: DigitTree -> Int

numContacts (Node []) = 0
numContacts (Node lst) = numContacts (snd (head lst)) + numContacts ((Node (drop 1 lst)))
numContacts (Leaf _) = 1

-- getContacts: Generate the contacts and their phone numbers in order given a tree. 
getContacts :: DigitTree -> [(PhoneNumber, String)]
getContacts tree = getContacts_helper tree []

getContacts_helper (Node []) digits = []
getContacts_helper (Node lst) digits = getContacts_helper (snd (head lst)) (digits ++ [fst (head lst)]) ++ getContacts_helper ((Node (drop 1 lst))) digits
getContacts_helper (Leaf name) digits = [(digits,name)]

-- autocomplete: Create an autocomplete list of contacts given a prefix
-- e.g. autocomplete "32" areaCodes -> 
--      [([Digit '2'], "Adana"), ([Digit '6'], "Hatay"), ([Digit '8'], "Osmaniye")]
autocomplete :: String -> DigitTree -> [(PhoneNumber, String)]
autocomplete _ _ = undefined


-----------
-- Example Trees
-- Two example trees to play around with, including THE exampleTree from the text. 
-- Feel free to delete these or change their names or whatever!

exampleTree :: DigitTree
exampleTree = Node [
    (Digit '1', Node [
        (Digit '3', Node [
            (Digit '7', Node [
                (Digit '8', Leaf "Jones")])]),
        (Digit '5', Leaf "Steele"),
        (Digit '9', Node [
            (Digit '1', Leaf "Marlow"),
            (Digit '2', Node [
                (Digit '3', Leaf "Stewart")])])]),
    (Digit '3', Leaf "Church"),
    (Digit '7', Node [
        (Digit '2', Leaf "Curry"),
        (Digit '7', Leaf "Hughes")])]

areaCodes :: DigitTree
areaCodes = Node [
    (Digit '3', Node [
        (Digit '1', Node [
            (Digit '2', Leaf "Ankara")]),
        (Digit '2', Node [
            (Digit '2', Leaf "Adana"),
            (Digit '6', Leaf "Hatay"),
            (Digit '8', Leaf "Osmaniye")])]),
    (Digit '4', Node [
        (Digit '6', Node [
            (Digit '6', Leaf "Artvin")])])]

