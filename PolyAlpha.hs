{-       Matthew Tennis & Romain Brunie       -}
{- 	           CSCI 3300 Project 		      -}
{-     Simple Crypto-Messenger Application    -}

{- Polyalphabetic cipher module takes plaintext
   and [Char] key, outputs encrypted ciphertext. 
   -} 

module PolyAlpha where

import Data.Char

-- PolyAlpha encryption
-- cycle repeat key
encrypt :: String -> String -> [Char]
encrypt text key = [ shift (convertCharInt k) t | (k,t) <- zip (cycle key) text ]

-- PolyAlpha decryption
decrypt :: String -> String -> [Char]
decrypt text key = [ shift (26 - convertCharInt k) t | (k,t) <- zip (cycle key) text]

-- this shift letter by n places modulo 26
shift :: Int -> Char -> Char
shift n c | isLower c = convertIntChar ((convertCharInt c + n) `mod` 26)
          | otherwise = c

-- this converts letter to int
convertCharInt :: Char -> Int
convertCharInt c = ord c - 97

-- this converts an int to letter
convertIntChar :: Int -> Char
convertIntChar i = chr (97 + i)




