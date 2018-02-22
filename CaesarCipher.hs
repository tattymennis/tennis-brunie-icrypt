{-       Matthew Tennis & Romain Brunie       -}
{- 	           CSCI 3300 Project 		      -}
{-     Simple Crypto-Messenger Application    -}

{- Caesar Cipher module: one of the first and -}
{- most basic ciphers. Only operates on 	  -}
{- alphabetic characters in upper case.       -}
module CaesarCipher where

import Data.Char

-- Takes: plaintext and integer key; output ciphertext

encrypt :: String -> Int -> String
encrypt p k = encrypt' (map toUpper $ filter isAlpha p) k

encrypt' :: String -> Int -> String
encrypt' (p:[]) k = [chr $ shift p k]
encrypt' (p:ps) k = (chr $ shift p k):(encrypt' ps k)  

-- Takes: ciphertext and integer key; output plaintext

decrypt :: String -> Int -> String
decrypt c k = decrypt' (map toUpper $ filter isAlphaNum c) k

decrypt' :: String -> Int -> String
decrypt' (c:[]) k = [chr $ shift' c k]
decrypt' (c:cs) k = (chr $ shift' c k):decrypt' cs k

-- Char elem of text to be shifted 'k' indices.
-- shift for forward shift (encryption)
-- shift' for backward shift (decryption)

shift :: Char -> Int -> Int
shift i k = ord $ upperWrap i k

shift' i k = ord $ upperWrap' i k

upperWrap :: Char -> Int -> Char
upperWrap c 0 = c
upperWrap c k | ord c + 1 == 91 = upperWrap 'A' $ k - 1
			  | otherwise 		= upperWrap (chr $ ord c + 1) $ k - 1

upperWrap' :: Char -> Int -> Char			  
upperWrap' c 0 = c
upperWrap' c k | ord c - 1 == 64 = upperWrap' 'Z' $ k - 1
			  | otherwise 		= upperWrap' (chr $ ord c - 1) $ k - 1