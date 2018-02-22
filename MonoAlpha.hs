{-       Matthew Tennis & Romain Brunie       -}
{- 	           CSCI 3300 Project 		      -}
{-     Simple Crypto-Messenger Application    -}

{- Monoalphabetic cipher module takes plaintext
   string and an integer key; outputs encrypted 
   ciphertext.       		  				  -}

module MonoAlpha where

import Data.Char

-- Takes: plaintext and integer key; output ciphertext

encrypt :: String -> Int -> String
encrypt p k = encrypt' (filter isAlphaNum p) k

encrypt' :: String -> Int -> String
encrypt' (p:[]) k = [chr $ shift p k]
encrypt' (p:ps) k = (chr $ shift p k):(encrypt' ps k)  

-- Takes: ciphertext and integer key; output plaintext

decrypt :: String -> Int -> String
decrypt c k = decrypt' (filter isAlphaNum c) k

decrypt' :: String -> Int -> String
decrypt' (c:[]) k  = [chr $ shift' c k]
decrypt' (c:cs) k  = (chr $ shift' c k):(decrypt' cs k)

-- Char elem of text to be shifted 'k' indices.
-- Need to implement step to get proper wrap-around

shift :: Char -> Int -> Int
shift i k | isDigit i = ord $ numberWrap i k
		  | isLower i = ord $ lowerWrap i k
		  | isUpper i = ord $ upperWrap i k
		  | otherwise = error "Invalid char." -- Append char? ++ [i]

shift' :: Char -> Int -> Int
shift' i k | isDigit i = ord $ numberWrap' i k
		   | isLower i = ord $ lowerWrap' i k
		   | isUpper i = ord $ upperWrap' i k
		   | otherwise = error "Invalid char." -- Append char? ++ [i]
		  
-- For alphabet wrap around: ASCII 65-90 && 97-122

numberWrap :: Char -> Int -> Char
numberWrap c 0 = c
numberWrap c k | ord c + 1 == 58 = numberWrap '0' $ k - 1
			   | otherwise 		 = numberWrap (chr $ ord c + 1) $ k - 1

-- decrypt wrap
numberWrap' :: Char -> Int -> Char
numberWrap' c 0 = c
numberWrap' c k | ord c - 1 == 47 = numberWrap' '9' $ k - 1
				| otherwise 	  = numberWrap' (chr $ ord c - 1) $ k - 1
			   
lowerWrap :: Char -> Int -> Char
lowerWrap c 0 = c
lowerWrap c k | ord c + 1 == 123 = lowerWrap 'a' $ k - 1
			  | otherwise 		 = lowerWrap (chr $ ord c + 1) $ k - 1

lowerWrap' :: Char -> Int -> Char
lowerWrap' c 0 = c
lowerWrap' c k | ord c - 1 == 96 = lowerWrap' 'z' $ k - 1
			   | otherwise 		 = lowerWrap' (chr $ ord c - 1) $ k - 1
			  
upperWrap :: Char -> Int -> Char
upperWrap c 0 = c
upperWrap c k | ord c + 1 == 91 = upperWrap 'A' $ k - 1
			  | otherwise 		= upperWrap (chr $ ord c + 1) $ k - 1
			  
upperWrap' :: Char -> Int -> Char
upperWrap' c 0 = c
upperWrap' c k | ord c - 1 == 64 = upperWrap' 'Z' $ k - 1
			   | otherwise 		 = upperWrap' (chr $ ord c - 1) $ k - 1