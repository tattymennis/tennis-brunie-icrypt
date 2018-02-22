{-       Matthew Tennis & Romain Brunie       -}
{- 	           CSCI 3300 Project 		      -}
{-     Simple Crypto-Messenger Application    -}

import Network.Socket
import System.IO
import Control.Exception
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Monad
import Control.Monad.Fix (fix)
import qualified CaesarCipher (encrypt, decrypt)
import qualified MonoAlpha (encrypt, decrypt)
import qualified PolyAlpha (encrypt, decrypt)

data Sum a b = 
  InL a
 |InR b

main :: IO ()
main = withSocketsDo $ 
	do
		chan <- newChan
		addrinfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "4242")
		let serveraddr = head addrinfos       
		sock <- socket (addrFamily serveraddr) Stream defaultProtocol       
		connect sock (addrAddress serveraddr)
		putStrLn "Connected"
		hdl <- socketToHandle sock ReadWriteMode
		hSetBuffering hdl LineBuffering

		-- for Name
		reader <- forkIO $ fix $ \foo -> do
			msg <- getLine
			hPutStrLn hdl msg

		-- broken, asks every client, not just first.
		-- for cipher algo selection
		reader <- forkIO $ fix $ \foo -> do
			msg <- getLine
			hPutStrLn hdl msg
			
		-- for messages
		reader <- forkIO $ fix $ \loop -> do
		   msg <- getLine
		   if msg == "quit"
		   then hPutStrLn hdl "quit"
		   else hPutStrLn hdl (algoHelper' 2 msg $ InR "password")
		   loop
		handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
			   line <- hGetLine hdl
			   putStrLn line
			   loop
		killThread reader
		hClose hdl
				
getAlgorithm :: Handle -> IO (Int)
getAlgorithm hdl = do
	hSetBuffering hdl LineBuffering
	msg <- hGetLine hdl
	let msg' = read msg :: Int
	if (msg' `elem` [0..2]) == False
	 then putStrLn "Wrong choice; selecting #2"
	 else putStrLn "Wrong choice; selecting #2"
	return (msg')
	
--algoHelper :: Int -> (String -> Int -> String)
algoHelper 0 = CaesarCipher.encrypt
algoHelper 1 = MonoAlpha.encrypt
algoHelper _ = error "ERROR" 

algoHelper' :: Int -> String -> Sum Int String -> String
algoHelper' 0 m (InL n) = CaesarCipher.encrypt m n
algoHelper' 1 m (InL n) = MonoAlpha.encrypt m n
algoHelper' 2 m (InR s) = PolyAlpha.encrypt m s
algoHelper' _ _ _ 		= error "Invalid Sum Type"

getIntKey :: Int -> Int
getIntKey i = i

getStrKey :: String -> String
getStrKey s | s == ""   = error "String key is empty!"
			| otherwise = s