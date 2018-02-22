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
 
type Msg = (Int, String)
 
main :: IO ()
main = do
    chan <- newChan
    sock <- socket AF_INET Stream 0
    setSocketOption sock ReuseAddr 1
    bindSocket sock (SockAddrInet 4242 iNADDR_ANY)
    listen sock 2
    forkIO $ fix $ \loop -> do
        (_, msg) <- readChan chan
        loop
    mainLoop sock chan 0 0

-- add counter in param	
mainLoop :: Socket -> Chan Msg -> Int -> Int -> IO ()
mainLoop sock chan nr c = do
	cchan <- newChan
	if nr == 0 then writeChan cchan 0 else writeChan cchan nr
	conn <- accept sock
	forkIO (runConn conn chan cchan nr) -- forkIO (runConn conn chan nr)
	mainLoop sock chan (nr+1) c
	
--runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()	
runConn :: (Socket, SockAddr) -> Chan Msg -> Chan Int -> Int -> IO ()
runConn (sock, _) chan cchan nr = do 
	let broadcast msg = writeChan chan (nr, msg)
	hdl <- socketToHandle sock ReadWriteMode
	hSetBuffering hdl LineBuffering
	hPutStrLn hdl "Hi, what's your name?"
	name <- hGetLine hdl
	broadcast ("--> " ++ name ++ " entered.")
	hPutStrLn hdl ("Welcome, " ++ name ++ "!")
	chan' <- dupChan chan
	reader <- forkIO $ fix $ \loop -> do
		(nr', line) <- readChan chan'
		when (nr /= nr') $ hPutStrLn hdl line
		loop
	handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
		counter <- (readChan cchan)::IO Int
		if counter == 0
		then do 
			hPutStrLn hdl "Choose an algorithm: \n \
				\0: Caesar Cipher \n \
				\1: Monoalphabetic Substitution Cipher \n \
				\2: Polyalphabetic Substitution Cipher"
			x <- getAlgorithm hdl
			writeChan cchan (counter+1)
			loop
		else do
			writeChan cchan (counter+1)
			line <- hGetLine hdl
			putStrLn "Message received."	
			case line of
			 "quit" -> hPutStrLn hdl "Bye!"
			 _      -> do
				putStrLn $ "Name: "++name
				broadcast (name ++ ": " ++ line)
                                loop
	killThread reader
	broadcast ("<-- " ++ name ++ " left.")
	hClose hdl

getAlgorithm :: Handle -> IO (Int)
getAlgorithm hdl = do
	hSetBuffering hdl LineBuffering
	msg <- hGetLine hdl
	let msg' = read msg :: Int
	if (msg' `elem` [0..2]) == False
	 then putStrLn "Invalid entry!"
	 else putStrLn "Good job!"
	return (msg')
