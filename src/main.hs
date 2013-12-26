import System.Environment
import Control.Exception

main = do
     putStrLn "Enter temperature value: "
     valStr <- getLine
     putStrLn "Enter temperature dimension: "
     dimStr <- getLine
     putStrLn (doConv valStr dimStr) `catch` handleErr

handleErr :: SomeException -> IO ()
handleErr e = putStrLn "An error has occurred. Be careful when entering data!"

doConv :: String -> String -> String
doConv valStr dimStr = conv valStr (head dimStr)

conv :: String -> Char -> String
conv valStr 'C' = "Temperature = " ++ show (cToF (read valStr)) ++ " F"
conv valStr 'F' = "Temperature = " ++ show (fToC (read valStr)) ++ " C"
conv valStr dimStr = "Unknown dimension! Be careful when entering data!"

cToF :: Double -> Double
cToF tC = tC * 9 / 5 + 32

fToC :: Double -> Double
fToC tF = (tF - 32) * 5 / 9
