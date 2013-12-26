--
-- convT
-- Converter temperature values​​.
--
-- File: main.hs
--
-- Copyright (C) 2013 Artem Petrov <pa2311@gmail.com>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
-- GNU General Public License for more details.
-- You should have received a copy of the GNU General Public License
-- along with this program. If not, see <http://www.gnu.org/licenses/>.
--

import System.Environment
import Control.Exception
import Text.Printf

version :: String
version = "v0.2.0"

about :: String
about = printf "\n%s %s\n%s\n" "convT" version "Converter temperature values."

main = do
    putStrLn about
    putStrLn "Enter temperature value: "
    valStr <- getLine
    putStrLn "Enter temperature dimension: "
    dimStr <- getLine
    putStrLn (doConv valStr dimStr) `catch` handleErr

handleErr :: SomeException -> IO ()
handleErr e = putStrLn "An error has occurred. Be careful when entering data!"

doConv :: String -> String -> String
doConv valStr dimStr = conv (read valStr) (head dimStr)

conv :: Double -> Char -> String
conv valStr 'C' = "Temperature = " ++ (printf "%.3f" (cToF valStr)) ++ " F"
conv valStr 'F' = "Temperature = " ++ (printf "%.3f" (fToC valStr)) ++ " C"
conv valStr dimStr = "Unknown dimension! Be careful when entering data!"

cToF :: Double -> Double
cToF tC = tC * 9 / 5 + 32

fToC :: Double -> Double
fToC tF = (tF - 32) * 5 / 9
