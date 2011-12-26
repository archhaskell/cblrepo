{-
 - Copyright 2011 Per Magnus Therning
 -
 - Licensed under the Apache License, Version 2.0 (the "License");
 - you may not use this file except in compliance with the License.
 - You may obtain a copy of the License at
 -
 -     http://www.apache.org/licenses/LICENSE-2.0
 -
 - Unless required by applicable law or agreed to in writing, software
 - distributed under the License is distributed on an "AS IS" BASIS,
 - WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 - See the License for the specific language governing permissions and
 - limitations under the License.
 -}

module ConvertDB where

-- {{{1 imports
-- {{{2 local
import Util.Misc
import qualified OldPkgDB as ODB
import qualified PkgDB as NDB

-- {{{2 system
import Control.Monad.Reader
import Data.Version
import System.IO

convertDb :: Command ()
convertDb = do
    inDb <- cfgGet inDbFile >>= \ fn -> liftIO $ ODB.readDb fn
    outDbFn <- cfgGet outDbFile
    newDb <- liftIO $ mapM doConvert inDb
    liftIO $ NDB.saveDb newDb outDbFn

doConvert :: ODB.CblPkg -> IO NDB.CblPkg
doConvert opkg@(n, (v, d, r))
    | ODB.isBasePkg opkg = let
            withNoStdBuffering f = do
                old <- hGetBuffering stdin
                hSetBuffering stdin NoBuffering
                result <- f
                hSetBuffering stdin old
                return result
            getValidChar = do
                putStr " (g)hc or (d)istro? " >> hFlush stdout
                getChar >>= (\ c -> putStrLn "" >> return c) >>= (\ c -> if c `elem` "gd" then return c else getValidChar)
            createPkg c
                | c == 'g' = return $ NDB.createGhcPkg n v
                | c == 'd' = do
                    putStr " release? " >> hFlush stdout
                    rel <- getLine
                    return $ NDB.createDistroPkg n v rel
        in do
            putStr n
            withNoStdBuffering $ getValidChar >>= createPkg
    | otherwise = return $ NDB.createRepoPkg n v d (show r)
