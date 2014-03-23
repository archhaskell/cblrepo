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

module Urls where

import Util.Misc

import Control.Monad.Reader

urls :: Command ()
urls = do
    pkgs <- optGet $ pkgVers . optsCmd
    liftIO $ mapM_ (putStrLn . createUrl) pkgs

createUrl (pkg, ver) = "http://hackage.haskell.org/packages/archive/" ++ pkg ++ "/" ++ ver ++ "/" ++ pkg ++ ".cabal"
