{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-
 - Copyright 2011-2015 Per Magnus Therning
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

module PkgTypes where

import           Control.Applicative
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Aeson.TH (deriveJSON)
import           Data.Text (unpack)
import qualified Data.Version as DV
import qualified Distribution.Package as P
import           Distribution.PackageDescription
import qualified Distribution.Version as V
import           Text.ParserCombinators.ReadP (readP_to_S)

data Pkg = GhcPkg GhcPkgD
         | DistroPkg DistroPkgD
         | RepoPkg RepoPkgD
         deriving (Eq, Show)

data GhcPkgD = GhcPkgD { gpVersion :: V.Version }
             deriving (Eq, Show)

data DistroPkgD = DistroPkgD
                  { dpVersion :: V.Version
                  , dpXrev :: Int
                  , dpRelease :: Int
                  } deriving (Eq, Show)

data RepoPkgD = RepoPkgD
                { rpVersion :: V.Version
                , rpXrev :: Int
                , rpDeps :: [P.Dependency]
                , rpFlags :: FlagAssignment
                , rpRelease :: Int
                } deriving (Eq, Show)

data CblPkg = CP String Pkg
            deriving (Eq, Show)

type CblDB = [CblPkg]

instance Ord CblPkg where
  compare (CP n1 (GhcPkg d1)) (CP n2 (GhcPkg d2)) =
    compare (n1, gpVersion d1) (n2, gpVersion d2)
  compare (CP _ GhcPkg {}) _ = LT
  compare _ (CP _ GhcPkg {}) = GT

  compare (CP n1 (DistroPkg d1)) (CP n2 (DistroPkg d2)) =
    compare (n1, dpVersion d1, dpRelease d1) (n2, dpVersion d2, dpRelease d2)
  compare (CP _ DistroPkg {}) _ = LT
  compare _ (CP _ DistroPkg {}) = GT

  compare (CP n1 (RepoPkg d1)) (CP n2 (RepoPkg d2)) =
    compare (n1, rpVersion d1, rpRelease d1) (n2, rpVersion d2, rpRelease d2)

-- JSON instances
instance ToJSON V.Version where
  toJSON = toJSON . DV.showVersion

instance FromJSON V.Version where
  parseJSON = withText "Version" $ go . readP_to_S DV.parseVersion . unpack
    where
      go [(v,[])] = return v
      go (_ : xs) = go xs
      go _        = fail $ "could not parse Version"

instance ToJSON FlagName where
  toJSON (FlagName s) = toJSON s

instance FromJSON FlagName where
  parseJSON = fmap FlagName . parseJSON

instance ToJSON V.VersionRange where
  toJSON = V.foldVersionRange
    (object ["AnyVersion" .= ([]::[(Int,Int)])])
    (\ v -> object ["ThisVersion" .= v])
    (\ v -> object ["LaterVersion" .= v])
    (\ v -> object ["EarlierVersion" .= v])
    (\ vr0 vr1 -> object ["UnionVersionRanges" .= [vr0, vr1]])
    (\ vr0 vr1 -> object ["IntersectVersionRanges" .= [vr0, vr1]])

instance FromJSON V.VersionRange where
  parseJSON = withObject "VersionRange" go
    where
      go o =
        V.thisVersion <$> o .: "ThisVersion" <|>
        V.laterVersion <$> o .: "LaterVersion" <|>
        V.earlierVersion <$> o .: "EarlierVersion" <|>
        V.WildcardVersion <$> o .: "WildcardVersion" <|>
        nullaryOp V.anyVersion <$> o .: "AnyVersion" <|>
        binaryOp V.unionVersionRanges <$> o .: "UnionVersionRanges" <|>
        binaryOp V.intersectVersionRanges <$> o .: "IntersectVersionRanges" <|>
        V.VersionRangeParens <$> o .: "VersionRangeParens"

      nullaryOp :: a -> Value -> a
      nullaryOp = const

      binaryOp f [a, b] = f a b

instance ToJSON P.Dependency where
  toJSON (P.Dependency pn vr) = toJSON (P.unPackageName pn, vr)

instance FromJSON P.Dependency where
  parseJSON v = do
    (pn, vr) <- parseJSON v
    return $ P.Dependency (P.PackageName pn) vr

$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''Pkg)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''GhcPkgD)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''DistroPkgD)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''RepoPkgD)
$(deriveJSON defaultOptions { sumEncoding = ObjectWithSingleField, allNullaryToStringTag = False } ''CblPkg)
