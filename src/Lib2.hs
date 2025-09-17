{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1

-- | Parses user's input.
-- The function must have tests.
parseCommand :: String -> Either String Lib1.Command
parseCommand _ = Left "Not implemented"

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand _ = "Not implemented"

instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  _ == _ = False
