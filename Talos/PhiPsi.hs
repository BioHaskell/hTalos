{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Talos.PhiPsi where

import qualified Data.ByteString.Char8 as BS

import Rosetta.SS

data PhiPsi = PhiPsi { phi, psi :: Double
                     , resId    :: Int
                     , resName  :: BS.ByteString
                     , ss       :: SSCode
                     }
  deriving (Eq, Show) -- add read later...

parsePhiPsi :: BS.ByteString -> BS.ByteString -> [PhiPsi] 
parsePhiPsi fname input = []
  where
    lines = BS.lines input

parsePhiPsiFile fname = parsePhiPsi (BS.pack fname) `fmap` BS.readFile fname
                        

