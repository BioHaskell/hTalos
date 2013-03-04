{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Talos.PhiPsi( parsePhiPsi
                   , parsePhiPsiFile
                   , PhiPsi(..)      ) where

import Control.Monad.Instances()
import Data.Typeable
import Data.List(partition)
import Data.Either(partitionEithers)

import qualified Data.ByteString.Char8 as BS

import Rosetta.SS

data PhiPsi = PhiPsi { phi, psi :: Double
                     , resId    :: Int
                     , resName  :: BS.ByteString
                     , ss       :: SSCode
                     }
  deriving (Eq, Show) -- add read later...

parsePhiPsi :: BS.ByteString -> BS.ByteString -> ( [PhiPsi]
                                                 , [String] )
parsePhiPsi fname input = (goodRecords, errors)
  where
    -- error handling:
    errors = reports "Expected five columns in PhiPsi record, but found: " recordsOfInvalidLength ++
             map withErrorSource recordParseErrors
    reports msg ls = map (\x -> withErrorSource $ msg ++ show x) ls 
    withErrorSource msg = "Error parsing " ++ BS.unpack fname ++ ": " ++ msg

    -- filter out comments
    (_comments, maybeRecords) = partition ((=='#') . BS.head) $ BS.lines input
    -- filter out empty lines
    records = filter (/=[]) . map BS.words $ maybeRecords
    -- filter out records of bad length
    (recordsOfGoodLength, recordsOfInvalidLength) = partition ((==5) . length) records
    -- split off parse errors
    (recordParseErrors,   goodRecords           ) = partitionEithers . map parseRecord $ recordsOfGoodLength
    parseRecord [phiStr, psiStr, resIdStr, resNameStr, ssStr] =
        do aPhi     <- parseCol phiStr
           aPsi     <- parseCol psiStr
           aResId   <- parseCol resIdStr
           aSS      <- parseSS  ssStr
           return $ PhiPsi aPhi aPsi aResId resNameStr aSS
    parseCol :: (Read a, Typeable a) => BS.ByteString -> Either String a
    parseCol c = case reads $ BS.unpack c of
                   ((result, []):_) -> return result
                   l                -> let typeHolder = fst $ head l
                                       in colError c typeHolder
    parseSS :: BS.ByteString -> Either String SSCode
    parseSS "random" = return Loop -- not sure?
    parseSS "beta"   = return Strand
    parseSS "alpha"  = return Helix
    parseSS c        = colError c Helix
    colError c t = fail ( "Cannot parse column " ++
                          (show . BS.unpack) c   ++
                          " as "                 ++
                          (show . typeOf) t ) -- t is never evaluated - just provides the type.

parsePhiPsiFile fname = parsePhiPsi (BS.pack fname) `fmap` BS.readFile fname
                        

