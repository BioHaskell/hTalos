module Main where

import Bio.Talos.PhiPsi

main = do phiPsi <- parsePhiPsiFile "examples/pHIpSI.talosPlus.tab"
          print phiPsi

