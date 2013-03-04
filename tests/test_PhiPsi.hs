module Main where

import Talos.PhiPsi

main = do phiPsi <- parsePhiPsiFile "examples/pHIpSI.talosPlus.tab"
          print phiPsi

