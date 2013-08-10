module Test where

import Syntax
import Directed

testConstraints = Constraints { allowedOp1s   = [Not, Shl1, Shr1, Shr4, Shr16]
                              , allowedOp2s   = [And, Or, Xor, Plus]
                              , op1sLeftToUse = [] --Not, Shl1, Shr1, Shr4, Shr16]
                              , op2sLeftToUse = [] --And, Or, Xor, Plus]
                              , sizeAvailable = 4
                              , unforcedElements = 1
                              , foldAvailable = False
                              , tfoldAvailable = False
                              }

test = search (PartialProgram Unforced testConstraints)


test2 = do
  p1 <- search (PartialProgram Unforced testConstraints) 1 2
  p2 <- search p1 100 200
  p3 <- search p2 20 40
  return p3
