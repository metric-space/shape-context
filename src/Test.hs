module Test (tests) where

import Matrix
import Test.HUnit


tests :: IO ()
tests =  runTestTTAndExit . test  $ [
  "Matrix size test" ~: "Simple test" ~: (size . Matrix $ [[2,3],[4,5]]) ~=? (2,2),

  "rowInc test" ~: "Property test" ~: (let m = Matrix [[(2,3),(4,8)],[(7,3),(4,5)]]
                                       in (fmap ((rowInc 1) . (rowInc (-1))) m) ~=? m),

  "colInc test" ~: "Property test" ~: (let m = Matrix [[(2,3),(4,8)],[(7,3),(4,5)]]
                                       in (fmap ((colInc 1) . (colInc (-1))) m) ~=? m),

  "indexMatrix test" ~: "generation of 2x2" ~: (indexMatrix (2,2)) ~=? (Matrix [[(0,0),(0,1)],
                                                                                [(1,0),(1,1)]]),

  "indexMatrix test" ~: "generation of 3x3" ~: (indexMatrix (3,3)) ~=? (Matrix [[(0,0),(0,1),(0,2)],
                                                                                [(1,0),(1,1),(1,2)],
                                                                                [(2,0),(2,1),(2,2)]])
  ]

