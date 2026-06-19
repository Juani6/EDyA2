module ArrTests where

import Test.HUnit
import Seq
import Arr        (Arr)
import ArrSeq


s0, s1, s2, s3 :: Arr Int
s0 = fromList []
s1 = fromList [4]
s2 = fromList [5,1]
s3 = fromList [6,3,4]

-- nthS
testNthSeq :: Test
testNthSeq =
  TestCase $ assertEqual "Error on nthS"
                         3 (nthS s3 1)

-- mapS con función no trivial
testMapSquareSeq :: Test
testMapSquareSeq =
  TestCase $ assertEqual "Error on mapS square"
                         (fromList [36,9,16]) (mapS (^2) s3)

-- reduceS con longitud 1
testReduceSumSeq1 :: Test
testReduceSumSeq1 =
  TestCase $ assertEqual "Error reducing sequence of length 1"
                         4 (reduceS (+) 0 s1)

-- reduceS con longitud 2
testReduceSumSeq2 :: Test
testReduceSumSeq2 =
  TestCase $ assertEqual "Error reducing sequence of length 2"
                         6 (reduceS (+) 0 s2)

-- reduceS no conmutativo (resta)
testReduceSubSeq :: Test
testReduceSubSeq =
  TestCase $ assertEqual "Error reducing with non-commutative op"
                         (0 - ((6 - 3) - 4)) (reduceS (-) 0 s3)

-- scanS con longitud 1
testScanSumSeq1 :: Test
testScanSumSeq1 =
  TestCase $ assertEqual "Error on scan for sequence of length 1"
                         (fromList [0], 4) (scanS (+) 0 s1)

-- scanS con longitud 2
testScanSumSeq2 :: Test
testScanSumSeq2 =
  TestCase $ assertEqual "Error on scan for sequence of length 2"
                         (fromList [0,5], 6) (scanS (+) 0 s2)

-- scanS el resultado acumulado es correcto (res == reduceS)
testScanReduceConsistency :: Test
testScanReduceConsistency =
  TestCase $ assertEqual "scanS total should match reduceS"
                         (reduceS (+) 0 s3) (snd (scanS (+) 0 s3))

-- filterS
testFilterEmptySeq :: Test
testFilterEmptySeq =
  TestCase $ assertEqual "Error on empty sequence filter"
                         s0 (filterS (const True) s0)

testFilterNonEmptySeq :: Test
testFilterNonEmptySeq =
  TestCase $ assertEqual "Error on filterS keeping evens"
                         (fromList [6,4]) (filterS even s3)

testFilterAllOut :: Test
testFilterAllOut =
  TestCase $ assertEqual "Error on filterS removing all"
                         s0 (filterS (const False) s3)

testLengthEmptySeq :: Test
testLengthEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence length"
                         0 (lengthS s0)

testLengthNonEmptySeq :: Test
testLengthNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence length"
                         2 (lengthS s2)

testMapEmptySeq :: Test
testMapEmptySeq = 
  TestCase $ assertEqual "Error on empty sequence map"
                         s0 (mapS (+1) s0)

testMapNonEmptySeq :: Test
testMapNonEmptySeq = 
  TestCase $ assertEqual "Error on non-empty sequence map"
                         (fromList [7,4,5]) (mapS (+1) s3)

testReduceSumSeq0 :: Test
testReduceSumSeq0 = 
  TestCase $ assertEqual "Error reducing empty sequence"
                         0 (reduceS (+) 0 s0)

testReduceSumSeq3 :: Test
testReduceSumSeq3 = 
  TestCase $ assertEqual "Error reducing sequence of length 3"
                         13 (reduceS (+) 0 s3)

testScanSumSeq0 :: Test
testScanSumSeq0 = 
  TestCase $ assertEqual "Error on empty sequence scan"
                         (emptyS, 0) (scanS (+) 0 s0)

testScanSumSeq3 :: Test
testScanSumSeq3 = 
  TestCase $ assertEqual "Error on scan for sequence of length 3"
                         (fromList[0,6,9], 13) (scanS (+) 0 s3)

testsArray = 
  [
    testMapEmptySeq,
    testMapNonEmptySeq,
    testMapSquareSeq,
    testLengthEmptySeq,
    testLengthNonEmptySeq,
    testNthSeq,
    testReduceSumSeq0,
    testReduceSumSeq1,
    testReduceSumSeq2,
    testReduceSumSeq3,
    testReduceSubSeq,
    testScanSumSeq0,
    testScanSumSeq1,
    testScanSumSeq2,
    testScanSumSeq3,
    testScanReduceConsistency,
    testFilterEmptySeq,
    testFilterNonEmptySeq,
    testFilterAllOut
  ]


main :: IO Counts
main = runTestTT $ TestList testsArray
