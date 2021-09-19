import Interp

testFactExpected :: [Val]
testFactExpected =
  [ IntVal 0
  , IntVal 1
  , IntVal 2
  , IntVal 6
  , IntVal 24
  , IntVal 120
  , IntVal 720
  , IntVal 5040
  , IntVal 40320
  , IntVal 362880
  , IntVal 3628800
  , IntVal 39916800
  , IntVal 479001600
  , IntVal 6227020800
  , IntVal 87178291200
  , IntVal 1307674368000
  ]

main :: IO ()
main =
  if testFact == testFactExpected then
    putStrLn "Test suite succeeded"
  else
    putStrLn "Test suite failed"
