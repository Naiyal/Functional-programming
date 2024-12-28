testAmicableNumbers :: Test
testAmicableNumbers = TestList [
    TestCase (assertEqual "amicableNumbersTail 10000" (amicableNumbersTail 10000) 31626),
    TestCase (assertEqual "amicableNumbersRecursion 10000" (amicableNumbersRecursion 10000) 31626),
    TestCase (assertEqual "sumAmicableNumbersModular 10000" (sumAmicableNumbersModular 10000) 31626),
    TestCase (assertEqual "sumAmicableNumbersMap 10000" (sumAmicableNumbersMap 10000) 31626),
    TestCase (assertEqual "sumAmicableNumbersDo 10000" (sumAmicableNumbersDo 10000) 31626),
    TestCase (assertEqual "amicableNumbersLazy 10000" (amicableNumbersLazy 10000) 31626)
    ]