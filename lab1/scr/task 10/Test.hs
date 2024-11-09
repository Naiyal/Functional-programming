testSumPrimes :: Test
testSumPrimes = TestList [
    TestCase (assertEqual "sumPrimesTail 10" (sumPrimesTail 10) 17),
    TestCase (assertEqual "sumPrimesRecursion 10" (sumPrimesRecursion 10) 17),
    TestCase (assertEqual "sumPrimesModular 10" (sumPrimesModular 10) 17),
    TestCase (assertEqual "sumPrimesMap 10" (sumPrimesMap 10) 17),
    TestCase (assertEqual "sumPrimesDo 10" (sumPrimesDo 10) 17),
    TestCase (assertEqual "sumPrimesLazy 10" (sumPrimesLazy 10) 17),
    TestCase (assertEqual "sumPrimesTail 2000000" (sumPrimesTail 2000000) 142913828922)
    ]