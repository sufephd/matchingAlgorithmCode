# matchingAlgorithmCode
implementation of matching algorithms and related things in Haskell.

There are several editions:

1.daFreeSyle.hs is for counting the number of all the free-style DA algorithms in a randomly produced market environment and testing their equivalence in producing SOSM matching outcome. It can only test a magnitude of the market similar to the scale of 5 proposers, 3 acceptors with 2 or 3 quota due to exponential increase of the number of possible free-style DA algorithms.(Even this small magnitude may sometimes lead to millions of free-style DA algorithms in a randomly generated Market environment, thus may take a few minutes to run.)

2.daProperties.hs implements an efficient DA algorithm  using Data.Set and Data.List. Then it uses it to test properties of the matching outcome. It is a program that can be run to test the optimally stable property of SOSM matching outcome. 

3.daSet.hs is an alternative implementation which uses Data.Set operation to implement the test of equivalence instead of list monad. It lacks the ability to count the number of all the free-style DA algorithms, however, it provides an alternative way to show eqivalence of all the free-style DA algorithms. This may be useful to those who want to formally prove our equivalence theorem using computer proof assistants.

For example, if you want to test the daProperties.hs, you need to have a standard haskell environment installation frist, and then download daProperties.hs to a directory. From the commandline or shell, enter the same directory where "daProperties.hs" lies in, input "runhaskell daProperties.hs", then you can answer the questions to see DA properties verified for a randomly generated matching Market with your designated specification.

For all the three files, we use {-# LANGUAGE Strict #-} pragma, because we want eager strict evaluation which is not Haskell default. Please use ghc series of tools for our haskell programs. Version should be above ghc8.


If you still have other questions, you may email to : juyan255@126.com.
