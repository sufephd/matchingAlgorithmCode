# matchingAlgorithmCode
implementation of matching algorithms and related things in Haskell.

There are several editions:

1.daListMonad.hs is the final edition.

2.daStable implements standard DA algorithm and stable test using Data.Set 
and Data.List. The stable test used list comprehension as well as list monad for illustration purpose. 

3.daMonad is designed to use list monad to model free-style DA algorithm. This is the
version given in the paper.

4.daOriginal is the first implementation which uses list instead of set to represent temporily held proposers in the "enrol" field of Acceptor type.

If you still have other questions, you may email to : juyan255@126.com 
