# dice-game

The "Land of Lisp" book has a great extended example of building a dice game from scratch.  This project is an attempt to recreate that project in Haskell.

## issues I had and places I found anwsers:
| problem | solution | source |
| ------- | -------- | ------- |
| Adding a test suite | Using hspec |    |
| Getting stack to know about my tests | modifying .cabal file | [this sample project by Taylor Fausak was really helpful](http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/) |
| Getting random numbers to build boards|  | UPenn CIS194 homework was super helpful [Here](http://www.seas.upenn.edu/~cis194/hw/07-monads.pdf) |
| My User prompts aren't showing up when I think they should(stdout buffering)| flushing stdout with hFlush from System.IO| [this SO question](http://stackoverflow.com/questions/13190314/haskell-do-monad-io-happens-out-of-order) |
| fixtures would improve the readability of my tests  | using context and let in tests | http://www.codewars.com/docs/haskell-test-reference  |
| profiling time of execution  | stack has flags for this plus some explicit ghc flags  | the stack stuff is [here](https://github.com/commercialhaskell/stack/blob/master/doc/GUIDE.md#debugging) the compiler flags came from [here](http://stackoverflow.com/a/3276557)  |
| there were unsafe head issues with handling AI players  | i used Safe.headNote to help debug this  |  |
| I fixed the unsafe head issues by using a priority queue |  | i Used PQueue. other priority queue implementations do not allow you to easily discover the priorities |
| The stresstest executable occation takes up a HUGE amount of space.  | the -hy profiling flag can be used to determine what objects are being allocated.  use the hp2pretty tool to turn the output file (stress.hp) into something usable by a human  |  |
|  |  |  |

## other sources
 1. monad reminders, http://seanhess.github.io/2015/08/18/practical-haskell-using-monads.html
 2. randomness help and monadic thinking: https://mail.haskell.org/pipermail/beginners/2010-December/005957.html
 3. test setup: http://stackoverflow.com/questions/20331209/haskell-unit-testing
 4. help getting profiling information: http://stackoverflow.com/questions/32123475/profiling-builds-with-stack
 5.

## the compiler command I used to profile is:
```bash
$ stack install --enable-executable-profiling --enable-library-profiling --ghc-options="-O2 -rtsopts -auto-all" dice-game:stress
$ time ~/.local/bin/stress +RTS -p
 ```
