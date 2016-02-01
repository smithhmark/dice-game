# dice-game

The "Land of Lisp" book has a great extended example of building a dice game from scratch.  This project is an attempt to recreate that project in Haskell.

## Places I found anwsers:
| problem | solution | source |
| ------- | -------- | ------- |
| Adding a test suite | Using hspec |    |
| Getting stack to know about my tests | modifying .cabal file | [this sample project by Taylor Fausak was really helpful](http://taylor.fausak.me/2014/03/04/haskeleton-a-haskell-project-skeleton/) |
| Getting random numbers to build boards|  | UPenn CIS194 homework was super helpful [Here](http://www.seas.upenn.edu/~cis194/hw/07-monads.pdf) |
| My User prompts aren't showing up when I think they should(stdout buffering)| flushing stdout with hFlush from System.IO| [this SO question](http://stackoverflow.com/questions/13190314/haskell-do-monad-io-happens-out-of-order) |
|  |  |  |

## other sources
 1. http://seanhess.github.io/2015/08/18/practical-haskell-using-monads.html
 2. 
