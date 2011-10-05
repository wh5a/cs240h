# Basics

## Tip: variable names

* Inner functions (e.g., `loop`) often have arguments related to
  outer function
    * It is legal to shadow bindings and re-use variable names, but
      the compiler will warn you
    * Typical practice is to add `'` (prime) to the inner-function's
      argument
    * Haskell accepts the `'` character in variables, except as first
      character
* Personally, I find this practice a bit error-prone
    * While learning Haskell, I repeatedly made the error of dropping
      primes, e.g.:

    ~~~ {.haskell}
    factorial n = loop 1 n
        where loop acc n' | n' > 1    = loop (acc * n) (n' - 1) -- bug
                          | otherwise = acc
    ~~~

    * You can avoid the problem by using the longer symbol name for
      the outer function

    ~~~ {.haskell}
    factorial n0 = loop 1 n0
        where loop acc n | n > 1     = loop (acc * n) (n - 1)
                         | otherwise = acc
    ~~~

    * Here accidentally typing "`factorial n0 = loop 1 n`" causes
      compile error

## Running `urldump`

~~~~
$ ghc --make urldump
[1 of 1] Compiling Main             ( urldump.hs, urldump.o )
Linking urldump ...
$ ./urldump http://www.scs.stanford.edu/
<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN"
...
~~~~

* What if you want to run it in GHCI?

    ~~~~
$ ghci ./urldump.hs
Prelude Main>
    ~~~~

    * No `*` before `Main` means no access to internal symbols (because
      compiled)

    ~~~~
Prelude Main> :load *urldump.hs
[1 of 1] Compiling Main             ( urldump.hs, interpreted )
Ok, modules loaded: Main.
*Main> withArgs ["http://cs240h.scs.stanford.edu/"] main
    ~~~~

    * Alternate GHCI shortcut:

    ~~~~
Prelude Main> :main "http://cs240h.scs.stanford.edu/"
    ~~~~

## The [Dreaded][DMRWiki] [Monomorphism Restriction][DMR] (DMR)

* Let's say you want to cache result of super-expensive function

    ~~~~ {.haskell}
    superExpensive val = len $ veryExpensive (val :: Int)
        where len [] = 0
              len (x:xs) = 1 + len xs
    cachedResult = superExpensive 5
    ~~~~

    * `cachedResult` will start as thunk, be executed once, then
      contain value

* Let's think about the types

    ~~~~
    *Main> :t superExpensive
    superExpensive :: Num a => Int -> a
    *Main> :t cachedResult
    cachedResult :: Integer
    ~~~~

    * \+ and 0 are overloaded, so `superExpensive` can return any
      `Num` you want
    * Why don't we have `cachedResult :: (Num a) => a`?
    * Recall context restrictions are like hidden arguments... so
      would make `cachedResult` into a function, undermining our
      caching goal!
    * But how is compiler smart enough to save us here?

## The DMR continued

* Answer: in this case, compiler is not actually that smart
    * Heuristic: If it looks like a function, can infer *ad hoc*
      polymorphic types
    * If it looks like anything else, no *ad hoc* polymorphism unless
      explicitly declared
    * *parametric* polymorphic types can always be inferred (no hidden
      arguments)
* What looks like a function?
    * Has to bind a single symbol (`f`), rather than a pattern (`(x,
      y)`, `(Just x)`)
    * Has to have at least one explicit argument (`f x =` ... ok, `f
      =` ... not)
* How are monomorphic types inferred?
    * If bound symbol used elsewhere in module, infer type from use
    * If still ambiguous and type is of class `Num`, try `Integer`
      then `Double` (this sequence can be changed with a
      [`default` declaration][default])
    * If still ambiguous, compilation fails

## The DMR take-away message

* Think of type restrictions as implicit dictionary arguments
    * Compiler won't saddle non-function with implicit arguments
* This code will compile

    ~~~~ {.haskell}
    -- Compiler infers: show1 :: (Show x) => x -> String
    show1 x = show x
    ~~~~

* But neither of these will:

    ~~~~ {.haskell}
    show2 = show
    show3 = \x -> show x
    ~~~~

    * I'd rather you heard it from me than from GHC...

* Relatively easy to work around DMR
    * Add type signatures to functions--a good idea anyway for
      top-level bindings, and sometimes necessary for `let` bindings

        ~~~~ {.haskell}
        -- No problem, compiler knows you want ad hoc polymorphism
        show2 :: (Show x) => x -> String
        show2 = show
        ~~~~

[DMR]: http://www.haskell.org/onlinereport/haskell2010/haskellch4.html#x10-930004.5.5
[DMRWiki]: http://www.haskell.org/haskellwiki/Monomorphism_restriction


# Memory

## `seq` revisited

* Recall `seq :: a -> b -> b`
    * If `seq a b` is forced, then first `a` is forced, then `b` is
      forced and returned
* Consider the following code:

    ~~~~ {.haskell}
    infiniteLoop = infiniteLoop :: Char   -- loops forever

    seqTest1 = infiniteLoop `seq` "Hello" -- loops forever

    seqTest2 = str `seq` length str       -- returns 6
        where str = infiniteLoop:"Hello"
    ~~~~

    * `seqTest1` hangs forever, while `seqTest2` happily returns 6
* `seq` only forces a `Val`, not the `arg` fields of the `Val`
    * `seqTest2`'s `seq` forces `str`'s constructor `(:)`, but not the
      head or tail
    * This is known as putting `str` in *Weak Head Normal Form* (WHNF)
    * Can't fully evaluate an arbitrary data type (but see
      [Control.DeepSeq](http://hackage.haskell.org/packages/archive/deepseq/latest/doc/html/Control-DeepSeq.html))

# Semantic effects of strictness

* Strictness is primarily used for optimization
    * To avoid building up long chains of thunks
    * To save overhead of checking whether thunk evaluated
* But has semantic effects:  A non-strict `Int` is not just a number
    * Can also throw an exception or loop forever when evaluated
    * Such behavior can be modeled as a special value $\bot$
      ("bottom")
    * So the values of `Int` are $\{0,1\}^{64} \cup \{\bot\}$
    * Types that include value $\bot$ are called *lifted*
* Note 1: an unboxed type is necessarily unlifted
* Note 2: `!Int` not a first-class type, only valid for `data` fields

    ~~~~ {.haskell}
    data SMaybe a = SJust !a | SNothing   -- ok, data field
    strictAdd :: !Int -> !Int -> !Int     -- error
    type StrictMaybeInt = Maybe !Int      -- error
    ~~~~

