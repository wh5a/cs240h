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

## Semantic effects of strictness

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

## `newtype` semantics

* What's the semantic difference between these two declarations?

    ~~~~ {.haskell}
    newtype NTInt = NTInt Int deriving (Show)
    ~~~~

    ~~~~ {.haskell}
    data SInt = SInt !Int deriving (Show)
    ~~~~

* The `NTInt` constructor is a "fake" compile-time-only construct
    * A case statement deconstructing a `newtype` compiles to nothing

    ~~~~ {.haskell}
    newtype NTInt = NTInt Int deriving (Show)
    uNTInt = NTInt undefined
    testNT = case uNTInt of NTInt _ -> True   -- returns True

    data SInt = SInt !Int deriving (Show)
    uSInt = SInt undefined
    testS = case uSInt of SInt _ -> True      -- undefined
    ~~~~

## The [`UNPACK`][UNPACK] pragma

* `newtype` almost always better than `data` when it applies
* What about a multi-field data type?

    ~~~~ {.haskell}
    data TwoInts = TwoInts !Int !Int
    ~~~~

    * Fields are strict, we know they'll have `CONSTRNO` `ValInfo`
    * Why not stick the `Int#`s directly into the `args` of a
      `TwoInts` `Val`?
    * GHC provides an `UNPACK` pragma to do just this

        ~~~~ {.haskell}
        data TwoInts = TwoInts {-# UNPACK #-} !Int {-# UNPACK #-} !Int
        ~~~~

    * Works for any strict field with a single-constructor datatype
* Unlike `newtype`, `UNPACK` is not always a win
    * If you pass field as argument, will need to re-box it
* `-funbox-strict-fields` flag unpacks *all* strict fields

## User-managed memory

* Opaque type [`Ptr a`][Ptr] represents pointers to type `a`
    * Pointers are not typesafe--allow pointer arithmetic and casting

        ~~~~ {.haskell}
        nullPtr :: Ptr a
        plusPtr :: Ptr a -> Int -> Ptr b
        minusPtr :: Ptr a -> Ptr b -> Int
        castPtr :: Ptr a -> Ptr b
        ~~~~

    * Pointer arithmetic is always in units of bytes (unlike in C,
      where unit is size of the pointed-to object)
* Class [`Storable`][Storable] provides raw access to memory using
  `Ptr`s

    ~~~~ {.haskell}
    class Storable a where
        sizeOf :: a -> Int
        alignment :: a -> Int
        peek :: Ptr a -> IO a
        poke :: Ptr a -> a -> IO ()
        ...
    ~~~~

    * Most basic types (`Bool`, `Int`, `Char`, `Ptr a`, etc.) are `Storable`

## `alloca`

* Easiest way to get a valid `Ptr` is `alloca`:

    ~~~~ {.haskell}
    alloca :: Storable a => (Ptr a -> IO b) -> IO b
    ~~~~

    * Allocates enough space for an object of type `a`
    * Calls function with a `Ptr` to the space
    * Reclaims the memory when the function returns (much like C
      `alloca`)
    * Can also ask for a specific number of bytes:

    ~~~~ {.haskell}
    allocaBytes :: Int -> (Ptr a -> IO b) -> IO b
    ~~~~

* `Foreign` module provides handy [`with`][with] utility

    ~~~~ {.haskell}
    with :: Storable a => a -> (Ptr a -> IO b) -> IO b
    with val f  =
      alloca $ \ptr -> do
        poke ptr val
        res <- f ptr
        return res
    ~~~~


## More `Storable` types

* `Foreign.C` contains wrappers for C types
    * `CInt`, `CUInt`, `CChar`, `CDouble`, `CIntPtr` etc.
* `Data.Int` and `Data.Word` have all sizes of machine integer
    * `Int8`, `Int16`, `Int32`, `Int64` -- signed integers
    * `Word8`, `Word16`, `Word32`, `Word64` -- unsigned integers

* Example: extract all the bytes from a `Storable` object

    ~~~~ {.haskell}
    toBytes :: (Storable a) => a -> [Word8]
    toBytes a = unsafePerformIO $
        with a $ \pa -> go (castPtr pa) (pa `plusPtr` sizeOf a)
        where go p e | p < e = do b <- peek p
                                  bs <- go (p `plusPtr` 1) e
                                  return (b:bs)
                     | otherwise = return []
    ~~~~

    * `unsafePerformIO` might be okay here since `toBytes` pure
    * Notice how `plusPtr` lets us change from `Ptr a` to `Ptr Word8`

## `malloc` and `mallocForeignPtr`

* Can also allocate longer-lived memory with `malloc`

    ~~~~ {.haskell}
    malloc :: Storable a => IO (Ptr a)
    mallocBytes :: Int -> IO (Ptr a)
    free :: Ptr a -> IO ()
    realloc :: Storable b => Ptr a -> IO (Ptr b)
    reallocBytes :: Ptr a -> Int -> IO (Ptr a)
    ~~~~

    * Disadvantage:  bad programming can lead to memory
      leaks/corruption

* `ForeignPtr` lets you delegate deallocation to garbage collector

    ~~~~ {.haskell}
    mallocForeignPtr :: Storable a => IO (ForeignPtr a)
    mallocForeignPtrBytes :: Int -> IO (ForeignPtr a)
    ~~~~

## Working with `ForeignPtr`s

* To use `ForeignPtr`, must convert it to `Ptr`
    * Problem: How does GC know `ForeignPtr` in scope when using
      `Ptr`?
    * Solution: use `Ptr` within function that keeps reference to
      `ForeignPtr`

    ~~~~ {.haskell}
    withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
    ~~~~

* Can also convert `Ptr`s to `ForeignPtr`s

    ~~~~ {.haskell}
    type FinalizerPtr a = FunPtr (Ptr a -> IO ())
    newForeignPtr :: FinalizerPtr a -> Ptr a
                  -> IO (ForeignPtr a)
    newForeignPtr_ :: Ptr a -> IO (ForeignPtr a)
    addForeignPtrFinalizer :: FinalizerPtr a -> ForeignPtr a
                           -> IO ()
    ~~~~

    * Can add multiple finalizers, will run in reverse order
* Note use of `FunPtr` -- this is type wrapper for C function pointer
    * Need foreign function interface to create these
    * [`finalizerFree`][finalizerFree] symbol conveniently provides
      function pointer for `free`


## [Foreign function interface][FFI] (FFI)

* Can import foreign functions like this:

    ~~~~ {.haskell}
    foreign import ccall unsafe "stdlib.h malloc"
        c_malloc :: CSize -> IO (Ptr a)
    foreign import ccall unsafe "stdlib.h free"
        c_free :: Ptr a -> IO ()
    ~~~~

    * `ccall` says use C calling convention (also `cplusplus` and few
      others)
    * `unsafe` promises the C function will not call back into
      Haskell
    * `unafe` faster than `safe`, but gives undefined results if call
      triggers GC
* Spec for import string: `"`[`static`] [*c-header*] [`&`][*c-name*]`"`
    * `static` required only if *c-name* is `dynamic` or `wrapper`
    * *c-header* is a single `.h` file with the declaration
       (ignored by GHC)
    * '&' imports pointer rather than function (required for `FunPtr`s)


## FFI types

* FFI function arguments must be *basic foreign types*
    * `Char`, `Int`, `Double`, `Float`, `Bool`, `Int8`, `Int16`,
      `Int32`, `Int64`, `Word8`, `Word16`, `Word32`, `Word64`, `Ptr`
      `a`, `FunPtr a`, and `StablePtr a`
    * Also accepts any `type` or `newtype` wrappers for basic types
      (`CInt`, `CChar`, etc.)<br/>
      [Documentation incorrectly says `data CInt`, but `:i` in GHCI
      reveals truth.]
* FFI function results can be
    * Any valid argument type
    * `()` (for functions returning `void`)
    * `IO a` where `a` is either of the above two
* Place result `IO` if function has side effects or non-determinism
    * Okay to omit if it is a pure C function:

        ~~~~ {.haskell}
        foreign import ccall unsafe "arpa/inet.h ntohl"
            ntohl :: Word32 -> Word32
        ~~~~

    * Haskell can't check C purity, so omitting `IO` can cause
      problems

## [`hsc2hs`][hsc2hs]

* How to access C data structures?

    ~~~~ {.c}
    struct mystruct {
      char *name;
      int value;
    };
    ~~~~

    * Might model with opaque placeholder type

    ~~~~ {.haskell}
    data MyStruct        -- no constructors, just a placeholder
    getValue :: Ptr MyStruct -> IO CInt
    getValue ptr = peek $ ptr `plusPtr` 8  -- assumes char * 8 bytes
    ~~~~

* [`hsc2hs`][hsc2hs] is pre-processor that lets you compute C values

    ~~~~ {.haskell}
    #include "myheader.h"
    getValue ptr = peek $ ptr `plusPtr`
                   #{offset struct mystruct, value}
    ~~~~

    * Super-simple implementation just uses C macros & `printf`
    * Find the file [`template-hsc.h`][template-hsc.h] on your system
      to see defs of `#` commands
    * Can also define your own macros with `#let` (like `#define` w/o
      parens)

## Lazy `ByteString` implementation

* Lazy `ByteString`s are implemented in terms of strict ones

    ~~~~ {.haskell}
    data ByteString = Empty
                    | Chunk {-# UNPACK #-} !S.ByteString ByteString
    ~~~~

    * Invariant: `Chunk`'s first argument (`S.ByteString`) never `null`
    * Basically a linked list of strict `ByteString`s
    * Head is strict, tail is not, allowing lazy computation or I/O

* When to use strict/lazy `ByteString`s?
    * Obviously use lazy when you need laziness (e.g., lazy I/O,
      infinite or cyclical strings, etc.)
    * Lazy also much faster at concatenation (need to build a new list
      of `S.ByteString`s, but not copy the data they contain)
    * Strict makes it much easier to implement things like string
      search
    * Converting strict to lazy `ByteString`s is cheap, reverse is not
      (so if a library can work efficiently on lazy `ByteString`s,
      good to expose that functionality)

## Exceptions in pure code

* Can `throw` exceptions in pure code, yet `catch` them only in `IO`
    * This is because evaluation order depends on implementation
    * Which error is thrown by `(error "one") + (error "two")`?<br/>
      Can be non-deterministic, which is [okay][imprecise exceptions]
      if `catch` is restricted to the `IO` Monad
* In `IO`, use `throwIO` (not `throw`) to make exception sequencing
  precise

    ~~~~ {.haskell}
        do x <- throwIO (MyError "one")  -- this exception thrown
           y <- throwIO (MyError "two")  -- this code not reached
    ~~~~

* Beware `catch` only catches exceptions if code actually evaluated


    ~~~~ {.haskell}
    pureCatcher :: a -> IO (Maybe a)
    pureCatcher a = (a `seq` return (Just a))
                    `catch` \(SomeException _) -> return Nothing
    ~~~~

    ~~~~
    *Main> pureCatcher (undefined :: String)
    Nothing
    *Main> pureCatcher (undefined:undefined :: String)
    Just "*** Exception: Prelude.undefined
    ~~~~

[Ptr]: http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/Foreign-Ptr.html#t:Ptr
[Storable]: http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/Foreign-Storable.html#t:Storable
[GHC.Prim]: http://www.haskell.org/ghc/docs/latest/html/libraries/ghc-prim-0.2.0.0/GHC-Prim.html
[MagicHash]: http://www.haskell.org/ghc/docs/latest/html/users_guide/syntax-extns.html#magic-hash
[UNPACK]: http://www.haskell.org/ghc/docs/latest/html/users_guide/pragmas.html#unpack-pragma
[with]: http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/Foreign-Marshal-Utils.html#v:with
[FFI]: http://www.haskell.org/onlinereport/haskell2010/haskellch8.html
[hsc2hs]: http://www.haskell.org/ghc/docs/latest/html/users_guide/hsc2hs.html
[template-hsc.h]: http://darcs.haskell.org/cgi-bin/gitweb.cgi?p=hsc2hs.git;a=blob;f=template-hsc.h;hb=HEAD
[bytestring]: http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring-0.9.2.0/index.html
[ByteString.Lazy]: http://www.haskell.org/ghc/docs/latest/html/libraries/bytestring-0.9.2.0/Data-ByteString-Lazy.html
[finalizerFree]: http://www.haskell.org/ghc/docs/latest/html/libraries/base-4.4.0.0/Foreign-Marshal-Alloc.html#v:finalizerFree
[imprecise exceptions]: http://research.microsoft.com/en-us/um/people/simonpj/papers/imprecise-exn.htm


# Testing

## Type defaulting - a recap

Haskell's usual defaulting rules take each group of constraints `(C1 a, C2
a, ..., Cn a)` for each type variable `a`, and defaults the type
variable if all of the following conditions hold:

* The type variable `a` appears in no other constraints.

* All the classes `Ci` are standard.

* At least one of the classes `Ci` is numeric.

## That's not enough for us lazy programmers!

To reduce the number of types we're forced to specify by hand, `ghci`
relaxes the standard rules (changes in italics):

* The type variable `a` appears in no other constraints. *Unchanged*.

* All the classes `Ci` are standard, *and all are single-parameter
  type classes*.

* At least one of the classes `Ci` is numeric, *or is `Show`, `Eq`, or
  `Ord`*.

It also adds another critical step when defaulting:

* The type `()` becomes the first of the the standard list of types
  tried when doing type defaulting.

## Peek inside

We can use the `verboseCheck` function to see all the test data that
QuickCheck is generating for us.

~~~~ {.haskell}
t_idempotent xs = 
    sort (sort xs) == sort xs
~~~~

~~~~
>> import Test.QuickCheck
>> quickCheck t_idempotent
>> verboseCheck t_idempotent
~~~~

Notice that we have endless lists of `()`?

Our supposedly reassuring test isn't very useful!

~~~~
>> import Data.Word (Word8)
>> verboseCheck (t_idempotent :: [Word8] -> Bool)
~~~~

## Witness the fitness

Here's an alternative approach:

~~~~
t_witnessed p a xs = sortBy p (sortBy p xs) == sortBy p xs
  where _witness = a < head xs
~~~~

What's that `_witness` variable for?

* It's a *type witness*, a value that exists to express a constraint
  between several types (it "witnesses" the constraint).
  
* Thanks to the use of `<`, this witness forces the type of `a` and
  the type of the elements of `xs` to be the same.

(We prefix the name with an underscore to tell GHC that it's an unused
wild card.)

## Instantiating our new polymorphic test

We can supply a value for `a` of the appropriate type to test over:

~~~~
>> verboseCheck (t_witnessed compare 'a')
~~~~

Of course, the value of `a` is never used.

As a result, we don't even need to supply a working value, provided the
type of what we supply is correct:

~~~~
>> verboseCheck (t_witnessed compare (undefined::Double))
~~~~

## Where do random values come from?

To generate random values of some type, we must write an `Arbitrary`
instance for it.

~~~~ {.haskell}
class Arbitrary a where
  arbitrary :: Gen a
~~~~

Here's an example, making use of the fact that this unknown type `Gen`
is an instance of `Monad`:

~~~~ {.haskell}
import Control.Monad (liftM2)

data Point = Point Int Int

instance Arbitrary Point where
    arbitrary = liftM2 Point arbitrary arbitrary
~~~~

## Conditional properties

Suppose we want to verify that the sum of two odd integers is always
even.

It would be nice if we could express the idea "check this property
only if the inputs satisfy these constraints".

In fact, there's a combinator for that: `==>`

~~~~ {.haskell}
p_sum_odd1 a b =
    odd a && odd b ==> 
    even (a+b)
~~~~

This specifies that the property on the right should hold whenever the
`Bool`-valued test on the left succeeds.

QuickCheck will discard inputs for which the test fails.

## Correctness by construction

Instead of filtering out data that isn't right for us, it's better to
generate *only* data that is right.

~~~~
newtype Odd a = Odd a
    deriving (Show)

instance (Integral a, Arbitrary a) => Arbitrary (Odd a) where
    arbitrary = do
      a <- arbitrary
      return $! Odd (if even a then a + 1 else a)
~~~~

It's clear from inspection that the `Arbitrary` instance for `Odd a`
will only generate odd-valued integers.

# Sizing a test

Test data generators have an implicit size parameter, hidden inside
the `Gen` type.

QuickCheck starts by generating small test cases; it increases the
size as testing progresses.

The meaning of "size" is specific to the needs of an `Arbitrary`
instance.

* The `Arbitrary` instance for lists interprets it as "the maximum
  length of a list of arbitrary values".

We can find the current size using the `sized` function, and modify it
locally using `resize`:

~~~~ {.haskell}
sized  :: (Int -> Gen a) -> Gen a
resize ::  Int -> Gen a  -> Gen a
~~~~

## Testing a recursive data type

Suppose we have a tree type:

~~~~ {.haskell}
data Tree a = Node (Tree a) (Tree a)
            | Leaf a
              deriving (Show)
~~~~

Here's an obvious `Arbitrary` instance:

~~~~ {.haskell}
instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = oneof [
                  liftM Leaf arbitrary
                , liftM2 Node arbitrary arbitrary
                ]
~~~~

The `oneof` combinator chooses a generator at random.

~~~~ {.haskell}
oneof :: [Gen a] -> Gen a
~~~~

## What's up, Doc?

Potential trouble:

* This generator may not terminate at all!

* It's likely to produce *huge* trees.

We can use the `sample` function to generate and print some arbitrary
data.

~~~~ {.haskell}
sample :: (Show a) => Gen a -> IO ()
~~~~

This helps us to explore what's going on.

## A safer instance

Here's where the sizing mechanism comes to the rescue.

~~~~ {.haskell}
instance (Arbitrary a) => Arbitrary (Tree a) where
    arbitrary = sized tree

tree :: (Arbitrary a) => Int -> Gen (Tree a)
tree 0 = liftM Leaf arbitrary
tree n = oneof [
           liftM  Leaf arbitrary
         , liftM2 Node subtree subtree
         ]
  where subtree = tree (n `div` 2)
~~~~


# Concurrency

## Bound vs. unbound threads

* Without `-threaded`, all Haskell threads run in one OS thread
    * Thread switch is basically just a procedure call, i.e. super-fast

* `-threaded` introduces multiple OS-level threads
    * Some Haskell threads are *bound* to a particular OS thread
    * *Unbound* Haskell threads share (and migrate between) OS threads
    * `unbound` haskell threads have same performance as w/o
      `-threaded`

## What good are OS threads?

* If an unbound thread blocks, can block whole program
    * Unix runtime tries to avoid blocking syscalls, but can't avoid
      blocking for things like file system IO and paging
    * With `-threaded`, GHC ensures `safe` FFI calls run in separate
      OS thread
    * `unsafe` FFI calls from unbound threads can block other threads
* FFI functions may expect to be called from same thread
    * E.g., foreign code using `pthread_getspecific` can get confused
      if called from a migrated unbound thread
* May want to override scheduler and run on particular CPU
    * E.g., see
      [`forkOn`](http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Concurrent.html#v:forkOn)

## Channels

* [`Control.Concurrent.Chan`] provides unbounded *channels*
    * Implemented as two `MVar`s -- for read and and write end of `Stream`

    ~~~~ {.haskell}
    data Item a = Item a (Stream a)
    type Stream a = MVar (Item a)
    data Chan a = Chan (MVar (Stream a)) (MVar (Stream a))
    ~~~~

![](chan.svg)


# Lenses

## The frob merchant's web store

Suppose we're building a web app, where we want to send frobs to
customers of our web site.

~~~~ {.haskell}
data Customer = Customer {
      custID :: Int
    , custName :: String
    , custAddress :: Address
    }

newtype Zip = Zip Int

data Address = Address {
      addrStreet :: String
    , addrCity :: String
    , addrState :: String
    , addrZip :: Zip
    }
~~~~

## Oh noes!

A customer has made a mistake in entering their shipping zip code.
They've called us up, irate that we've been unable to fulfil their
urgent frob order.

So. We need to change their zip code.

Here are our desiderata:

1. We want to be able to access fields within records.

1. We want to be able to *compose* accesses, so that we can inspect
   fields within records that are themselves fields of records.

1. We want to be able to update fields within records.

1. We want to be able to *compose* updates, so that we can modify
   fields within records that are themselves fields of records.

With Haskell's record syntax, we get #1 and #2, sort of #3 (if we
squint), and definitely not #4.

## Lenses

What we want is a type that behaves something like this:

~~~~ {.haskell}
data Lens rec fld = Lens {
      get :: rec -> fld
    , set :: fld -> rec -> rec
    }
~~~~

This "bundles together" a record type `rec` with a field type `fld`,
so that we know:

* how to get a field out of a record, and

* how to update a field within a record.

(Why the name "lens"? Because it lets us *focus* on a field within a
record.)

## What does a real lens look like?

The following definitions correspond to those in the
[data-lens](http://hackage.haskell.org/package/data-lens) package.

NOTE: There are other alternatives, such as
[data-accessor](http://blog.ezyang.com/2010/04/inessential-guide-to-data-accessor/)
and
[fclabels](http://blog.ezyang.com/2010/04/inessential-guide-to-fclabels/). [Some](http://stackoverflow.com/questions/5767129/lenses-fclabels-data-accessor-which-library-for-structure-access-and-mutatio)
[comparisons](http://www.reddit.com/r/haskell/comments/gw7hz/lenses_fclabels_dataaccessor_which_library_for/)
are
available. [Semantic editor combinators](http://conal.net/blog/posts/semantic-editor-combinators)
is another approach.

~~~~ {.haskell}
newtype Lens rec fld = Lens (rec -> Store fld rec)
~~~~

where

~~~~ {.haskell}
data Store fld rec = Store (fld -> rec) fld
~~~~

That's hard to follow, so let's dig in and try to understand.  First,
we'll get rid of the name `Store`, to give the tuple:

~~~~ {.haskell}
(fld -> rec, fld)
~~~~

Then we'll substitute this into the definition of `Lens`:

~~~~ {.haskell}
newtype Lens rec fld = Lens (rec -> (fld -> rec, fld))
~~~~

That is, a `Lens` is:

* A function that accepts a record type `rec` as its argument

* It returns a pair

* The first element is a setter: give it a field value of type `fld`,
  and it will return a new record

* The second element is the current value of the field

## Why the coupling?

Why does a lens give us both the value of a field and a function for
setting a new value of that field?

* Suppose that computing the path to the right place in the record for
  the getter is expensive.
  
* This representation allows the setter to reuse that computation.

## The get operator

Here is our getter:

~~~~ {.haskell}
(^.) :: rec -> Lens rec fld -> fld
a ^. (Lens f) = pos (f a)
infixr 9 ^.

-- internal
pos :: Store fld rec -> fld
pos (Store _ s) = s
~~~~

## The set operator

And here is our setter:

~~~~ {.haskell}
(^=) :: Lens rec fld -> fld -> rec -> rec
(Lens f) ^= b = peek b . f
infixr 4 ^=

-- internal
peek :: fld -> Store fld rec -> rec
peek s (Store g _) = g s
~~~~

## Constructing a lens

Given a getter and a setter, we can build a lens:

~~~~ {.haskell}
lens :: (rec -> fld) -> (fld -> rec -> rec) -> Lens rec fld
lens get set = Lens $ \a -> Store (\b -> set b a) (get a)
~~~~

Alternatively, we can construct a lens from an *isomorphism* between
record and field types:

~~~~ {.haskell}
iso :: (rec -> fld) -> (fld -> rec) -> Lens rec fld
iso f g = Lens (Store g . f)
~~~~

## A lens for points

Consider our venerable `Point` type:

~~~~ {.haskell}
data Point = Point {
      ptX :: Int
    , ptY :: Int
    } deriving (Show)
~~~~

We need to define two lenses for this type, one to focus on the `x`
coordinate, and another for `y`:

~~~~ {.haskell}
x, y :: Lens Point Int
x = lens ptX (\x pt -> pt {ptX = x})
y = lens ptY (\y pt -> pt {ptY = y})
~~~~

## Using our lens on points

The getter:

~~~~ {.haskell}
>> let pt = Point 1 1
>> pt ^. x
1
~~~~

The setter:

~~~~ {.haskell}
>> (x ^= 2) pt
Point {ptX = 2, ptY = 1}
~~~~

## Function composition: not gnar enough

By now, we are familiar with (and love) function composition:

~~~~ {.haskell}
(.) :: (b -> c) -> (a -> b) -> (a -> c)
~~~~

However, we can make composition more abstract:

~~~~ {.haskell}
import Prelude hiding (id, (.))

class Category cat where
  id :: cat a a
  (.) :: cat b c -> cat a b -> cat a c
~~~~

Now we can recast function composition as just an instance of this
more general `Category` class:

~~~~ {.haskell}
instance Category (->) where
    id a = a
    f . g = \x -> f (g x)
~~~~

## Category? Composition? Abstraction? Huh?

We care about the `Category` class because it turns out we can compose
lenses!

~~~~ {.haskell}
import Control.Category

instance Category Lens where
    id = Lens (Store id)

    Lens f . Lens g = Lens $ \a -> case g a of
      Store wba b -> case f b of
	Store wcb c -> Store (wba . wcb) c
~~~~

How do we do this in practice?

Just as we compose two functions to get another function, when we
compose two lenses, we get another lens.

## Composition of lenses

~~~~ {.haskell}
data Line = Line {
      lnBeg :: Point
    , lnEnd :: Point
    } deriving (Show)

beg, end :: Lens Line Point
beg = lens lnBeg (\b l -> l {lnBeg = b})
end = lens lnEnd (\e l -> l {lnEnd = e})
~~~~

Access a nested field:

~~~~ {.haskell}
>> let l = Line (Point 1 2) (Point 3 4)
>> l ^. (x . beg)
~~~~

Modify a nested field:

~~~~ {.haskell}
>> ((y . end) ^= 7) l
Line {lnBeg = Point {ptX = 1, ptY = 2},
      lnEnd = Point {ptX = 3, ptY = 7}}
~~~~

## A map as a lens

Lenses are not restricted to use solely with algebraic data types.

They're just as applicable to container types, for instance:

~~~~ {.haskell}
import qualified Data.Map as Map
import Data.Map (Map)

mapLens :: (Ord k) => k -> Lens (Map k v) (Maybe v)
mapLens k = Lens $ \m ->
            let set Nothing  = Map.delete k m
                set (Just v) = Map.insert k v m
                get          = Map.lookup k m
            in Store set get
~~~~


# Phantom types

## Managing mutation

Application writers are often faced with a question like this:

* I have a big app, and parts of it need their behaviour tweaked by an
  administrator at runtime.
  
There are of course many ways to address this sort of problem.

Let's consider one where we use a reference to a piece of config data.

Any piece of code that's executing in the `IO` monad, if it knows the
name of the config reference, can get the current config:

~~~~ {.haskell}
curCfg <- readIORef cfgRef
~~~~

The trouble is, ill-behaved code could clearly also *modify* the
current configuration, and leave us with a debugging nightmare.

## Phantom types to the rescue!

Let's create a new type of mutable reference.

We use a phantom type `t` to statically track whether a piece of code
is allowed to modify the reference or not.

~~~~ {.haskell}
import Data.IORef

newtype Ref t a = Ref (IORef a)
~~~~

Remember, our use of `newtype` here means that the `Ref` type only
exists at compile time: it imposes *no* runtime cost.

Since we are using a phantom type, we don't even need values of our
access control types:

~~~~ {.haskell}
data ReadOnly
data ReadWrite
~~~~

We're already in a good spot!  Not only are we creating
compiler-enforced access control, but it will have *zero* runtime
cost.

## Creating a mutable reference

To create a new reference, we just have to ensure that it has the
right type.

~~~~ {.haskell}
newRef :: a -> IO (Ref ReadWrite a)
newRef a = Ref `fmap` newIORef a
~~~~

## Reading and writing a mutable reference

Since we want to be able to read both read-only and read-write
references, we don't need to mention the access mode when writing a
type signature for `readRef`.

~~~~ {.haskell}
readRef :: Ref t a -> IO a
readRef (Ref ref) = readIORef ref
~~~~

Of course, code can only write to a reference if the compiler can
statically prove (via the type system) that it has write access.

~~~~ {.haskell}
writeRef :: Ref ReadWrite a -> a -> IO ()
writeRef (Ref ref) v = writeIORef ref v
~~~~

## Converting a reference to read-only

This function allows us to convert any kind of reference into a
read-only reference:

~~~~ {.haskell}
readOnly :: Ref t a -> Ref ReadOnly a
readOnly (Ref ref) = Ref ref
~~~~

In order to prevent clients from promoting a reference from read-only
to read-write, we do *not* provide a function that goes in the
opposite direction.

We also use the familiar technique of constructor hiding at the top of
our source file:

~~~~ {.haskell}
module Ref
    (
      Ref, -- export type ctor, but not value ctor
      newRef, readOnly,
      readRef, writeRef
    ) where
~~~~

## Databases

Love 'em or hate 'em, everybody has to deal with databases.

Here are some typical functions that a low-level database library will
provide, for clients that have to modify data concurrently:

~~~~ {.haskell}
begin    :: Connection -> IO Transaction
commit   :: Transaction -> IO ()
rollback :: Transaction -> IO ()
~~~~

We can create a new transaction with `begin`, finish an existing
one with `commit`, or cancel one with `rollback`.

Typically, once a transaction has been committed or rolled back,
accessing it afterwards will result in an exception.

## Shaky foundations build a shaky house

Clearly, these constructs make it easy to inadvertantly write bad
code.

~~~~ {.haskell}
oops conn = do
  txn <- begin conn
  throwIO (AssertionFailed "forgot to roll back!")
  -- also forgot to commit!
~~~~

We can avoid `rollback` and `commit` forgetfulness with a suitable
combinator:

~~~~ {.haskell}
withTxn :: Connection -> IO a -> IO a
withTxn conn act = do
  txn <- begin conn
  r <- act `onException` rollback txn
  commit txn
  return r
~~~~

All right!  The code running in `act` never sees a `Transaction`
value, so it can't leak a committed or rolled back transaction.

## But still...

We're not out of the woods yet!

High-performance web apps typically use a dynamically managed pool of
database connections.

~~~~ {.haskell}
getConn :: Pool -> IO Connection
returnConn :: Pool -> Connection -> IO ()
~~~~ {.haskell}

It's a major bug if a database connection is not returned to the pool
at the end of a handler.

So we write a combinator to handle this for us:

~~~~ {.haskell}
withConn :: Pool -> (Connection -> IO a) -> IO a
withConn pool act =
  bracket (getConn pool) (returnConn pool) act
~~~~

Nice and elegant. But correct? Read on!

## Connections vs transactions

In a typical database API, once we enter a transaction, we don't need
to refer to the handle we got until we either commit or roll back the
transaction.

So it was fine for us to write a transaction wrapper like this:

~~~~ {.haskell}
withTxn :: Connection -> IO a -> IO a
~~~~

On other other hand, if we're talking to a database, we definitely
need a connection handle.

~~~~ {.haskell}
query :: Connection -> String -> IO [String]
~~~~

So we have to pass that handle into our combinator:

~~~~ {.haskell}
withConn :: Pool -> (Connection -> IO a) -> IO a
~~~~

Unfortunately, since `withConn` gives us a connection handle, we can defeat the
intention of the combinator (sometimes accidentally).

What is the type of this function?

~~~~ {.haskell}
evil pool = withConn pool return
~~~~

## Phantom types! They'll save us again!

Here, we are using the `newtype` keyword to associate a phantom type
with the `IO` monad.

~~~~ {.haskell}
newtype DB c a = DB {
      fromDB :: IO a
    }
~~~~

We're going to run some code in the `IO` monad, and pass around a
little extra bit of type information at compile time.

Let's create a phantom-typed wrapper for our earlier `Connection`
type:

~~~~ {.haskell}
newtype SafeConn c = Safe Connection
~~~~

Where are these phantom types taking us?

## Safe querying

The easiest place to start to understand with a little use of our new
code, in the form of a function we'll export to clients.

This is just a wrapper around the `query` function we saw earlier,
making sure that our `newtype` machinery is in the right places to
keep the type checker happy.

~~~~ {.haskell}
safeQuery :: SafeConn c -> String -> DB c [String]
safeQuery (Safe conn) str = DB (query conn str)
~~~~

Notice that our phantom type `c` is mentioned in both our uses of
`SafeConn c` and `DB c`: we're treating it as a token that we have to
pass around.

Our library will *not* be exporting the value constructors for
`SafeConn` or `DB` to clients.  Once again, this `newtype` machinery
is internal to us!

## Giving a client a connection from a pool

Here, we'll use our earlier exception-safe `withConn` combinator.
Recall its type:

~~~~ {.haskell}
withConn :: Pool -> (Connection -> IO a) -> IO a
~~~~

To make it useful in our new setting, we have to wrap the
`Connection`, and unwrap the `DB c` that is our `act` to get an action
in the `IO` monad.

~~~~ {.haskell}
withSafeConn pool act =
  withConn pool $ \conn ->
    fromDB (act (Safe conn))
~~~~

It's not at all obvious what this is doing for us until we see the
type of `withSafeConn`.

## Scariness

Here's a burly type for you:

~~~~ {.haskell}
{-# LANGUAGE Rank2Types #-}

withConnection :: Pool
               -> (forall c. SafeConn c -> DB c a) 
               -> IO a
~~~~

We've introduced a universal quantifier (that `forall`) into our type
signature.  And we've added a `LANGUAGE` pragma!  Whoa!

Relax!  Let's not worry about those details just yet.  What does our
signature seem to want to tell us?

* We accept a `Pool`.

* And an "I have a connection, so I can talk to the database now"
  action that accepts a `SafeConn c`, returning a value `a` in the
  world of `DB c`.

Not so scary after all, except for the detail we're ignoring.

## Universal quantification to the rescue!

Let's start with the obviously bothersome part of the type signature.

~~~~ {.haskell}
(forall c. SafeConn c -> DB c a)
~~~~

This is the same universal quantification we've seen before, meaning:

* Our "I can haz connection" action must work *over all types* `c`.

* The *scope* of `c` extends only to the rightmost parenthesis here.

Putting it back into context:

~~~~ {.haskell}
withConnection :: Pool
               -> (forall c. SafeConn c -> DB c a) 
               -> IO a
~~~~

The type variable `a` is mentioned in a place where `c` is *not* in
scope, so although `a` is also universally quantified, it *cannot be
related* to `c`.

## Wait, wait. What, exactly, got rescued?

~~~~ {.haskell}
withConnection :: Pool
               -> (forall c. SafeConn c -> DB c a) 
               -> IO a
~~~~

Because `SafeConn c` shares the same phantom type as `DB c`, and the
quantified `c` type cannot escape to the outer `IO`, there is no way
for a `SafeConn c` *value* to escape, either!

In other words, we have ensured that a user of `withConnection` cannot
either accidentally allow or force a connection to escape from the
place where we've deemed them legal to use.


# Random numbers

## Purely functional random numbers

Haskell supplies a `random` package that we can use in a purely
functional setting.

~~~~ {.haskell}
class Random a where
    random :: RandomGen g => g -> (a, g)

class RandomGen g where
    next   :: g -> (Int, g)
    split  :: g -> (g, g)
~~~~

## RandomGen

The `RandomGen` class is a building block: it specifies an interface
for a generator that can generate uniformly distributed pseudo-random
`Int`s.

There is one default instance of this class:

~~~~ {.haskell}
data StdGen {- opaque -}

instance RandomGen StdGen
~~~~

## Random

The `Random` class specifies how to generate a pseudo-random value of
some type, given the random numbers generated by a `Gen` instance.

Quite a few common types have `Random` instances.

* For `Int`, the instance will generate any representable value.

* For `Double`, the instance will generate a value in the range
  $[0,1]$.
  
## Generators are pure

Since we want to use a PRNG in pure code, we obviously can't modify
the state of a PRNG when we generate a new value.

This is why `next` and `random` return a *new* state for the PRNG
every time we generate a new pseudo-random value.

## Throwing darts at the board

Here's how we can generate a guess at $x^2 + y^2$:

~~~~ {.haskell}
guess :: (RandomGen g) => (Double,g) -> (Double,g)
guess (_,g) = (z, g'')
    where z        = x^2 + y^2
          (x, g')  = random g
          (y, g'') = random g'
~~~~

Note that we have to hand back the *final* state of the PRNG along
with our result! 

If we handed back `g` or `g'` instead, our numbers would either be all
identical or disastrously correlated (every `x` would just be a repeat
of the previous `y`).

## Global state

We can use the `getStdGen` function to get a handy global PRNG state:

~~~~ {.haskell}
getStdGen :: IO StdGen
~~~~

This does *not* modify the state, though. If we use `getStdGen` twice
in succession, we'll get the same result each time.

To be safe, we should update the global PRNG state with the final PRNG
state returned by our pure code:

~~~~ {.haskell}
setStdGen :: StdGen -> IO ()
~~~~

# Ugh - let's split!

Calling `getStdGen` and `setStdGen` from `ghci` is a pain, so let's
write a combinator to help us.

Remember that `split` method from earlier?

~~~~ {.haskell}
class RandomGen g where
    split  :: g -> (g, g)
~~~~

This "forks" the PRNG, creating two children with different states.

The hope is that the states will be different enough that
pseudo-random values generated from each will not be obviously
correlated.

~~~~ {.haskell}
withGen :: (StdGen -> a) -> IO a
withGen f = do
  g <- getStdGen
  let (g',g'') = split g
  setStdGen g'
  return (f g'')
~~~~

## Living in ghci

Now we can use our `guess` function reasonably easily.

~~~~ {.haskell}
>> let f = fst `fmap` withGen (guess . ((,) undefined))
>> f
1.2397265526054513
>> f
0.9506331164887969
~~~~

## Let's iterate

Here's a useful function from the `Prelude`:

~~~~ {.haskell}
iterate :: (a -> a) -> a -> [a]
iterate f x = x : iterate f (f x)
~~~~

Obviously that list is infinite.

Let's use `iterate` and `guess`, and as much other `Prelude` machinery
as we can think of, to write a function that can approximate $\pi$.

By the way, in case you don't recognize this technique, it's a famous
example of the family of
[Monte Carlo methods](http://en.wikipedia.org/wiki/Monte_Carlo_method).
