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
