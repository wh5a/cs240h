module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Data.Monoid
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.IO
import System.FilePath
import System.Posix
import System.IO.Unsafe

data Chunk = Chunk { chunkData :: !L.ByteString
                   , chunkAtEOF :: !Bool } deriving (Show)

newtype Iter a = Iter { runIter :: Chunk -> Result a }

data Result a = Done { rResult :: a, rResidual :: Chunk }
              | NeedInput !(Iter a)
              | NeedIO !(IO (Result a))
              | Failed !SomeException

instance Monoid Chunk where
    mempty = Chunk L.empty False
    mappend (Chunk la eofa) (Chunk lb eofb) =
        Chunk (L.append la lb) (eofa || eofb)

instance Monad Iter where
    return a = Iter $ Done a
    m >>= k = Iter $ \c -> check (runIter m c)
        where check (Done a c)     = runIter (k a) c
              check (NeedInput m') = NeedInput (m' >>= k)
              check (NeedIO io)    = NeedIO (liftM check io)
              check (Failed e)     = Failed e
    fail msg = iterThrow (ErrorCall msg)

iterThrow :: (Exception e) => e -> Iter a
iterThrow e = Iter $ \_ -> Failed (toException e)

instance MonadIO Iter where
    liftIO io = Iter $ \c -> NeedIO $ try io >>= mkResult c
        where mkResult _ (Left e)  = return (Failed e)
              mkResult c (Right a) = return (Done a c)

instance MonadPlus Iter where
    mzero = fail "mzero"
    mplus itera0 iterb = go mempty itera0
        where go acc itera = Iter $ \c ->
                  let acc' = mappend acc c
                      check (NeedInput i) = NeedInput (go acc' i)
                      check (NeedIO io) = NeedIO (liftM check io)
                      check (Failed _) = runIter iterb acc'
                      check r = r
                  in check $ runIter itera c

instance Show (Result a) where
    show (Done _ c) = "Done _ " ++ show c
    show (NeedInput _) = "NeedInput"
    show (NeedIO _) = "NeedIO"
    show (Failed e) = "Failed " ++ show e

type Enumerator a = Iter a -> IO (Result a)

type Inum a = Iter a -> Iter (Result a)

chunkEOF :: Chunk
chunkEOF = Chunk L.empty True

getResult0 :: Result a -> IO a
getResult0 (Done a _)           = return a
getResult0 (NeedInput (Iter f)) = getResult0 (f chunkEOF)
getResult0 (NeedIO io)          = io >>= getResult0
getResult0 (Failed e)           = throwIO e

getResult :: (MonadIO m) => Result a -> m a
getResult (Done a _)           = return a
getResult (NeedInput (Iter f)) = getResult (f chunkEOF)
getResult (NeedIO io)          = liftIO io >>= getResult
getResult (Failed e)           = liftIO $ throwIO e

run :: (MonadIO m) => Iter a -> m a
run = getResult . NeedInput

(.|) :: Inum a -> Iter a -> Iter a
(.|) inum iter = inum iter >>= getResult
infixr 4 .|


cat0 :: Enumerator a -> Enumerator a -> Enumerator a
cat0 a b iter = a iter >>= check
    where check (NeedInput iter') = b iter'
          check (NeedIO io)       = io >>= check
          check r                 = return r

cat :: Inum a -> Inum a -> Inum a
cat a b iter = a iter >>= check
    where check (NeedInput iter') = b iter'
          check (NeedIO io)       = liftIO io >>= check
          check r                 = return r


iterCatch :: Iter a -> (SomeException -> Iter a) -> Iter a
iterCatch (Iter f0) handler = Iter (check . f0)
    where check (NeedInput (Iter f)) = NeedInput (Iter (check . f))
          check (NeedIO io)          = NeedIO (liftM check io)
          check (Failed e)           = NeedInput (handler e)
          check done                 = done

onFailed :: Iter a -> Iter b -> Iter a
onFailed iter cleanup = iter `iterCatch` \e -> cleanup >> iterThrow e

iterBracket :: Iter a -> (a -> Iter b) -> (a -> Iter c) -> Iter c
iterBracket before after action = do
  a <- before
  b <- action a `onFailed` after a
  _ <- after a
  return b

inumBracket :: Iter a -> (a -> Iter b) -> (a -> Inum c) -> Inum c
inumBracket before after inum iter =
    iterBracket before after (flip inum iter)


readLine :: Iter (Maybe L.ByteString)
readLine = Iter (go L.empty)
    where go acc (Chunk input eof)
              | not (L.null b) = Done (Just acca) (Chunk btail eof)
              | not eof        = NeedInput (Iter (go acca))
              | otherwise      = Done Nothing (Chunk acca eof)
              where (a, b) = L8.break (== '\n') input
                    acca = L.append acc a
                    btail = L.tail b


-- Return chunk that is non-empty of has EOF set
iterChunk :: Iter Chunk
iterChunk = Iter $ \c@(Chunk buf eof) ->
            if L.null buf && not eof
            then NeedInput iterChunk
            else Done c (Chunk L.empty eof)

-- Dump input to standard output
iterStdout :: Iter ()
iterStdout = do
  (Chunk buf eof) <- iterChunk
  liftIO $ L.putStr buf
  unless eof iterStdout

nlines0 :: Iter Int
nlines0 = Iter (go 0)
    where go n c0 = check (runIter readLine c0)
              where
                check (NeedInput (Iter f)) =
                    NeedInput (Iter (check . f))
                check (Done (Just _) c) = go (n + 1) c
                check (Done Nothing c)  = Done n c
                check (NeedIO r)        = NeedIO (liftM check r)
                check (Failed e)        = Failed e

nlines1 :: Iter Int
nlines1 = go 0
    where go n = readLine >>= check n
          check n (Just _) = go $! n + 1
          check n Nothing  = return n




type Codec a = Iter (L.ByteString, Maybe (Inum a))

runCodec :: Codec a -> Inum a
runCodec codec iter = do
  (input, mNext) <- codec
  maybe (inumPure input) (inumPure input `cat`) mNext $ iter

inumPure :: L.ByteString -> Inum a
inumPure buf (Iter f) = return (f (Chunk buf False))

inumNull :: Inum a
inumNull = inumPure L.empty


enumerateFile :: FilePath -> Enumerator a
enumerateFile path iter0 = bracket (openFile path ReadMode) hClose $ \h ->
    let go iter = do
          input <- S.hGetSome h 32752
          if S.null input
            then return (NeedInput iter)
            else check $ runIter iter $ Chunk (L.fromChunks [input]) False
        check (NeedInput iter) = go iter
        check (NeedIO iter)    = iter >>= check
        check result           = return result
    in go iter0

inumFile :: FilePath -> Inum a
inumFile path iter0 = liftIO $ enumerateFile path iter0

enumFile  :: FilePath -> Inum a
enumFile path = inumBracket (liftIO $ openFile path ReadMode)
                (liftIO . hClose) $ \h ->
    let inum = runCodec $ do
          input <- liftIO $ S.hGetSome h 32752
          let next = if S.null input then Nothing else Just inum
          return (L.fromChunks [input], next)
    in inum


xargsCat :: Inum a
xargsCat iter = do
  mpath <- readLine
  case mpath of
    Nothing   -> return (NeedInput iter)
    Just path -> inumFile (L8.unpack path) `cat` xargsCat $ iter


enumDir :: FilePath -> Inum a
enumDir dir = inumBracket (liftIO $ openDirStream dir)
              (liftIO . closeDirStream) $ \ds ->
  let inum = runCodec nextName
      nextName = liftIO (readDirStream ds) >>= checkName

      checkName "" = return (L.empty, Nothing)
      checkName "." = nextName
      checkName ".." = nextName
      checkName name = liftIO (getSymbolicLinkStatus path)
                       >>= checkStat path
          where path = dir </> name

      checkStat path stat
          | isRegularFile stat =
              return (L8.pack $ path ++ "\n", Just inum)
          | isDirectory stat =
              return (L.empty, Just $ enumDir path `cat` inum)
          | otherwise = nextName
  in inum

enumerateNull :: Enumerator a
enumerateNull = return . NeedInput

-- | Recursively enumerate all regular files in a directory and all
-- subdirectories.
recDir :: FilePath -> IO [FilePath]
recDir dir = do
  ds <- openDirStream dir
  let protect m = m `onException` closeDirStream ds
      nextName = protect (readDirStream ds) >>= checkName
      checkName "" = closeDirStream ds >> return []
      checkName "." = nextName
      checkName ".." = nextName
      checkName name = getSymbolicLinkStatus path >>= checkStat path
          where path = dir </> name
      checkStat path stat
          | isRegularFile stat = liftM (path :) nextName
          | isDirectory stat   = liftM2 (++) (recDir path) nextName
          | otherwise          = nextName
  nextName

countLines0 :: FilePath -> IO Int
countLines0 dir = do
  files <- recDir dir
  let enumerator = foldr cat0 enumerateNull $ map enumerateFile files
  enumerator nlines1 >>= getResult0

main :: IO ()
main = return ()
