import Control.Exception
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import System.FilePath
import System.Posix
import System.IO.Unsafe    -- for understanding, not recommended

readFiles :: [FilePath] -> IO L.ByteString
readFiles [] = return L.empty
readFiles (f:fs) = liftM2 L.append (L.readFile f)
                   (readFiles fs)

countLines :: FilePath -> IO ()
countLines dir =
     recDir dir >>= readFiles >>= print . L8.count '\n'

recDir :: FilePath -> IO [FilePath]
recDir dir = do
  ds <- openDirStream dir
  let protect m = m `onException` closeDirStream ds  -- Catch e.g. permission errors

      nextName = unsafeInterleaveIO $
                 protect (readDirStream ds) >>= checkName

      checkName "" = closeDirStream ds >> return []
      checkName "." = nextName
      checkName ".." = nextName
      checkName name = getSymbolicLinkStatus path >>= checkStat path
          where path = dir </> name

      checkStat path stat
          | isRegularFile stat = liftM (path :) nextName
          | isDirectory stat =
              liftM2 (++) (protect $ recDir path) nextName
          | otherwise = nextName

  nextName
