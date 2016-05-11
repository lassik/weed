import Control.Monad
import Data.Char
import Data.List hiding (group)
import Data.Maybe
import qualified Data.ByteString.Lazy as B
import System.Environment
import System.FilePath

-- Don't these exist in the standard library?
also f g x = f x && g x
alternatively f g x = f x || g x
stringToLower = map toLower

----------------------------------------------------------------------

data File = File
            { name :: String
            , bytes :: B.ByteString
            }

fileNamedWithBytes fileName fileBytes =
  File { name = takeFileName fileName, bytes = fileBytes }

fileNamed fileName =
  (B.readFile fileName) >>= return . fileNamedWithBytes fileName

isNamed goalName file = stringToLower goalName == stringToLower (name file)

nameStartsWith prefix file = prefix == take (length prefix) (name file)

nameEndsWith suffix file =
  suffix == drop slack nam
  where nam = name file; slack = length nam - length suffix

byteSizeIs goalSize file = goalSize == B.length (bytes file)

hasBytesAtOffset offset goalBytes file =
  goalBytes == B.take (B.length goalBytes) (B.drop offset $ bytes file)

startsWithBytes = hasBytesAtOffset 0

rawBytes :: [Int] -> B.ByteString
rawBytes = B.pack . map fromIntegral

ascii :: String -> B.ByteString
ascii = rawBytes . map ord

----------------------------------------------------------------------

data Weed = Weed
              { option :: String
              , title :: String
              , group :: String
              , fileMatches :: (File -> Bool)
              }

instance Show Weed where show = title

allWeedTypes = [
  Weed
  { option = "empty-file"
  , title = "empty file"
  , group = "empty"
  , fileMatches =
      ((byteSizeIs 0)
       `alternatively` (byteSizeIs 1 `also` startsWithBytes (rawBytes [0xa]))
       `alternatively` (byteSizeIs 1 `also` startsWithBytes (rawBytes [0xd]))
       `alternatively` (byteSizeIs 2 `also` startsWithBytes (rawBytes [0xd, 0xa])))
      `also` (not . isNamed "__init__.py")
      -- `also` not . any (x in file.dirparts for x in (".git", ".hg"))
  },
  Weed
  { option = "bak"
  , title = "backup file"
  , group = "backup"
  , fileMatches = nameEndsWith ".bak"
  },
  Weed
  { option = "ds-store"
  , title = "Mac OS X Finder folder settings"
  , group = "state"
  , fileMatches = isNamed ".DS_Store"
    `also` hasBytesAtOffset 4 (ascii "Bud1")
  },
  Weed
  { option = "dot-underscore"
  , title = "Mac OS X Finder file settings"
  , group = "state"
  , fileMatches = nameStartsWith "._"
    `also` startsWithBytes (rawBytes [0x00, 0x05, 0x16, 0x07])
  },
  Weed
  { option = "thumbs-db"
  , title = "Windows Explorer image thumbnail cache"
  , group = "cache"
  , fileMatches = isNamed "Thumbs.db"
    `also` hasBytesAtOffset 512 (rawBytes [0xfd, 0xff, 0xff, 0xff])
    `also` hasBytesAtOffset 524 (rawBytes [0x04, 0x00, 0x00, 0x00])
  }
  ]

----------------------------------------------------------------------

fileWeedType file =
  find (\weedType -> fileMatches weedType $ file) allWeedTypes

describeFileNamed :: String -> (IO String)
describeFileNamed fileName =
  fileNamed fileName >>=
  return . \file -> (fileName ++ ": " ++
                     maybe "unknown" show (fileWeedType file))

describeFilesNamed :: [String] -> (IO String)
describeFilesNamed fileNames =
  mapM describeFileNamed fileNames >>= return . unlines

main = getArgs >>= describeFilesNamed >>= putStr
