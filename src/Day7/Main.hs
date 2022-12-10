module Main where

import Control.Exception (throw)
import Data.Map qualified as M
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Relude.Extra.Lens (Lens', lens, over, view)
import Text.Megaparsec (
  Parsec,
  choice,
  runParser,
  some,
 )
import Text.Megaparsec.Char (
  alphaNumChar,
  char,
  hspace,
  hspace1,
  newline,
  space,
  string,
 )
import Text.Megaparsec.Char.Lexer (decimal)
import Prelude hiding (some)

type Parser = StateT FSState (Parsec Void Text)

type Filename = Text

_newtype :: forall a b. Coercible a b => Lens' a b
_newtype = lens coerce (\_ n -> coerce n)

newtype CWD = CWD [Filename]
  deriving newtype (Show)

_path :: Lens' CWD [Filename]
_path = _newtype

data FSState = FSState
  { fileSystem :: FileSystem
  , current :: CWD
  }
  deriving stock (Show)

_fileSystem :: Lens' FSState FileSystem
_fileSystem = lens (\(FSState fs _) -> fs) (\s fs -> s {fileSystem = fs})

_cwd :: Lens' FSState CWD
_cwd = lens (\(FSState _ cwd) -> cwd) (\s cwd -> s {current = cwd})

data File
  = F Word
  | D Directory
  deriving stock (Show)

data Directory = Directory (Map Filename File) Word
  deriving stock (Show)

newtype FileSystem = Root Directory
  deriving stock (Show)

_root :: Lens' FileSystem Directory
_root = _newtype

showTree :: FileSystem -> Text
showTree (Root r@(Directory _ rs)) =
  "- / (dir, size="
    <> show rs
    <> ")"
    <> showTree' 1 r
  where
    indent = "  "

    showTree' level (Directory m _) =
      foldMap (uncurry $ showEntry level) (M.toList m)

    showEntry level name f =
      "\n"
        <> T.replicate level indent
        <> "- "
        <> name
        <> " "
        <> showFile level f

    showFile _ (F size) =
      "(file, "
        <> "size="
        <> show size
        <> ")"
    showFile level (D d@(Directory _ size)) =
      "(dir, "
        <> "size="
        <> show size
        <> ")"
        <> showTree' (level + 1) d

unionDir :: Directory -> Directory -> Directory
unionDir (Directory d1 s1) (Directory d2 s2) =
  Directory (M.union d1 d2) (s1 + s2)

sizeOfEntries :: Map Filename File -> Word
sizeOfEntries =
  M.foldl'
    ( \a -> \case
        F s -> a + s
        D (Directory _ s) -> a + s
    )
    0

setDirAtPath ::
  [Filename] ->
  Directory ->
  Directory ->
  Parser Directory
setDirAtPath (reverse -> path) = go path
  where
    go [] dN dO =
      pure $ unionDir dN dO
    go (x : xs) dCN (Directory mP _) = do
      let nextLevel m = Just . D <$> setDirAtPath xs dCN m

      mN <-
        M.alterF
          ( \case
              Nothing -> nextLevel (Directory M.empty 0)
              Just (D dC) -> nextLevel dC
              _ -> fail "Cannot union a file and a directory"
          )
          x
          mP

      let sN = sizeOfEntries mN

      pure $ Directory mN sN

pFileName :: Parser Filename
pFileName = some (alphaNumChar <|> char '.' <|> char '-') <&> T.pack

pCd :: Parser ()
pCd =
  let modifyCwd = modify . over (_cwd . _path)
   in string "cd" *> hspace1
        *> choice
          [ string ".."
              >> modifyCwd
                ( \case
                    _ : xs -> xs
                    [] -> []
                )
          , char '/' >> modifyCwd (const [])
          , pFileName >>= \x -> modifyCwd (x :)
          ]
          <* newline

type Entry = (Filename, File)

pLs :: Parser ()
pLs = do
  void $ string "ls" *> hspace *> newline

  entries <- many pLsEntry <&> M.fromList
  let dir = Directory entries $ sizeOfEntries entries

  (FSState (Root r) cwd@(CWD path)) <- get
  r' <- setDirAtPath path dir r
  put $ FSState (Root r') cwd

pLsEntry :: Parser Entry
pLsEntry =
  let pDirEntry =
        string "dir"
          *> hspace1
          *> pFileName
          <&> (,D $ Directory M.empty 0)
      pFileEntry = do
        size <- decimal
        hspace1
        filename <- pFileName
        pure (filename, F size)
   in (pDirEntry <|> pFileEntry) <* newline

pLog :: Parser FileSystem
pLog =
  some
    (char '$' *> hspace1 *> (pCd <|> pLs) *> space)
    *> get
    <&> view _fileSystem

foldlDirs :: (a -> Directory -> a) -> a -> Directory -> a
foldlDirs f a d@(Directory m _) =
  let a' = f a d

      rest =
        M.foldl
          ( \a'' -> \case
              F _ -> a''
              D d' -> foldlDirs f a'' d'
          )
          a'
          m
   in rest

sumAllDirsSmallerOrEqualTo100000 :: FileSystem -> Word
sumAllDirsSmallerOrEqualTo100000 (Root r) =
  foldlDirs
    ( \a (Directory _ size) ->
        if size <= 100000
          then a + size
          else a
    )
    0
    r

liftEither (Left err) = throw err
liftEither (Right x) = pure x

parse p =
  runParser $
    evalStateT p $
      FSState (Root $ Directory M.empty 0) (CWD [])

main :: IO ()
main = do
  fs <- TIO.getContents >>= (parse pLog "<stdin>" >>> liftEither)

  TIO.putStrLn $ showTree fs

  print $ sumAllDirsSmallerOrEqualTo100000 fs

ls :: Directory -> IO ()
ls (Directory m _) =
  mapM_
    ( uncurry $ \f ->
        ( \case
            F s -> show s <> " " <> f
            D _ -> "dir " <> f
        )
          >>> TIO.putStrLn
    )
    $ M.toList m
