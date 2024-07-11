module Main where

import Text.Regex.TDFA ((=~))
import Options.Applicative
import Options.Applicative.Extra

data RTItem = RTPlain String
            | RTSubmatch Int
            | RTMatch

newtype ReplacementTemplate = ReplacementTemplate [RTItem]

startsPlain :: Char -> Bool
startsPlain '\\' = False
startsPlain '&' = False
startsPlain _ = True

readRef :: Char -> Maybe Int
readRef '1' = Just 1
readRef '2' = Just 2
readRef '3' = Just 3
readRef '4' = Just 4
readRef '5' = Just 5
readRef '6' = Just 6
readRef '7' = Just 7
readRef '8' = Just 8
readRef '9' = Just 9
readRef _ = Nothing

parseReplacement' :: String -> [RTItem]
parseReplacement' ('\\':cs) =
  case cs of
    "" -> []
    '\\':postEscd -> RTPlain "\\":parseReplacement' postEscd
    -- 'n':postEscd -> RTPlain "\n":parseReplacement' postEscd
    '&':postEscd -> RTPlain "&":parseReplacement' postEscd
    digit:postEscd ->
      case readRef digit of
        Just index -> RTSubmatch index:parseReplacement' postEscd
        Nothing -> parseReplacement' postEscd
parseReplacement' ('&':cs) =
  RTMatch: parseReplacement' cs
parseReplacement' "" = []
parseReplacement' replacement =
  let plain = RTPlain $ takeWhile startsPlain replacement
      rest = dropWhile startsPlain replacement
  in plain:parseReplacement' rest

parseReplacement :: String -> ReplacementTemplate
parseReplacement source = ReplacementTemplate $ parseReplacement' source

safeIndex :: Int -> [a] -> Maybe a
safeIndex i list =
  if i < length list
  then Just $ list !! i
  else Nothing

formatReplacement' :: String -> [String] -> [RTItem] -> Maybe String
formatReplacement' _ _ [] = Just ""
formatReplacement' match submatches (item:restItems) = do
  prefix <- case item of
    RTPlain plainText -> Just plainText
    RTSubmatch i -> safeIndex (i - 1) submatches
    RTMatch -> Just match
  rest <- formatReplacement' match submatches restItems
  Just $ prefix ++ rest

formatReplacement :: String -> [String] -> ReplacementTemplate -> Maybe String
formatReplacement match submatches (ReplacementTemplate items) =
  formatReplacement' match submatches items

sInLine :: String -> ReplacementTemplate -> String -> String
sInLine pattern rt line =
  if line =~ pattern
  then
    let (before, match, after, submatches) =
          line =~ pattern :: (String, String, String, [String])
        replacementText = formatReplacement match submatches rt
    in case replacementText of
      Just text -> before ++ text ++ after
      Nothing -> line
  else line

sAllInLine :: String -> ReplacementTemplate -> String -> String
sAllInLine pattern rt line =
  if line =~ pattern
  then
    let (before, match, after, submatches) =
          line =~ pattern :: (String, String, String, [String])
        replacementText = formatReplacement match submatches rt
    in case replacementText of
      Just text -> before ++ text ++
       sAllInLine pattern rt after
      Nothing -> line
  else line

data DelimMode = Delim Char | NoDelim

isDelim :: DelimMode -> Char -> Bool
isDelim (Delim c_delim) c = c_delim == c
isDelim NoDelim _ = False

putDelim :: DelimMode -> IO ()
putDelim (Delim c_delim) = putChar c_delim
putDelim NoDelim = pure ()

substitute :: DelimMode -> Bool -> String -> String -> String -> IO ()
substitute _ _ _ _ "" = pure ()
substitute delimMode isGlobal pattern replacement original = do
  let first = takeWhile (not . isDelim delimMode) original
  let rest = drop 1 $ dropWhile (not . isDelim delimMode) original
  let replacementTemplate = parseReplacement replacement
  let newFirst =
        if isGlobal
        then sAllInLine pattern replacementTemplate first
        else sInLine pattern replacementTemplate first
        
  putStr newFirst
  putDelim delimMode

  substitute delimMode isGlobal pattern replacement rest

data CmdLnOptions = CmdLnOptions Bool DelimMode String String (Maybe String)

cmdln_global :: Parser Bool
cmdln_global = switch (
  long "global" <>
  short 'g' <>
  help "Match all instances.")

cmdln_delim :: Parser DelimMode
cmdln_delim = Delim <$> option auto (
  long "delimeter" <>
  short 'd' <>
  help "Delimeter for e.g. lines of input." <>
  metavar "DELIM")

cmdln_noDelim :: Parser DelimMode
cmdln_noDelim = flag' NoDelim (
  long "no-delimeter" <>
  short 'n' <>
  help "Consume input without breaking into e.g. lines.")

cmdln_delimMode :: Parser DelimMode
cmdln_delimMode = cmdln_delim <|> cmdln_noDelim <|> pure (Delim '\n')

cmdln_pattern :: Parser String
cmdln_pattern = argument str (
  help "Pattern to match against." <>
  metavar "PAT")

cmdln_replacement :: Parser String
cmdln_replacement = argument str (
  help "Replacement. May reference match and submatches." <>
  metavar "REPLACE")

cmdln_filename :: Parser (Maybe String)
cmdln_filename = optional $ argument str (
  help "Name of file to read from." <>
  metavar "FILE")

cmdln_options :: Parser CmdLnOptions
cmdln_options = CmdLnOptions <$>
  cmdln_global <*>
  cmdln_delimMode <*>
  cmdln_pattern <*>
  cmdln_replacement <*>
  cmdln_filename

sHelper :: Parser (a -> a)
sHelper = helperWith (
  long "help" <>
  short 'h' <>
  help "Show this help text." <>
  hidden)

sOptions :: ParserInfo CmdLnOptions
sOptions = info (cmdln_options <**> sHelper) (
  fullDesc <>
  header "substitute: regular expression finder-replacer." <>
  progDesc
    "substitute works a little like the s command of sed and other \
    \programs. It takes a pattern and a replacement string; uses these to \
    \perform a find-replace operation, on either a file or the standard \
    \input; and prints the result to the standard output.")
  
main :: IO ()
main = do
  options <- execParser sOptions
  let CmdLnOptions global delimMode pattern replacement maybeFilename = options
  
  doc <- case maybeFilename of
    Just filename -> readFile filename
    Nothing -> getContents
  substitute delimMode global pattern replacement doc
