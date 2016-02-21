{-# LANGUAGE OverloadedStrings #-}

module Turtle.Options.Timecode
( Timecode(..)
, RelTimecode(..)
, optTimecode
, defTimecodeHelp
, (<+>)
) where

import Turtle (ArgName, ShortName, HelpMessage, opt)
import Data.Optional (Optional)
import qualified Turtle
import qualified Data.Text as Text

--import Data.Monoid (Sum, (<>))
import Data.Monoid (Monoid, mappend)

import Text.Parsec
import Text.ParserCombinators.Parsec.Error (Message(..), newErrorMessage)
import Text.Parsec.Pos (initialPos)

import Turtle.Options.Parsers (Parser, percent, float, number, plus, minus)

import Debug.Trace (traceShow)

defTimecodeHelp :: Optional HelpMessage
defTimecodeHelp = "Timecode option. TIMECODE can be in the following formats: "

type Hour = Int
type Minute = Int
type Second = Int
type Millisecond = Int

data Timecode = Timecode Hour Minute Second Millisecond deriving (Eq)

data RelTimecode = 
  PosTimecode Timecode
  | NegTimecode Timecode
  deriving (Eq)de

instance Show Timecode where
  show (Timecode h m s ms) = (show h) ++ ":" ++ (show m) ++ ":" ++ (show s) ++ "." ++ (show ms)

instance Show NegativeTimecode where
  show (PosTimecode t) = show t
  show (NegTimecode t) = "-" ++ (show t)

instance Monoid Timecode where
  mappend (Timecode ha ma sa msa) (Timecode hb mb sb msb) = normalizeTimecode (Timecode (ha + hb) (ma + mb) (sa + sb) (msa + msb))
  mempty = Timecode 0 0 0 0

infixr 5 <+>
(<+>) :: Timecode -> Timecode -> Timecode
a <+> b = mappend a b

normalizeTimecode :: Timecode -> Timecode
normalizeTimecode (Timecode h m s ms) = Timecode newH newM  newS newMs
  where
    msTotal = ms + 1000 * (s + 60 * (m + 60 * h))  
    newMs = msTotal `mod` 1000
    sLeft = (msTotal - newMs) `div` 1000
    --newS = ((msTotal - newMs) `div` 1000) `mod` 60 
    newS = sLeft `mod` 60 
    --newM = ((((msTotal - newMs) `div` 1000) - newS) `div` 60) `mod` 60
    mLeft = (sLeft - newS) `div` 60
    newM = mLeft `mod` 60
    --newH = (((((msTotal - newMs) `div` 1000) - newS) `div` 60) - newM) `div` 60
    newH = (mLeft - newM) `div` 60

normalTimecode :: Parser Timecode
normalTimecode = do
  --plus <|> minus
  ts <- number `sepBy1` char ':'
  ms <- read <$> (option "0" $ char '.' *> number)
  return $ case (fmap read ts) of
    (h:m:s:[]) -> toTimecode h m s ms
    (m:s:[]) -> toTimecode 0 m s ms
    (s:[]) -> toTimecode 0 0 s ms

toTimecode :: Int -> Int -> Int -> Int -> Timecode
toTimecode h m s ms = normalizeTimecode (Timecode h m s ms)

msToTimecode :: Int -> Timecode
msToTimecode ms = Timecode h m s (traceShow newMs newMs)
  where
    newMs = ms `mod` 1000
    s = ((ms - newMs) `div` 1000) `mod` 60
    m = ((ms - newMs) `div` (60 * 1000)) `mod` 60 
    h = ((ms - newMs) `div` (60 * 60 * 1000)) `mod` 60

sToTimecode :: Int -> Timecode
sToTimecode s = Timecode h m (traceShow newS newS) 0
  where
    newS = s `mod` 60
    m = (s - newS) `div` 60 
    h = m `mod` 60

mToTimecode :: Int -> Timecode
mToTimecode m = Timecode h newM 0 0
  where
    newM = m `mod` 60
    h = ((m - newM) `div` 60) `mod` 60

hToTimecode :: Int -> Timecode
hToTimecode h = Timecode h 0 0 0

timecode :: Parser Timecode
timecode = normalTimecode

readTimecode :: String -> Maybe Timecode
readTimecode str = case (parse timecode "Timecode" str) of
  Left err -> error $ "Error parsing timecode: " ++ (show err)
  Right s -> Just s

optTimecode :: ArgName -> ShortName -> Optional HelpMessage -> Turtle.Parser Timecode
optTimecode = opt (readTimecode . Text.unpack)