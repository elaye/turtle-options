module Turtle.Options.Parsers
( float
, percent
) where

import Control.Applicative ((<$>), (<*>), (<*), (*>))

import Text.Parsec (Parsec, many1, digit, char, option, oneOf, (<|>))

type Parser a = Parsec String () a

infixr 5 <++>
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
a <++> b = (++) <$> a <*> b

infixr 5 <:>
(<:>) :: Applicative f => f a -> f [a] -> f [a]
a <:>  b = (:) <$> a <*> b

number :: Parser String
number = many1 digit

plus :: Parser String
plus = char '+' *> number

minus :: Parser String
minus = char '-' <:> number

integer :: Parser String
integer = plus <|> minus <|> number

-- | Shamelessly taken from http://stackoverflow.com/a/31358854/2287402
float :: Parser Float
float = fmap rd $ integer <++> decimal <++> exponent
    where rd       = read :: String -> Float
          decimal  = option "" $ char '.' <:> number
          exponent = option "" $ oneOf "eE" <:> integer

percent :: Parser Float
percent = float <* char '%'