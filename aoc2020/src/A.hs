-- |

module A(module A, module Prelude, module Text.Parsec) where

import           Text.Parsec           hiding (count, parse, uncons)
import qualified Text.Parsec           as Parsec
import           Text.Parsec.Expr
import           Data.List

type Parser = Parsec String ()

parse :: Parser a            -- ^ The parser for "a"s
      -> String              -- ^ The string to be parsed
      -> Either ParseError a -- ^ The successfully parsed value or an error
parse p = Parsec.parse p ""
