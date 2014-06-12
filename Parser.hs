module Parser where

import Data.Text hiding (unlines)
import Data.Functor
import Control.Monad

import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec
import qualified Text.ParserCombinators.Parsec.Token as P
import Text.ParserCombinators.Parsec.Language

import Test.Hspec
import Test.QuickCheck

data BinOpExpr = Unit Integer
               | Negative BinOpExpr
               | Positive BinOpExpr
               | Product BinOpExpr BinOpExpr
               | Divide BinOpExpr BinOpExpr
               | Difference BinOpExpr BinOpExpr
               | Sum BinOpExpr BinOpExpr deriving (Show, Eq)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser
        (emptyDef
        {
          reservedOpNames = ["*","/","+","-"]
        })

natural    = P.natural lexer
parens     = P.parens lexer
reservedOp = P.reservedOp lexer

parseExpr parser input = parse parser "" input

parseBinExpr = parseExpr binaryOp

binaryOp = buildExpressionParser table term

term = binOpNatural <|>
       parens binaryOp

binOpNatural = do
  n <- natural
  return (Unit n)

table = [
  [prefix "-" Negative,          prefix "+" id],
  [binary "*" Product AssocLeft, binary "/" Divide AssocLeft],
  [binary "+" Sum AssocLeft,     binary "-" Difference AssocLeft]
  ]


binary  name fun assoc = Infix (do{ reservedOp name; return fun }) assoc
prefix  name fun       = Prefix (do{ reservedOp name; return fun })
postfix name fun       = Postfix (do{ reservedOp name; return fun })

parseBinaryOp = parse binaryOp ""

-- TODO: Add some tests!
should :: Example a => String -> a -> Spec
should x = it ("should " ++ x)

runTests = hspec $ do
  describe "Binary operation" $ do
    should "parse * above +" $ do
      (show $ parseBinaryOp "1 * (-3) + 1 * 8") `shouldBe`
        "Right (Sum (Product (Unit 1) (Negative (Unit 3))) (Product (Unit 1) (Unit 8)))"
    should "parse / above -" $ do
      (show $ parseBinaryOp "1 / (-3) - 1 / 8") `shouldBe`
        "Right (Difference (Divide (Unit 1) (Negative (Unit 3))) (Divide (Unit 1) (Unit 8)))"
