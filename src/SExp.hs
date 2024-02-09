--https://gist.github.com/heitor-lassarote/ea54d05aca2956efa29b47ebbf048cbd
module SExp where

import Data.Scientific
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

newtype Identifier = Identifier
  { getId :: String
  } deriving (Show)

data SExp
  = SSExp    SExp [SExp]  -- (foo 0 "hello" bar), (bar (baz 1)), (foo)
  | SInteger Integer  -- 42
  | SDouble Double
  | SString  String  -- "hello, world"
  | SBool    Bool  -- false, true
  | SId      Identifier  -- foo
  deriving (Show)

type Parser = Parsec
  -- The type for custom error messages. We have none, so use `Void`.
  Void
  -- The input stream type. Let's use `String` for now, but for
  -- better performance, you might want to use `Text` or `ByteString`.
  String

bool :: Parser Bool
bool = False <$ string "false" <|> True <$ string "true" <?> "boolean"

integer :: Parser Integer
integer = read <$> some numberChar <?> "integer"

str :: Parser String
str = label "string" $ between (char '"') (char '"') (takeWhileP Nothing (/= '"'))

identifier :: Parser Identifier
identifier = label "identifier" $ do
  first <- letterChar <|> char '_'
  rest <- many $ alphaNumChar <|> char '_'
  pure $ Identifier $ first : rest

skipSpace :: Parser ()
skipSpace = L.space
  space1
  (L.skipLineComment ";;")
  (L.skipBlockCommentNested "/*" "*/")

sexp :: Parser (SExp, [SExp])
sexp = label "S-expression" $ between (char '(') (char ')') ((,) <$> atom <*> many atom)

double :: Parser Double
double = do
  value <- L.signed skipSpace L.scientific
  pure $ toRealFloat value

atom :: Parser SExp
atom = choice
  [ SBool <$> bool,
    SString <$> str,
    uncurry SSExp <$> sexp,
    SDouble <$> double
  ]