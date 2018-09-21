module Grammar
  ( grammar
  , Grammar(..)
  , AST(..)
  ) where

data AST
  = One
  | Zero
  deriving (Eq, Show)

data Grammar a = Grammar
  { parse :: String -> Maybe (a, String)
  , format :: a -> String
  }

(<|>) :: Grammar a -> Grammar a -> Grammar a
(<|>) = eitherGrammar

mapGrammar :: (a -> b) -> (b -> a) -> Grammar a -> Grammar b
mapGrammar f g a =
  Grammar
    { parse =
        \s ->
          case parse a s of
            Just (a', s') -> Just (f a', s')
            Nothing -> Nothing
    , format = format a . g
    }

eitherGrammar :: Grammar a -> Grammar a -> Grammar a
eitherGrammar a b =
  Grammar
    { parse =
        \s ->
          case parse a s of
            Just v -> Just v
            Nothing -> parse b s
    , format = \s -> format a s ++ format b s
    }

sequenceGrammar :: Grammar (a -> b) -> Grammar a -> Grammar b
sequenceGrammar a b =
  Grammar
    { parse =
        \s ->
          case parse a s of
            Just (a', s') ->
              case parse b s' of
                Just (b', s'') -> Just (a' b', s'')
                Nothing -> Nothing
            Nothing -> Nothing
    , format = undefined
    }

emptyGrammar :: Grammar a
emptyGrammar = Grammar {parse = const Nothing, format = const ""}

char :: Eq a => Char -> a -> Grammar a
char s a =
  let parse s' =
        case s' of
          [] -> Nothing
          (x:xs) ->
            if x == s
              then Just (a, xs)
              else Nothing
      format a' =
        if a == a'
          then [s]
          else ""
   in Grammar {parse = parse, format = format}

many :: Grammar a -> Grammar [a]
many g =
  let p xs s =
        case parse g s of
          Nothing -> Just (xs, s)
          Just (a', s') -> p (a' : xs) s'
      f = concatMap (format g)
   in Grammar {parse = p [], format = f}

grammar :: Grammar AST
grammar = char '1' One <|> char '0' Zero

data Parens
  = EmptyParens
  | NestedParens Parens
  deriving (Eq)

parens :: Grammar [Parens]
parens = many (emptyParens <|> nestedParens)
  where
    emptyParens = char '(' ()
