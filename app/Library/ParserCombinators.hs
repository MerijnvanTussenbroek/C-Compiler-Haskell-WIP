module Library.ParserCombinators where

import Prelude hiding ((<$>),(<*>),(<|>))
import Library.ElementaryParsers

-- parser combinators
infixr 4 <|>
(<|>) :: Parser s a -> Parser s a -> Parser s a
(p <|> q) xs = p xs ++ q xs

(<<|>) :: Parser s a -> Parser s a -> Parser s a
p <<|> q = \xs -> let r = p xs in if null r then q xs else r

infixl 6 <*>
(<*>) :: Parser s (b -> a) -> Parser s b -> Parser s a
(p <*> q) xs = [ (y z, zs) | (y, ys) <- p xs, (z, zs) <- q ys]

infixl 7 <$>
(<$>) :: (a -> b) -> Parser s a -> Parser s b
(f <$> p) xs = [(f y, ys) | (y, ys) <- p xs]

(<$) :: b -> Parser s a -> Parser s b
(p <$ q) xs = [(p,ys) | (y,ys) <- q xs]

(*>) :: Parser s a -> Parser s b -> Parser s b
p *> q = (\_ a -> a) <$> p <*> q

(<*) :: Parser s a -> Parser s b -> Parser s a
p <* q = const <$> p <*> q