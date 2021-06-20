module Data.Combinator where

infixl 4 <^>

-- `f <^> x` replaces when you would use `f <*> pure x`
(<^>) :: Functor f => f (a -> b) -> a -> f b
(<^>) ff x = fmap ($ x) ff
