{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant multi-way if" #-}


module Lib (lev, levCached, levCachedS) where

import Control.Monad.Fix (fix)
import Control.Monad.State.Strict (State, gets, modify, runState)
import Data.Array (Array, array)
import qualified Data.Array as A
import Data.Functor (($>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)

-- naive levensthein

lev :: Eq a => [a] -> [a] -> Int
lev [] x = length x
lev x [] = length x
lev (x : xs) (y : ys) =
  if x == y
    then lev xs ys
    else 1 + minimum [lev (x : xs) ys, lev xs (y : ys), lev xs ys]

-- caching distances

type C = Map (Int, Int) Int

type L a = (Int, [a])

-- explicit state caching levensthein

cacheIn :: Ord k => Map k b -> k -> (Map k b, b) -> (Map k b, b)
cacheIn c k f = case M.lookup k c of
  Nothing -> let (c', x) = f in (M.insert k x c', x)
  Just x' -> (c, x')

type S = C -> (C, Int)

cachingC :: Eq a => L a -> L a -> S
cachingC x@(nx, _) y@(ny, _) c = cacheIn c (nx, ny) $ go x y c

go :: Eq a => L a -> L a -> S
go xs (_, []) c = (c, length xs)
go (_, []) ys c = (c, length ys)
go lx@(nx, x : xs) ly@(ny, y : ys) c =
  let nlx = (nx + 1, xs)
      nly = (ny + 1, ys)
      nlev1 = cachingC nlx nly c
   in if x == y
        then nlev1
        else
          let (c', r') = nlev1
              (c'', r'') = cachingC lx nly c'
              (c''', r''') = cachingC nlx ly c''
           in (c''', 1 + minimum [r', r'', r'''])

levCached :: Eq a => [a] -> [a] -> Int
levCached xs ys = snd $ cachingC (0, xs) (0, ys) mempty

-- monadic state caching

type S' = State C Int -- isomorhpic to C -> (C, Int)

cachingCS :: Eq a => L a -> L a -> S'
cachingCS x@(nx, _) y@(ny, _) = cacheInS (nx, ny) $ goS x y

goS :: Eq a => L a -> L a -> S'
goS (_, xs) (_, []) = pure $ length xs
goS (_, []) (_, ys) = pure $ length ys
goS lx@(nx, x : xs) ly@(ny, y : ys) = do
  let nlx = (nx + 1, xs)
      nly = (ny + 1, ys)
  d <- cachingCS nlx nly
  if x == y
    then pure d
    else
      succ . minimum . (d :)
        <$> sequence [cachingCS lx (succ ny, ys), cachingCS (succ nx, xs) ly]

cacheInS :: (Int, Int) -> S' -> S'
cacheInS k f = do
  r <- gets $ M.lookup k
  case r of
    Nothing -> do
      x <- f
      modify (M.insert k x) $> x
    Just x' -> pure x'

-- levCachedS :: Eq a => [a] -> [a] -> Int
levCachedS :: Eq a => [a] -> [a] -> (Int, C)
levCachedS xs ys = runState (cachingCS (0, xs) (0, ys)) mempty

--- rows

cacheRows :: C -> [[Int]]
cacheRows m = fromMaybe [] $ do
  (((l, h), _), _) <- M.maxViewWithKey m
  pure $ do
    x <- [0 .. l]
    pure $ do
      y <- [0 .. h]
      pure $ M.findWithDefault (-1) (x, y) m

run xs ys =
  mapM_ (\xs -> mapM_ (putStr . (<> " \t")) xs >> putStrLn "") $
    zipWith (:) (" " : fmap return xs <> [" "]) . (fmap return ys :) $
      fmap (fmap show) . cacheRows $
        snd $
          levCachedS xs ys

levCachedArray :: Eq a => [a] -> [a] -> Int
levCachedArray xs ys =
  let bs@(bx, by) = (length xs, length ys)
   in (A.! (0, 0)) $ fix $ \a ->
        Data.Array.array ((0, 0), bs) $
          [((x, by), bx - x) | x <- [0 .. bx]]
            <> [((bx, y), by - y) | y <- [0 .. by]]
            <> do
              (nx, x) <- zip [0 ..] xs
              (ny, y) <- zip [0 ..] ys
              pure $
                ((nx, ny),) $
                  if
                      | x == y -> a A.! (succ nx, succ ny)
                      | otherwise -> 1 + minimum ((a A.!) <$> [(succ nx, succ ny), (nx, succ ny), (succ nx, ny)])
