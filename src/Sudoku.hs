{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Sudoku (run) where

import Control.Monad (guard)
import Data.Foldable (Foldable (..), minimumBy)
import Data.Map.Lazy (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (maybeToList)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set


-- a point on the board
type P = (Int, Int)

type M = Map P

-- constraints
type V = Set P

-- constraints per point
type C = M V

merge :: (Ord k, Ord x) => [Map k (Set x)] -> Map k (Set x)
merge = M.unionsWith Set.union

mkC :: C
mkC = merge $ mkCs <$> [rowsI, colsI, boxesI]
  where
    mkC1 :: Set P -> C
    mkC1 s = merge $ do
      x <- toList s
      pure $ M.singleton x $ Set.delete x s

    mkCs = foldl' (\c s -> merge [mkC1 s, c]) mempty

    rowsI = [Set.fromList [(x, y) | y <- [1 .. 9]] | x <- [1 .. 9]]

    colsI = [Set.fromList [(x, y) | x <- [1 .. 9]] | y <- [1 .. 9]]

    boxesI = do
      x0 <- [0, 3, 6]
      y0 <- [0, 3, 6]
      pure $ Set.fromList $ do
        x <- [1 .. 3]
        y <- [1 .. 3]
        pure (x0 + x, y0 + y)

type Choices = Set Char

choices :: M Char -> M Choices
choices = fmap $ \case
  '.' -> Set.fromList "123456789"
  c -> Set.singleton c

prune :: M Choices -> (M Choices, M Char)
prune m =
  ( foldl' (\q p -> M.delete (fst p) . pruneC q $ p) m pruners,
    M.fromList pruners
  )
  where
    pruners :: [(P, Char)]
    pruners = do
      (p, x) <- M.assocs m
      guard $ length x == 1
      pure (p, Set.findMin x)

    pruneC :: M Choices -> (P, Char) -> M Choices
    pruneC mx (q, v) =
      let ds = M.findWithDefault mempty q mkC
       in foldl' (flip $ M.adjust (Set.delete v)) mx ds

data B = B Q (M Char)
  deriving (Show)

type Q = M (Map Char V)

mkQ :: M Choices -> Q
mkQ m = M.fromList $ do
  (p, x) <- M.assocs m
  pure $ (p,) $ merge $ do
    v <- toList x
    c' <- toList $ mkC M.! p
    let s = case M.lookup c' m of
          Nothing -> mempty
          Just s' -> if Set.member v s' then Set.singleton c' else mempty
    pure $ M.singleton v s

mkB :: (M Choices, M Char) -> B
mkB (m, v) = B (mkQ m) v

load :: String -> B
load s =
  mkB $
    prune $
      choices $
        M.fromList $
          let ps = [(x, y) | y <- [1 .. 9], x <- [1 .. 9]]
           in zip ps s

renderW :: M Char -> String
renderW w = do
  y <- [1 .. 9]
  x <- [1 .. 9]
  case M.lookup (x, y) w of
    Nothing -> "."
    Just c -> [c]
    <> if x == 9 then "\n" else ""

removeLink :: P -> Char -> P -> Q -> Q 
removeLink l v = M.adjust (M.adjust (Set.delete l) v)

reverseC :: Map Char V -> [(P, Char)]
reverseC m = do
  (v, ps) <- M.assocs m
  p <- toList ps
  pure (p, v)

takeSmaller :: Q -> Maybe (P, Map Char V)
takeSmaller q
  | null q = Nothing
  | otherwise = Just $ minimumBy (comparing (length . snd)) . M.assocs $ q

pickmove :: B -> [B]
pickmove b@(B m w) = case takeSmaller m of
  Nothing -> [b]
  Just (p, x) -> do
    let m' = foldl' (\ml (p', v) -> removeLink p v p' ml) m $ reverseC x
    (v, ds) <- M.assocs x
    m'' <- maybeToList (assign p v ds m')
    pickmove $ B m'' $ M.insert p v w

assign :: P -> Char -> V -> Q -> Maybe Q
assign p v ds q =
  let r :: Maybe Q -> P -> Maybe Q
      r Nothing _ = Nothing
      r (Just q') d =
        let z = q' M.! d
            h = z M.! v
            z' = M.delete v z
            q''' = foldl' (flip $ removeLink d v) q' $ toList h
         in if M.keys z == [v] then Nothing else Just $ M.insert d z' q'''
   in M.delete p <$> foldl' r (Just q) ds

run :: [Char] -> IO ()
run = mapM_ report . pickmove . load
  where
    report (B _ w) = putStrLn $ renderW w