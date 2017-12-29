module Day06 (day06) where

import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Maybe as M

type Pos = Int

day06 :: IO ()
day06 = do
    putStrLn "Part I"
    print $ computeCycle $ Seq.fromList [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]
    putStrLn "Part II"
    print $ computeLooSize $ Seq.fromList [11, 11, 13, 7, 0, 15, 5, 5, 4, 4, 1, 1, 7, 1, 15, 11]

-- Find bank with the largest block and its postion
selectMemBank :: Seq.Seq Int -> (Int, Pos)
selectMemBank = Seq.foldlWithIndex foldF (0,0)
    where
        foldF acc index elem = if fst acc < elem then (elem, index) else acc


reallocate :: Seq.Seq Int -> (Int, Pos) -> Seq.Seq Int
reallocate banks block  = fst $ until ((== 0) . fst . snd) update (newBanks, block)
    where bankLength = Seq.length banks
          newBanks = Seq.update (snd block) 0 banks
          update (banks', block') = (Seq.adjust (+ 1) ( findNextBank (snd block')  bankLength ) banks',  dec block' bankLength)
          dec (value, pos) len = (value -1, ((`mod` len ) . (+1)) pos)
          findNextBank pos len =  ((`mod` len) . (+ 1)) pos

roundF :: Seq.Seq Int -> Seq.Seq Int
roundF banks = reallocate banks $ selectMemBank banks

takeWhileUnique :: Ord a => (a -> a) -> a -> Set.Set a -> Set.Set a
takeWhileUnique f x s = if Set.member p s then s else takeWhileUnique f p (Set.insert p s)
    where p = f x

computeCycle :: Seq.Seq Int -> Int
computeCycle banks = (+1) . Set.size $ takeWhileUnique roundF banks Set.empty

takeWhileUniqueL :: Eq a => (a -> a) -> a -> Seq.Seq a -> Seq.Seq a
takeWhileUniqueL f x s = if (M.isJust . Seq.elemIndexL p) s then s Seq.|> p else  takeWhileUniqueL f p (s Seq.|> p)
    where p = f x

loopSize :: Seq.Seq (Seq.Seq Int) -> Int
loopSize elems = (pos !! 1) - (pos !! 0)
    where len = Seq.length elems
          lastE = Seq.index elems (len - 1)
          pos = Seq.elemIndicesL lastE elems

computeLooSize :: Seq.Seq Int -> Int
computeLooSize banks = loopSize $ takeWhileUniqueL roundF banks Seq.empty

