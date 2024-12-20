{-# LANGUAGE Strict #-}

import Control.Monad
import Data.Function (on)
import Data.List (delete, find, findIndex, insert, nub, sortBy, union, zipWith4, (\\))
import Data.Maybe
import qualified Data.Set as S
import System.IO
import System.Random

newtype Pid a = Pid {pi :: a} deriving (Eq, Show, Ord)

newtype Aid a = Aid {ai :: a} deriving (Eq, Show, Ord)

data Proposer = P {pid :: Pid Int, pref :: [Aid Int]}
  deriving (Eq, Show, Ord)

data Accepter = A
  { aid :: Aid Int,
    prio :: [Pid Int],
    quota :: Int,
    enrol :: S.Set (Pid Int)
  }
  deriving (Eq, Show, Ord)

type Matching = S.Set (Aid Int, S.Set (Pid Int))

type Market = (S.Set Proposer, S.Set Accepter)

type Action = Either (Pid Int) (Aid Int)

-- newtype Set a = Set {originList :: [a]}

-- instance Functor Set where

--   fmap  f  (Set xs) = Set (map f (nub xs))

-- instance Ord (Maybe (Pid Int)) where
--    _ <= Nothing = True
--    Nothing <= Just _ = False
--    Just x <= Just y = x <= y

sortByKeys :: (Ord a) => [a] -> [a] -> [a]
sortByKeys keys =
  sortBy
    ( compare
        `on` (\x -> if isNothing x then Just maxBound else x)
          . flip lookup (zip keys [0 :: Int ..])
    )

getMatching :: Market -> Matching
getMatching (_, ws) = S.map (\w -> (aid w, enrol w)) ws

hasactions :: Market -> S.Set Action
hasactions (ps, as) =
  let unenrolid = S.map pid ps `S.difference` S.unions (S.map enrol as)
      unenrol = S.filter ((`elem` unenrolid) . pid) ps
   in S.map (Left . pid) (S.filter (not . null . pref) unenrol)
        `S.union` S.map
          (Right . aid)
          ( S.filter
              ( \x ->
                  S.size (enrol x) > quota x || not (null (S.toList (enrol x) \\ prio x))
              )
              as
          )

dispatch :: Market -> Action -> Market
dispatch (ps, as) (Left p) =
  let Just proposer = find (\x -> p == pid x) ps
      newproposer = proposer {pref = tail (pref proposer)}
      Just acceptor =
        find
          (\y -> head (pref proposer) == aid y)
          as
      newacceptor = acceptor {enrol = S.insert p (enrol acceptor)}
   in ( changeItem proposer newproposer ps,
        changeItem acceptor newacceptor as
      )
  where
    changeItem old new set = S.insert new (S.delete old set)
dispatch (ps, as) (Right a) =
  let Just rejector = find (\y -> a == aid y) as
      newrejector =
        rejector
          { enrol =
              S.fromList $
                take
                  (quota rejector)
                  ( takeWhile
                      (`elem` prio rejector)
                      (sortByKeys (prio rejector) (S.toList (enrol rejector)))
                  )
          }
   in (ps, S.insert newrejector (S.delete rejector as))
deferAcceptMonad :: Market -> [Market]
deferAcceptMonad m =
    let actions = S.toList (hasactions m)
     in if not (null actions)
            then do
                action <- actions
                deferAcceptMonad (dispatch m action)
            else return m


deferAccepts :: Market -> S.Set Market
deferAccepts m =
  let actions = hasactions m
   in if not (null actions)
        then S.unions $ S.map deferAccepts (S.unions (S.map (S.singleton . (dispatch m)) actions))
        else S.singleton m


effiDA :: Market -> Market
effiDA m =
  let actions = hasactions m
   in if not (null actions)
        then effiDA (foldl dispatch m actions)
        else m

-- instance Num (Pid Int) where
-- ms = [P (Pid 1) (map Aid [2,1,3]),P (Pid 2) (map Aid [1,3,2]),P (Pid 3) (map Aid [1,2,3])]
-- ws =  [A (Aid 1) (map Pid [1,3,2]) 1 [], A (Aid 2) (map Pid [3,1,2]) 1 [], A (Aid 3) (map Pid [1,3,2]) 1 []]

-- results ::  [Market]
-- results = deferAccepts (ms,ws)
-- result :: Market
-- result = head results

stable :: Market -> Matching -> Bool
stable (mso, wso) matching =
  null
    ( do
        m <- ms
        w <- ws
        guard $ pid m `elem` (snd . head) (filter (\x -> fst x == aid w) (S.toList matching))
        guard $ any (\wm -> block m wm matching) (filter (\x -> aid x `elem` takeWhile (/= aid w) (pref m)) ws)
        return 1
    )
    && null
      [ 1 | m <- ms, pid m `notElem` S.unions (S.map snd matching), any (\wm -> block m wm matching) (filter (\x -> aid x `elem` pref m) ws)
      ]
  where
    ms = S.toList mso
    ws = S.toList wso
    -- toPids :: Maybe (S.Set (Pid Int)) -> S.Set (Pid Int)
    toPids Nothing = S.empty
    toPids (Just a) = a
    block m w matching =
      pid m `elem` prio w
        && pid m `elem` take (quota w) (sortByKeys (prio w) (pid m : S.toList (toPids (lookup (aid w) (S.toList matching)))))

randomPref :: (RandomGen p) => Int -> Int -> p -> [[Int]]
randomPref m w gen =
  let total = take (m * w) (randomRs (1, w) gen)
   in helper w total
  where
    helper w ws =
      let (mpref, remaining) = splitAt w ws
       in nub mpref : helper w remaining

-- main :: IO ()
-- main = do
--   --print (sortByKeys [6, 4, 8, 9,3,2,1] [1 .. 10])
--     print result
--     print (length results)
--     print (all ( == result) results)
allMatching :: Market -> [Matching]
allMatching (ms, ws) = if null ms
                        then return (getMatching (ms, ws))
                        else  do
                         m <- S.toList ms
                         w <- S.toList ws
                         if all (\w -> (length . enrol) w + 1 > quota w) ws
                           then return (getMatching (ms, ws))
                           else do
                            guard ((length . enrol) w + 1 <= quota w)
                            let ms'= S.delete m ms
                            let ws'= S.insert (w{enrol = S.insert (pid m) (enrol w)}) (S.delete w ws)
                            allMatching (ms',ws')

proposerBetter :: Market -> Matching -> Matching -> Bool
proposerBetter (ps,as) m1 m2 = null [1| (a, ps) <- m, p <- S.toList ps, (a', ps') <- m', p' <- S.toList ps',
                             p == p', if isNothing (findIndex (==a) (pidpref p))
                                      then not $ isNothing (findIndex (==a') (pidpref p))
                                      else findIndex (==a) (pidpref p) < findIndex (==a') (pidpref p)]

  where m = S.toList m1
        m'= S.toList m2
        
        pidpref p = pref (head (filter ( \x -> pid x == p) (S.toList ps)))
--proposerBetter (ms, ws) (_, ws') = null [1 | m <- ms, pid m `notElem` (foldr union [] (map enrol ws)), pid m `elem` (foldr union [] (map enrol ws'))]
--                                        &&
--                                    null [1 | m <- ms, w <- ws, pid m `elem` (enrol . head) (filter (\x -> aid x == aid w) ws),
--                                          w' <- ws', pid m `elem` (enrol . head) (filter (\x -> aid x == aid w') ws'),
--                                          findIndex (==aid w') (pref m) < findIndex (==aid w) (pref m)]

proposerOptimal :: Market -> Matching -> S.Set Matching -> Bool
proposerOptimal (ps,as) m = all (proposerBetter (ps, as) m)
-- main :: IO ()
main = do
  
  putStrLn "Please tell how many proposers are there?"
  men <- getLine
  let m = (read men :: Int)
  putStrLn "Please tell how many acceptors are there?"
  women <- getLine
  let w = (read women :: Int)
  putStrLn "Please tell what is the quota upper bound for the acceptors?"
  quotaUpper <- getLine
  let q = (read quotaUpper :: Int)
  gen <- getStdGen
  let quotaList = randomRs (1, q) gen
  
  gen' <- newStdGen
  let ms = zipWith P (map Pid [1 .. m]) (map (map Aid) (randomPref m w gen'))
  gen'' <- newStdGen
  let ws = zipWith4 A (map Aid [1 .. w]) (map (map Pid) (randomPref w m gen'')) quotaList (repeat S.empty)
  
  putStrLn "Proposers have the following preference given randomly by our program:"
  print ms
  putStrLn "Acceptors have the following priorities for proposers and their quotas given randomly by our program:"
  print ws
 
 
  -- let results = deferAccepts (ms,ws)
  let (ps, as) = (S.fromList ms, S.fromList ws)
  let result = effiDA (ps, as) -- head results  
  let matching = getMatching result
  putStrLn "DA produce the following matching:"
  print matching
  putStrLn "Is it stable? "
  print (stable (ps, as) matching)
  let allmatches = allMatching (ps, as)
  let stablematches = filter (stable (ps, as)) allmatches
  
  -- putStrLn "How many free-style Da algorithms are there?"
  -- print (length results)
  -- putStrLn "Does all the outcomes equal?"
  -- print (all ( == result) results)
  

-- print (stable (ms,ws) result)
  putStrLn "The number of all possible matchings are :"
  print $ length allmatches
-- putStrLn "The number of all possible stable matchings are :"
-- print $ length stablematches
  putStrLn "Is DA outcome proposer-optimal stable matching :"
  print $ proposerOptimal (ps, as) matching (S.fromList stablematches)
-- putStrLn "Women proposing DA produce the following matching:"
-- print wpDA
-- putStrLn "Is it stable? "
-- print (stable wpDA)
