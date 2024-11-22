{-# LANGUAGE Strict #-}
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.Set as S
import System.IO
import System.Random
import Data.Function (on)

newtype Pid = Pid {pi :: Int} deriving (Eq, Show, Ord)
newtype Aid = Aid {ai :: Int} deriving (Eq, Show, Ord)
type Preference = [Aid]
type Priority = [Pid]
data Proposer = P {pid :: !Pid, pref :: !Preference}
  deriving (Eq, Show, Ord)
data Accepter = A
  { aid :: !Aid,
    prio :: !Priority,
    quota :: !Int,
    enrol :: !(S.Set Pid)
  }
  deriving (Eq, Show, Ord)
type Matching = S.Set (Aid, S.Set Pid)
type Market = (S.Set Proposer, S.Set Accepter)
type Action = Either Pid Aid

sortByPrio ::Priority -> [Pid] -> [Pid]
sortByPrio prio proposers=
  sortBy
    (compare `on` flip elemIndex prio)
    (proposers `intersect` prio)
    
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
      Just accepter =
        find
          (\y -> head (pref proposer) == aid y)
          as
      newacceptor = accepter {enrol = S.insert p (enrol accepter)}
   in ( S.insert newproposer (S.delete proposer ps),
        S.insert newacceptor (S.delete accepter as))
dispatch (ps, as) (Right a) =
  let Just rejecter = find (\y -> a == aid y) as
      newrejecter =
        rejecter
          { enrol =
              S.fromList $
                take
                  (quota rejecter)                 
                      (sortByPrio (prio rejecter) (S.toList (enrol rejecter)))
                  
          }
   in (ps, S.insert newrejecter (S.delete rejecter as))
      
deferAcceptMonad :: Market -> [Market]
deferAcceptMonad m =
    let actions = S.toList (hasactions m)
     in if not (null actions)
            then do
                action <- actions
                deferAcceptMonad (dispatch m action)
            else return m
                 
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
    toPids Nothing = S.empty
    toPids (Just a) = a
    block m w matching =
      pid m `elem` prio w
        && pid m `elem` take (quota w) (sortByPrio (prio w) (pid m : S.toList (toPids (lookup (aid w) (S.toList matching)))))

getMatching :: Market -> Matching
getMatching (_, ws) = S.map (\w -> (aid w, enrol w)) ws

randomPref :: (RandomGen p) => Int -> Int -> p -> [[Int]]
randomPref m w gen =
  let total = take (m * w) (randomRs (1, w) gen)
   in helper w total
  where
    helper w ws =
      let (mpref, remaining) = splitAt w ws
       in nub mpref : helper w remaining

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
  let (ps, as) = (S.fromList ms, S.fromList ws)
  let results = deferAcceptMonad (ps, as)
  let result = head results
  putStrLn "How many free-style DA algorithms are there?"
  print (length results)
  putStrLn "Are the matching outcomes for them identical?"
  print (all ( == result) results)
  let matching = getMatching result
  putStrLn "DA produce the following matching:"
  print matching
  putStrLn "Is it stable? "
  print (stable (ps, as) matching)

