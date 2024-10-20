import Data.List ( find, delete, insert, sortBy, (\\) )
import System.Random
import System.IO
import Control.Monad
import Data.Function ( on )

sortByKeys :: (Ord a) => [a] -> [a] -> [a]
sortByKeys keys =
    sortBy
        ( compare
            `on` (\x -> if isNothing x then Just maxBound else x)
                . flip lookup (zip keys [0 :: Int ..])
        )

newtype Pid a = Pid { pi :: a} deriving(Eq,Show,Ord)
newtype Aid a = Aid { ai :: a} deriving(Eq,Show,Ord)
data Proposer = P { pid :: Pid Int, pref :: [Aid Int]}
              deriving(Eq,Show,Ord)
data Accepter = A { aid :: Aid Int, prio :: [Pid Int],
                    quota :: Int, enrol :: [Pid Int]}
              deriving(Eq,Show,Ord)
type Matching = [(Accepter,[Proposer])]
type Market = ([Proposer],[Accepter])
type Acter = Either (Pid Int) (Aid Int)

hasactions :: Market -> [Acter]
hasactions (ps,as) =
  let unenrolid = map pid ps \\ concatMap enrol as
      unenrol = filter (( `elem` unenrolid) . pid) ps
  in map (Left . pid) (filter (not . null . pref) unenrol)
     `union`
     map (Right . aid) (filter
     (\x -> length (enrol x) > quota x
            || (not . null) (enrol x \\ prio x)) as)

dispatch :: Acter -> Market -> Market
dispatch (Left p) (ps,as) =
  let Just proposer = find (\x -> p==pid x) ps
      newproposer = proposer{pref = tail (pref proposer)}
      Just acceptor = find
        (\y -> head (pref proposer) == aid y) as
      newacceptor = acceptor{enrol = p : enrol acceptor}
  in (changeItem proposer newproposer ps,
      changeItem acceptor newacceptor as)
  where
    changeItem old new list = insert new (delete old list)
dispatch (Right a) (ps,as) =
  let Just rejector = find (\y -> a == aid y) as
      newrejector =
        rejector{enrol =
         take (quota rejector)
          (takeWhile (`elem` prio rejector)
           (sortByKeys (prio rejector) (enrol rejector)))}
  in (ps, insert newrejector (delete rejector as))

deferAccepts :: Market -> [Market]
deferAccepts m = let actions = hasactions m
                 in if not (null actions)
                   then do
                      act <- actions
                      deferAccepts (dispatch act m)
                   else return m

getMatching :: Market -> Matching
getMatching (_, ws) = S.map (\w -> (aid w, enrol w)) ws