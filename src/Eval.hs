-----------------------------------------------------------------------------
-- | Poker hand evaluation: the best five-card hand from five, six, or
-- seven cards, with full kicker tie-breaking, plus the pretty name for
-- the winner banner. 'HandValue's compare exactly like poker hands.
-----------------------------------------------------------------------------
module Eval where
-----------------------------------------------------------------------------
import           Data.List (group, nub, sortBy, sortOn)
import           Data.Maybe (listToMaybe)
import           Data.Ord (comparing, Down(..))
-----------------------------------------------------------------------------
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Cards
-----------------------------------------------------------------------------
data Category
  = HighCard | OnePair | TwoPair | Trips | Straight
  | Flush | FullHouse | Quads | StraightFlush
  deriving (Eq, Ord, Show, Enum, Bounded)
-----------------------------------------------------------------------------
-- | Category plus tie-break ranks, most significant first. Derived 'Ord'
-- is exactly poker hand comparison; 'hvCards' are the five cards used
-- (so the view can spotlight them at showdown) and never affect order.
data HandValue = HandValue
  { hvCat :: Category
  , hvRanks :: [Int]
  , hvCards :: [Card]
  } deriving (Show)
-----------------------------------------------------------------------------
instance Eq HandValue where
  a == b = (hvCat a, hvRanks a) == (hvCat b, hvRanks b)
instance Ord HandValue where
  compare = comparing (\h -> (hvCat h, hvRanks h))
-----------------------------------------------------------------------------
-- | Best five-card hand from at least five cards.
bestHand :: [Card] -> HandValue
bestHand cs = case straightFlush cs of
  Just hv -> hv
  Nothing ->
    let groups = rankGroups cs
    in case groups of
      (4, r) : _ -> quadsValue cs r
      (3, r) : (n, r') : _ | n >= 2 -> fullHouseValue cs r r'
      _ -> case flushValue cs of
        Just hv -> hv
        Nothing -> case straightValue cs of
          Just hv -> hv
          Nothing -> case groups of
            (3, r) : _ -> tripsValue cs r
            (2, hi) : (2, lo) : _ -> twoPairValue cs hi lo
            (2, r) : _ -> onePairValue cs r
            _ -> highCardValue cs
-----------------------------------------------------------------------------
-- | Rank groups of a hand, sorted by (count, rank) descending:
-- AAKKQ -> [(2,Ace),(2,King),(1,Queen)].
rankGroups :: [Card] -> [(Int, Rank)]
rankGroups cs = sortOn Down
  [ (length g, r)
  | g@(r : _) <- group (sortBy (comparing Down) (map rank cs))
  ]
-----------------------------------------------------------------------------
-- | High card of a straight within the given ranks (values, unique),
-- honouring the wheel (A-2-3-4-5, high card Five).
straightHighFrom :: [Int] -> Maybe Int
straightHighFrom vals = listToMaybe
  [ h | h <- [14, 13 .. 5], all (`elem` wheeled) [h - 4 .. h] ]
  where
    uniq = nub vals
    wheeled = if 14 `elem` uniq then 1 : uniq else uniq
-----------------------------------------------------------------------------
-- | The five cards of a straight with the given high value, drawn from
-- @cs@ (one card per rank; Ace serves as 1 in the wheel).
straightCards :: Int -> [Card] -> [Card]
straightCards h cs =
  [ c
  | v <- [h, h - 1 .. h - 4]
  , Just c <- [ listToMaybe [ c' | c' <- cs, valOf c' == v ] ]
  ]
  where
    valOf c
      | h == 5 && rank c == Ace = 1
      | otherwise = rankVal (rank c)
-----------------------------------------------------------------------------
straightValue :: [Card] -> Maybe HandValue
straightValue cs = do
  h <- straightHighFrom (map (rankVal . rank) cs)
  pure (HandValue Straight [h] (straightCards h cs))
-----------------------------------------------------------------------------
flushSuit :: [Card] -> Maybe Suit
flushSuit cs = listToMaybe
  [ s | s <- [minBound ..], length (ofSuit s cs) >= 5 ]
-----------------------------------------------------------------------------
ofSuit :: Suit -> [Card] -> [Card]
ofSuit s = filter ((== s) . suit)
-----------------------------------------------------------------------------
flushValue :: [Card] -> Maybe HandValue
flushValue cs = do
  s <- flushSuit cs
  let top5 = take 5 (sortBy (comparing (Down . rank)) (ofSuit s cs))
  pure (HandValue Flush (map (rankVal . rank) top5) top5)
-----------------------------------------------------------------------------
straightFlush :: [Card] -> Maybe HandValue
straightFlush cs = do
  s <- flushSuit cs
  let suited = ofSuit s cs
  h <- straightHighFrom (map (rankVal . rank) suited)
  pure (HandValue StraightFlush [h] (straightCards h suited))
-----------------------------------------------------------------------------
-- | The n highest cards outside the given ranks (kickers).
kickers :: Int -> [Rank] -> [Card] -> [Card]
kickers n used cs = take n
  (sortBy (comparing (Down . rank)) [ c | c <- cs, rank c `notElem` used ])
-----------------------------------------------------------------------------
ofRank :: Rank -> [Card] -> [Card]
ofRank r = filter ((== r) . rank)
-----------------------------------------------------------------------------
quadsValue :: [Card] -> Rank -> HandValue
quadsValue cs r = HandValue Quads (rankVal r : map (rankVal . rank) ks) five
  where
    ks = kickers 1 [r] cs
    five = ofRank r cs ++ ks
-----------------------------------------------------------------------------
fullHouseValue :: [Card] -> Rank -> Rank -> HandValue
fullHouseValue cs r r' =
  HandValue FullHouse [rankVal r, rankVal r'] five
  where
    five = take 3 (ofRank r cs) ++ take 2 (ofRank r' cs)
-----------------------------------------------------------------------------
tripsValue :: [Card] -> Rank -> HandValue
tripsValue cs r = HandValue Trips (rankVal r : map (rankVal . rank) ks) five
  where
    ks = kickers 2 [r] cs
    five = take 3 (ofRank r cs) ++ ks
-----------------------------------------------------------------------------
twoPairValue :: [Card] -> Rank -> Rank -> HandValue
twoPairValue cs hi lo =
  HandValue TwoPair (rankVal hi : rankVal lo : map (rankVal . rank) ks) five
  where
    ks = kickers 1 [hi, lo] cs
    five = take 2 (ofRank hi cs) ++ take 2 (ofRank lo cs) ++ ks
-----------------------------------------------------------------------------
onePairValue :: [Card] -> Rank -> HandValue
onePairValue cs r = HandValue OnePair (rankVal r : map (rankVal . rank) ks) five
  where
    ks = kickers 3 [r] cs
    five = take 2 (ofRank r cs) ++ ks
-----------------------------------------------------------------------------
highCardValue :: [Card] -> HandValue
highCardValue cs = HandValue HighCard (map (rankVal . rank) ks) ks
  where
    ks = kickers 5 [] cs
-----------------------------------------------------------------------------
-- * Names
-----------------------------------------------------------------------------
valRank :: Int -> Rank
valRank v = toEnum (v - 2)
-----------------------------------------------------------------------------
-- | Banner-worthy hand name: "Full House, Kings full of Fours".
handName :: HandValue -> MisoString
handName (HandValue cat rs _) = case (cat, rs) of
  (StraightFlush, 14 : _) -> "Royal Flush"
  (StraightFlush, h : _) -> "Straight Flush, " <> rankName (valRank h) <> " high"
  (Quads, q : _) -> "Four of a Kind, " <> rankPlural (valRank q)
  (FullHouse, t : p : _) ->
    "Full House, " <> rankPlural (valRank t) <> " full of " <> rankPlural (valRank p)
  (Flush, h : _) -> "Flush, " <> rankName (valRank h) <> " high"
  (Straight, h : _) -> "Straight, " <> rankName (valRank h) <> " high"
  (Trips, t : _) -> "Three of a Kind, " <> rankPlural (valRank t)
  (TwoPair, hi : lo : _) ->
    "Two Pair, " <> rankPlural (valRank hi) <> " and " <> rankPlural (valRank lo)
  (OnePair, p : _) -> "Pair of " <> rankPlural (valRank p)
  (HighCard, h : _) -> rankName (valRank h) <> " High"
  _ -> "Unknown"
