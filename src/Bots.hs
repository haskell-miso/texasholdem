-----------------------------------------------------------------------------
-- | Bot decision making: Chen-formula preflop ranges, made-hand and
-- draw strength postflop, pot odds, and per-seat temperament — with a
-- little noise so no two table villains play alike.
-----------------------------------------------------------------------------
module Bots (botMove) where
-----------------------------------------------------------------------------
import           Data.List (nub, sort)
import           Data.Maybe (fromMaybe)
-----------------------------------------------------------------------------
import           Cards
import           Eval
import           Model
import           Poker (Legal(..), legalFor)
-----------------------------------------------------------------------------
-- | Decide a move for the bot in seat @i@. The supply provides a few
-- uniform [0,1) doubles for noise and bluff rolls.
botMove :: [Double] -> Model -> Int -> Move
botMove supply m i
  | _street m == Preflop && null (_board m) = preflop rs m i
  | otherwise = postflop rs m i
  where
    rs = supply ++ repeat 0.5 -- total safety: never run dry
-----------------------------------------------------------------------------
roll :: [Double] -> Int -> Double
roll rs k = rs !! k
-----------------------------------------------------------------------------
-- | Round chip amounts to something a human would slide forward.
roundChips :: Int -> Int
roundChips n = max 5 (5 * (n `div` 5))
-----------------------------------------------------------------------------
-- * Preflop
-----------------------------------------------------------------------------
-- | Chen formula score for a starting hand (roughly -1 .. 20).
chen :: [Card] -> Double
chen [a, b] =
  let hi = max (rankVal (rank a)) (rankVal (rank b))
      lo = min (rankVal (rank a)) (rankVal (rank b))
      base = case hi of
        14 -> 10; 13 -> 8; 12 -> 7; 11 -> 6
        v -> fromIntegral v / 2
      pair = rank a == rank b
      suited = suit a == suit b
      gap = hi - lo - 1
      gapPenalty = case gap of
        0 -> 0; 1 -> 1; 2 -> 2; 3 -> 4; _ -> 5
      straightBonus = if not pair && gap <= 1 && hi < 12 then 1 else 0
  in if pair
       then max 5 (base * 2)
       else base + (if suited then 2 else 0) - gapPenalty + straightBonus
chen _ = 0
-----------------------------------------------------------------------------
preflop :: [Double] -> Model -> Int -> Move
preflop rs m i
  | owe == 0 && not raised =
      -- unopened (or completed) pot / big-blind option
      if | score >= openAt || bluffing -> MRaiseTo openTo
         | otherwise -> MCheckCall
  | not raised =
      -- limped pot, small blind to complete etc.
      if | score >= openAt -> MRaiseTo openTo
         | score >= callAt || cheap -> MCheckCall
         | otherwise -> MFold
  | otherwise =
      -- facing a raise
      if | score >= jamAt && lMayRaise lg -> MRaiseTo reraiseTo
         | score >= priceAdjusted -> MCheckCall
         | bluffing && lMayRaise lg && roll rs 2 < 0.5 -> MRaiseTo reraiseTo
         | cheap && score >= callAt - 2 -> MCheckCall
         | otherwise -> MFold
  where
    p = seatAt m i
    Style aggro loose bluff = fromMaybe (Style 0.5 0.5 0.1) (_pStyle p)
    lg = legalFor m i
    (_, bb) = blinds (_handNo m)
    owe = _currentBet m - _pBet p
    raised = _currentBet m > bb
    score = chen (_pHole p) + loose * 2.5 + (roll rs 0 - 0.5) * 2
    openAt = 8.5 - aggro * 2.5
    callAt = 5.5 - loose * 2
    jamAt = 12.5 - aggro * 1.5
    -- steeper price to continue against bigger raises
    priceAdjusted = 8.5 + fromIntegral owe / fromIntegral (max 1 bb) * 0.55
    cheap = owe * 8 <= _pStack p && owe <= 2 * bb
    bluffing = roll rs 1 < bluff * 0.07
    openTo = roundChips (bb * 5 `div` 2 + round (roll rs 2 * 2) * bb)
    reraiseTo = roundChips
      (max (lMinTo lg) (_currentBet m * 5 `div` 2 + round (roll rs 3 * 1.5) * bb))
-----------------------------------------------------------------------------
-- * Postflop
-----------------------------------------------------------------------------
postflop :: [Double] -> Model -> Int -> Move
postflop rs m i
  | owe == 0 =
      if | s > betAt || semiBluff || stoneBluff -> MRaiseTo betTo
         | otherwise -> MCheckCall
  | otherwise =
      if | s > raiseAt && lMayRaise lg -> MRaiseTo raiseTo
         | s + drawEquity > price + 0.03 - loose * 0.08 -> MCheckCall
         | semiBluff && lMayRaise lg -> MRaiseTo raiseTo
         | otherwise -> MFold
  where
    p = seatAt m i
    Style aggro loose bluff = fromMaybe (Style 0.5 0.5 0.1) (_pStyle p)
    lg = legalFor m i
    owe = _currentBet m - _pBet p
    pot = potSize m
    price = fromIntegral owe / fromIntegral (max 1 (pot + owe))
    hole = _pHole p
    boardCs = _board m
    hv = bestHand (hole ++ boardCs)
    boardTop = maximum (2 : map (rankVal . rank) boardCs)
    holePair = case hole of
      [x, y] -> rank x == rank y
      _ -> False
    s0 = case hvCat hv of
      HighCard -> 0.06
      OnePair
        | holePair && pairRank > boardTop -> 0.52 -- overpair
        | pairRank >= boardTop -> 0.48            -- top pair
        | otherwise -> 0.26
      TwoPair -> 0.62
      Trips -> 0.72
      Straight -> 0.80
      Flush -> 0.86
      FullHouse -> 0.93
      Quads -> 0.98
      StraightFlush -> 0.99
      where
        pairRank = case hvRanks hv of (r : _) -> r; _ -> 0
    s = s0 + (roll rs 0 - 0.5) * 0.08
    stillToCome = _street m < River
    flushDraw = stillToCome && hvCat hv < Flush && any
      (\su -> length (filter ((== su) . suit) (hole ++ boardCs)) == 4
           && any ((== su) . suit) hole)
      [minBound ..]
    straightDraw = stillToCome && hvCat hv < Straight && openEnded (hole ++ boardCs)
    drawEquity = (if flushDraw then 0.18 else 0)
               + (if straightDraw then 0.13 else 0)
    semiBluff = (flushDraw || straightDraw) && roll rs 1 < aggro * 0.45
    stoneBluff = roll rs 1 < bluff * 0.13 && _street m >= Turn
    betAt = 0.55 - aggro * 0.12
    raiseAt = 0.78 - aggro * 0.08
    betTo = roundChips (max (lMinTo lg)
      (pot * 2 `div` 3 + round (roll rs 2 * fromIntegral pot * 0.2)))
    raiseTo = roundChips (max (lMinTo lg) (_currentBet m * 5 `div` 2))
-----------------------------------------------------------------------------
-- | Four consecutive distinct rank values using at least one hole card
-- (a crude open-ender detector; good enough for a table villain).
openEnded :: [Card] -> Bool
openEnded cs = any run4 (windows vals)
  where
    vals = nub (sort (map (rankVal . rank) cs))
    windows xs = [ take 4 (drop k xs) | k <- [0 .. length xs - 4] ]
    run4 [a, b, c, d] = d - a == 3 && b - a == 1 && c - b == 1
    run4 _ = False
