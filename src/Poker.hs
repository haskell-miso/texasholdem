-----------------------------------------------------------------------------
-- | The no-limit hold 'em engine, pure and testable: dealing, blinds,
-- betting (min-raise and under-raise all-in rules), street advancement,
-- side pots, and showdown.
--
-- Conventions: seats are indices into '_players'; a seat is /live/ while
-- it still contests the pot (in the hand, not folded), and /can act/
-- while it is live with chips behind.
-----------------------------------------------------------------------------
module Poker where
-----------------------------------------------------------------------------
import           Data.List (nub, sort)
import           Data.Maybe (fromMaybe, listToMaybe)
-----------------------------------------------------------------------------
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
import           Cards
import           Eval
import           Model
-----------------------------------------------------------------------------
live :: Player -> Bool
live p = not (_pOut p) && not (_pFolded p) && not (null (_pHole p))
-----------------------------------------------------------------------------
canAct :: Player -> Bool
canAct p = live p && not (_pAllIn p)
-----------------------------------------------------------------------------
-- | Seats in clockwise order starting after @i@ (one full lap).
after :: Int -> [Int]
after i = [ (i + k) `mod` seats | k <- [1 .. seats] ]
-----------------------------------------------------------------------------
-- | First seat after @i@ whose player satisfies the predicate.
nextWhere :: Model -> (Player -> Bool) -> Int -> Maybe Int
nextWhere m f i = listToMaybe [ j | j <- after i, f (seatAt m j) ]
-----------------------------------------------------------------------------
-- | Seats owing a decision: can act, and either short of the current
-- bet or yet to act since the last full raise.
pending :: Model -> Int -> Maybe Int
pending m i = nextWhere m owes i
  where
    owes p = canAct p && (_pBet p < _currentBet m || not (_pActed p))
-----------------------------------------------------------------------------
-- * Dealing
-----------------------------------------------------------------------------
-- | Start a hand: rotate the button, shuffle, deal, post blinds. If the
-- blinds already put everyone all-in there is no action: run it out.
dealHand :: [Double] -> Model -> Model
dealHand supply m0 =
  let m = postBlinds (m2 { _players = dealt })
  in if null (_toAct m) then endStreet m else m
  where
    m1 = m0
      { _players = map resetP (_players m0)
      , _board = []
      , _street = Preflop
      , _awards = []
      , _handNo = _handNo m0 + 1
      , _phase = Playing
      , _raiseOpen = False
      }
    resetP p = p
      { _pBet = 0, _pTotal = 0, _pHole = [], _pFolded = False
      , _pAllIn = False, _pActed = False, _pRevealed = False
      , _pLastAct = Nothing, _pWon = 0
      }
    btn = fromMaybe (_button m1) (nextWhere m1 (not . _pOut) (_button m1))
    m2 = m1 { _button = btn, _deck = shuffleWith supply fullDeck }
    inHand = [ j | j <- [0 .. seats - 1], not (_pOut (seatAt m2 j)) ]
    n = length inHand
    dealt =
      [ case lookup j (zip inHand [0 ..]) of
          Just k -> p { _pHole = [ _deck m2 !! k, _deck m2 !! (k + n) ] }
          Nothing -> p
      | (j, p) <- zip [0 ..] (_players m2)
      ]
-----------------------------------------------------------------------------
postBlinds :: Model -> Model
postBlinds m = m3
  { _currentBet = bb
  , _minRaise = bb
  , _deck = drop (2 * count) (_deck m)
  , _toAct = pending m3 bbSeat
  }
  where
    (sb, bb) = blinds (_handNo m)
    count = length [ () | p <- _players m, not (_pOut p) ]
    headsUp = count == 2
    btn = _button m
    sbSeat
      | headsUp = btn
      | otherwise = orBtn (nextWhere m (not . _pOut) btn)
    bbSeat = orBtn (nextWhere m (not . _pOut) sbSeat)
    orBtn = fromMaybe btn
    m3 = post bbSeat bb (post sbSeat sb m)
    post j amt mm =
      let p = seatAt mm j
          pay = min amt (_pStack p)
      in updateSeat j
           (\q -> q
             { _pStack = _pStack q - pay
             , _pBet = _pBet q + pay
             , _pTotal = _pTotal q + pay
             , _pAllIn = _pStack q - pay == 0
             })
           mm
-----------------------------------------------------------------------------
-- * Legal moves
-----------------------------------------------------------------------------
data Legal = Legal
  { lCheck :: Bool  -- ^ nothing owed
  , lCall :: Int    -- ^ chips to put in on a call (stack-capped)
  , lMayRaise :: Bool
  , lMinTo :: Int   -- ^ smallest legal raise-to
  , lMaxTo :: Int   -- ^ all-in raise-to
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
legalFor :: Model -> Int -> Legal
legalFor m i = Legal
  { lCheck = owe == 0
  , lCall = min owe (_pStack p)
  , lMayRaise = not (_pActed p) && maxTo > _currentBet m
  , lMinTo = min (_currentBet m + _minRaise m) maxTo
  , lMaxTo = maxTo
  }
  where
    p = seatAt m i
    owe = _currentBet m - _pBet p
    maxTo = _pBet p + _pStack p
-----------------------------------------------------------------------------
-- * Applying a move
-----------------------------------------------------------------------------
-- | Apply a (normalized, clamped) move for the seat and advance the
-- game: next actor, next street, runout, or showdown.
applyMove :: Int -> Move -> Model -> Model
applyMove i mv m = advanceFrom i (act mv)
  where
    lg = legalFor m i
    act MFold
      | lCheck lg = act MCheckCall -- never fold when checking is free
      | otherwise =
          finishTurn i (updateSeat i
            (\p -> p { _pFolded = True, _pLastAct = Just "fold" }) m)
    act MCheckCall =
      let pay = lCall lg
          label = if pay == 0 then "check" else "call " <> chips pay
      in finishTurn i (payIn i pay label m)
    act (MRaiseTo x)
      | not (lMayRaise lg) = act MCheckCall
      | otherwise =
          let to = if x >= lMaxTo lg then lMaxTo lg
                   else max (lMinTo lg) (min x (lMaxTo lg))
              p = seatAt m i
              pay = to - _pBet p
              fullRaise = to - _currentBet m >= _minRaise m
              opening = _currentBet m == 0
              label
                | to == lMaxTo lg = "all-in " <> chips to
                | opening = "bet " <> chips to
                | otherwise = "raise " <> chips to
              m1 = payIn i pay label m
              m2 = m1
                { _currentBet = max to (_currentBet m1)
                , _minRaise =
                    if fullRaise then to - _currentBet m else _minRaise m1
                }
              -- a full raise reopens the action for everyone else
              m3 = if fullRaise
                then m2 { _players =
                  [ if j /= i && canAct p' then p' { _pActed = False } else p'
                  | (j, p') <- zip [0 ..] (_players m2)
                  ] }
                else m2
          in finishTurn i m3
-----------------------------------------------------------------------------
chips :: Int -> MisoString
chips = ms
-----------------------------------------------------------------------------
payIn :: Int -> Int -> MisoString -> Model -> Model
payIn i pay label = updateSeat i $ \p -> p
  { _pStack = _pStack p - pay
  , _pBet = _pBet p + pay
  , _pTotal = _pTotal p + pay
  , _pAllIn = _pStack p - pay == 0
  , _pLastAct = Just (if _pStack p - pay == 0 && pay > 0
      then "all-in" else label)
  }
-----------------------------------------------------------------------------
finishTurn :: Int -> Model -> Model
finishTurn i = updateSeat i (\p -> p { _pActed = True })
-----------------------------------------------------------------------------
-- | After seat @i@ acted: hand over, next actor, or next street.
advanceFrom :: Int -> Model -> Model
advanceFrom i m
  | length liveSeats <= 1 = awardUncontested m
  | otherwise = case pending m i of
      Just j -> m { _toAct = Just j }
      Nothing -> endStreet m
  where
    liveSeats = [ j | j <- [0 .. seats - 1], live (seatAt m j) ]
-----------------------------------------------------------------------------
endStreet :: Model -> Model
endStreet m
  | _street m == River = showdown m
  | actors <= 1 = showdown (runout m)
  | otherwise =
      let m1 = clearBets m
          m2 = dealBoard (succ (_street m)) m1
      in m2 { _toAct = pending m2 (_button m2) }
  where
    actors = length [ () | p <- _players m, canAct p ]
-----------------------------------------------------------------------------
clearBets :: Model -> Model
clearBets m = m
  { _players =
      [ p { _pBet = 0, _pActed = False, _pLastAct = Nothing }
      | p <- _players m
      ]
  , _currentBet = 0
  , _minRaise = snd (blinds (_handNo m))
  }
-----------------------------------------------------------------------------
dealBoard :: Street -> Model -> Model
dealBoard st m = m
  { _street = st
  , _board = _board m ++ take n (_deck m)
  , _deck = drop n (_deck m)
  }
  where
    n = case st of Flop -> 3; Preflop -> 0; _ -> 1
-----------------------------------------------------------------------------
-- | Deal every remaining board card (all-in runout).
runout :: Model -> Model
runout m0 = foldl (flip dealBoard) (clearBets m0)
  [ st | st <- [Flop, Turn, River], st > _street m0 ]
-----------------------------------------------------------------------------
-- * Awarding
-----------------------------------------------------------------------------
-- | Everyone folded to one player: no showdown, cards stay hidden.
awardUncontested :: Model -> Model
awardUncontested m = case [ j | j <- [0 .. seats - 1], live (seatAt m j) ] of
  [w] ->
    let amt = potSize m
    in (creditWinners [(w, amt)] (zeroTotals (clearBets m)))
      { _phase = HandOver
      , _toAct = Nothing
      , _awards = [ Award w (_pName (seatAt m w)) amt Nothing ]
      }
  _ -> m
-----------------------------------------------------------------------------
-- | Showdown: reveal, evaluate, split every pot (with side pots).
showdown :: Model -> Model
showdown m0 =
  (creditWinners (concatMap snd pots) (zeroTotals m2))
    { _phase = HandOver
    , _toAct = Nothing
    , _awards = combine (concat
        [ [ (w, amt, Just (handFor w)) | (w, amt) <- ws ]
        | (_, ws) <- pots
        ])
    }
  where
    m1 = clearBets m0
    m2 = m1
      { _players =
          [ if live p then p { _pRevealed = True } else p
          | p <- _players m1
          ]
      }
    liveSeats = [ j | j <- [0 .. seats - 1], live (seatAt m2 j) ]
    handFor j = bestHand (_pHole (seatAt m2 j) ++ _board m2)
    pots =
      [ (amt, splitPot m2 amt elig)
      | (amt, elig) <- sidePots m2, amt > 0
      ]
    combine ws = -- one banner line per winning seat
      [ Award j (_pName (seatAt m2 j)) total hv
      | j <- liveSeats
      , let mine = [ (a, h) | (w, a, h) <- ws, w == j ]
      , not (null mine)
      , let total = sum (map fst mine)
      , let hv = case mine of ((_, h) : _) -> h; [] -> Nothing
      ]
-----------------------------------------------------------------------------
-- | Layered side pots from total contributions: for each distinct
-- contribution level, the slice everyone put in at that layer, and the
-- live seats eligible to win it.
sidePots :: Model -> [(Int, [Int])]
sidePots m =
  [ (slice lo hi, eligible hi)
  | (lo, hi) <- zip (0 : levels) levels
  ]
  where
    totals = [ _pTotal p | p <- _players m ]
    levels = nub (sort [ t | t <- totals, t > 0 ])
    slice lo hi = sum [ max 0 (min t hi - lo) | t <- totals ]
    eligible hi =
      [ j | j <- [0 .. seats - 1]
      , live (seatAt m j), _pTotal (seatAt m j) >= hi
      ]
-----------------------------------------------------------------------------
-- | Split @amt@ among the best hands in @elig@; odd chips go to the
-- first winner left of the button.
splitPot :: Model -> Int -> [Int] -> [(Int, Int)]
splitPot m amt elig
  | null elig = []
  | otherwise =
      let best = maximum [ handFor j | j <- elig ]
          winners =
            [ j | j <- after (_button m), j `elem` elig, handFor j == best ]
          n = length winners
          (q, r) = amt `divMod` n
      in [ (w, q + (if k < r then 1 else 0)) | (k, w) <- zip [0 ..] winners ]
  where
    handFor j = bestHand (_pHole (seatAt m j) ++ _board m)
-----------------------------------------------------------------------------
-- | The pot has been pushed: contributions live in stacks now.
zeroTotals :: Model -> Model
zeroTotals m = m { _players = [ p { _pTotal = 0 } | p <- _players m ] }
-----------------------------------------------------------------------------
creditWinners :: [(Int, Int)] -> Model -> Model
creditWinners ws m = foldl credit m ws
  where
    credit mm (j, amt) = updateSeat j
      (\p -> p { _pStack = _pStack p + amt, _pWon = _pWon p + amt }) mm
-----------------------------------------------------------------------------
-- * Between hands
-----------------------------------------------------------------------------
-- | Mark busted players out; decide whether the game continues.
-- Returns the updated model and what happens next.
data NextStep = Continue | HeroBusted | HeroChampion
  deriving (Eq, Show)
-----------------------------------------------------------------------------
settle :: Model -> (Model, NextStep)
settle m
  | _pOut (seatAt m' heroSeat) = (m', HeroBusted)
  | alive <= 1 = (m', HeroChampion)
  | otherwise = (m', Continue)
  where
    m' = m { _players =
      [ if _pStack p == 0 then p { _pOut = True } else p
      | p <- _players m
      ] }
    alive = length [ () | p <- _players m', not (_pOut p) ]
