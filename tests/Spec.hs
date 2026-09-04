-----------------------------------------------------------------------------
-- | Native correctness tests: the hand evaluator against known hands,
-- the betting engine's rules (blinds, min-raise, under-raise all-ins,
-- heads-up order), side pots, and a bot-vs-bot tournament fuzz that
-- checks chip conservation on every single step.
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Monad (unless, forM_)
import           Data.IORef
import           Data.List (nub)
import           System.Exit (exitFailure)
-----------------------------------------------------------------------------
import           Bots
import           Cards
import           Eval
import           Model
import           Poker
-----------------------------------------------------------------------------
-- tiny deterministic uniform supply
lcg :: Int -> [Double]
lcg s0 = go (abs (s0 * 2654435761 + 1) `mod` m0)
  where
    m0 = 2147483647
    go s = let s' = (s * 48271) `mod` m0
           in fromIntegral s' / fromIntegral m0 : go s'
-----------------------------------------------------------------------------
-- shorthand card builder: c Ace Spades
c :: Rank -> Suit -> Card
c = Card
-----------------------------------------------------------------------------
main :: IO ()
main = do
  failures <- newIORef (0 :: Int)
  let check name ok = do
        putStrLn ((if ok then "  ok  " else " FAIL ") <> name)
        unless ok (modifyIORef failures (+ 1))

  putStrLn "-- evaluator ------------------------------------------------"
  let hv = bestHand
  check "royal flush found among seven cards"
    (hvCat (hv [ c Ace Spades, c King Spades, c Queen Spades, c Jack Spades
               , c Ten Spades, c Two Hearts, c Nine Clubs ]) == StraightFlush)
  check "wheel straight is five high"
    (hvRanks (hv [ c Ace Clubs, c Two Hearts, c Three Spades, c Four Diamonds
                 , c Five Clubs, c King Hearts, c Nine Spades ]) == [5])
  check "six-high straight beats the wheel"
    (hv [ c Two Clubs, c Three Hearts, c Four Spades, c Five Diamonds
        , c Six Clubs ]
      > hv [ c Ace Clubs, c Two Hearts, c Three Spades, c Four Diamonds
           , c Five Clubs ])
  check "flush and straight together is not a straight flush"
    (hvCat (hv [ c Ace Hearts, c King Hearts, c Nine Hearts, c Four Hearts
               , c Two Hearts, c Queen Spades, c Jack Clubs ]) == Flush)
  check "quads pick the best kicker"
    (hvRanks (hv [ c Nine Clubs, c Nine Diamonds, c Nine Hearts, c Nine Spades
                 , c Ace Clubs, c King Hearts, c Two Spades ]) == [9, 14])
  check "two trips make a full house, bigger trips on top"
    (hvRanks (hv [ c Queen Clubs, c Queen Diamonds, c Queen Hearts
                 , c Seven Clubs, c Seven Diamonds, c Seven Hearts
                 , c Ace Spades ]) == [12, 7])
  check "three pairs collapse to two pair with the right kicker"
    (hvRanks (hv [ c Ace Clubs, c Ace Diamonds, c King Clubs, c King Diamonds
                 , c Queen Clubs, c Queen Diamonds, c Jack Hearts ])
      == [14, 13, 12])
  check "kickers break high-card ties"
    (hv [ c Ace Clubs, c King Hearts, c Queen Spades, c Jack Diamonds
        , c Nine Clubs ]
      > hv [ c Ace Diamonds, c King Spades, c Queen Hearts, c Jack Clubs
           , c Eight Diamonds ])
  check "identical values split (Eq on HandValue)"
    (hv [ c Ace Clubs, c King Hearts, c Queen Spades, c Jack Diamonds
        , c Nine Clubs ]
      == hv [ c Ace Diamonds, c King Spades, c Queen Hearts, c Jack Clubs
            , c Nine Diamonds ])
  check "pair beats high card, two pair beats pair"
    (Trips > TwoPair && TwoPair > OnePair && OnePair > HighCard)
  check "hand names read right"
    ( handName (hv [ c King Clubs, c King Diamonds, c King Hearts
                   , c Four Clubs, c Four Diamonds ])
        == "Full House, Kings full of Fours"
    && handName (hv [ c Ace Spades, c King Spades, c Queen Spades
                    , c Jack Spades, c Ten Spades ]) == "Royal Flush" )
  check "best five cards are reported (spotlight)"
    (length (nub (hvCards (hv (take 7 fullDeck)))) == 5)

  putStrLn "-- dealing & blinds -----------------------------------------"
  let started = dealHand (lcg 1) initialModel { _phase = Playing }
      chipsTotal m = sum [ _pStack p + _pTotal p | p <- _players m ]
  check "hand number advances, phase is Playing"
    (_handNo started == 1 && _phase started == Playing)
  check "every seat got two hole cards"
    (all ((== 2) . length . _pHole) (_players started))
  check "no card dealt twice"
    (let cs = concatMap _pHole (_players started) ++ _deck started
     in length (nub cs) == length cs)
  check "blinds are posted (sb+bb committed)"
    (potSize started == 10 + 20)
  check "chips are conserved by the deal"
    (chipsTotal started == seats * startingStack)
  check "button rotated to seat 0, so UTG is seat 3"
    (_button started == 0 && _toAct started == Just 3)
  check "big blind option: bb has not acted"
    (not (_pActed (seatAt started 2)))

  putStrLn "-- betting rules --------------------------------------------"
  let lg3 = legalFor started 3
  check "UTG owes a call of the big blind"
    (lCall lg3 == 20 && not (lCheck lg3))
  check "min raise-to is two big blinds"
    (lMinTo lg3 == 40 && lMayRaise lg3)
  let raised = applyMove 3 (MRaiseTo 60) started
  check "raise to 60 sets the price and reopens action"
    (_currentBet raised == 60 && _minRaise raised == 40
      && _toAct raised == Just 4)
  check "next min raise-to is 100"
    (lMinTo (legalFor raised 4) == 100)
  let folded = foldl (\mm j -> applyMove j MFold mm) raised [4, 5, 0, 1]
      bbCalls = applyMove 2 MCheckCall folded
  check "flop comes after the blind defends"
    (_street bbCalls == Flop && length (_board bbCalls) == 3)
  check "postflop: first live seat left of the button acts"
    (_toAct bbCalls == Just 2)
  check "chips conserved through the preflop round"
    (chipsTotal bbCalls == seats * startingStack)
  check "street bets were collected into the pot"
    (all ((== 0) . _pBet) (_players bbCalls) && potSize bbCalls == 130)

  -- under-raise all-in must not reopen the betting
  let m0 = bbCalls                          -- seat 2 vs seat 3, flop
      bet1 = applyMove 2 (MRaiseTo 100) m0  -- bet 100
      shortStack = updateSeat 3 (\p -> p { _pStack = 150 }) bet1
      jam = applyMove 3 (MRaiseTo 150) shortStack -- all-in for 150 (< min-raise 200)
  check "under-raise all-in leaves the bettor without a raise option"
    (case _toAct jam of
       Just 2 -> not (lMayRaise (legalFor jam 2)) && lCall (legalFor jam 2) == 50
       _ -> False)

  putStrLn "-- uncontested pots -----------------------------------------"
  let fold5 = foldl (\mm j -> applyMove j MFold mm)
        started [3, 4, 5, 0, 1]
  check "folding to the big blind ends the hand"
    (_phase fold5 == HandOver)
  check "big blind scoops without showing"
    (case _awards fold5 of
       [Award 2 _ 30 Nothing] -> True
       _ -> False)
  check "no cards revealed on an uncontested pot"
    (all (not . _pRevealed) (_players fold5))
  check "chips conserved after the scoop"
    (chipsTotal fold5 == seats * startingStack)

  putStrLn "-- side pots ------------------------------------------------"
  -- three players, engineered stacks and cards
  let trio = initialModel
        { _players =
            [ p0 { _pStack = 100 }   -- shortest, will win the main pot
            , p1 { _pStack = 300 }
            , p2 { _pStack = 900 }
            , out3, out4, out5
            ]
        , _button = 2
        , _phase = Playing
        }
        where
          [p0, p1, p2, q3, q4, q5] = initialPlayers
          out3 = q3 { _pOut = True, _pStack = 0 }
          out4 = q4 { _pOut = True, _pStack = 0 }
          out5 = q5 { _pOut = True, _pStack = 0 }
      -- rig the holes AFTER dealing (pure model surgery); strip the
      -- rigged cards from the deck so the runout cannot duplicate them
      rigged =
        [ c Ace Spades, c Ace Hearts, c King Clubs, c King Diamonds
        , c Queen Clubs, c Queen Diamonds
        ]
      runFrom seed =
        let dealt3 = dealHand (lcg seed) trio
            rig = (foldr (\(j, h) mm -> updateSeat j (\p -> p { _pHole = h }) mm)
                    dealt3
                    [ (0, [ c Ace Spades, c Ace Hearts ])   -- aces
                    , (1, [ c King Clubs, c King Diamonds ]) -- kings
                    , (2, [ c Queen Clubs, c Queen Diamonds ])
                    ])
                  { _board = [], _deck = filter (`notElem` rigged) (_deck dealt3) }
            -- shove chain: everybody all-in preflop
            orJ = maybe (error "no actor") id
            allin1 = applyMove (orJ (_toAct rig)) (MRaiseTo 10000) rig
            allin2 = applyMove (orJ (_toAct allin1)) (MRaiseTo 10000) allin1
        in applyMove (orJ (_toAct allin2)) (MRaiseTo 10000) allin2
      -- a board that pairs nobody and completes nothing, so the
      -- preflop order AA > KK > QQ is what gets paid
      clean m' =
        let b = _board m'
        in hvCat (bestHand ([c Ace Spades, c Ace Hearts] ++ b)) == OnePair
        && hvCat (bestHand ([c King Clubs, c King Diamonds] ++ b)) == OnePair
        && hvCat (bestHand ([c Queen Clubs, c Queen Diamonds] ++ b)) == OnePair
      allin3 = runFrom 7
  check "all-in chain runs the board out and ends the hand"
    (_phase allin3 == HandOver && length (_board allin3) == 5)
  check "chips conserved through the all-in"
    (chipsTotal allin3 == 100 + 300 + 900)
  case [ m' | seed <- [1 .. 80], let m' = runFrom seed, clean m' ] of
    m' : _ -> do
      check "aces triple through the main pot (3 x 100)"
        (_pStack (seatAt m' 0) == 300)
      check "kings take the side pot (2 x 200)"
        (_pStack (seatAt m' 1) == 400)
      check "the big stack keeps the change"
        (_pStack (seatAt m' 2) == 600)
    [] -> check "found a clean side-pot board within 80 seeds" False

  putStrLn "-- heads-up -------------------------------------------------"
  let duo = initialModel
        { _players = [ ph, pv, o2, o3, o4, o5 ], _button = 0, _phase = Playing }
        where
          [ph, pv, q2, q3, q4, q5] = initialPlayers
          o2 = q2 { _pOut = True }; o3 = q3 { _pOut = True }
          o4 = q4 { _pOut = True }; o5 = q5 { _pOut = True }
      hu = dealHand (lcg 3) duo
      huBtn = _button hu
      huOther = if huBtn == 0 then 1 else 0
  check "heads-up: the button posts the small blind and acts first"
    (_toAct hu == Just huBtn && _pBet (seatAt hu huBtn) == 10
      && _pBet (seatAt hu huOther) == 20)
  let huFlop = applyMove huOther MCheckCall (applyMove huBtn MCheckCall hu)
  check "heads-up postflop: the big blind acts first"
    (_street huFlop == Flop && _toAct huFlop == Just huOther)

  putStrLn "-- split pots -----------------------------------------------"
  -- seats 0 and 1 tie; seat 2 folded 25 of dead money, so the 225 pot
  -- splits 113/112 with the odd chip left of the button
  let sd = showdown (foldr (\(j, h) mm -> updateSeat j (\p -> p { _pHole = h, _pTotal = 100 }) mm)
             (updateSeat 2 (\p -> p
               { _pHole = [ c Three Hearts, c Three Spades ]
               , _pFolded = True
               , _pTotal = 25
               })
               initialModel
                 { _phase = Playing, _button = 0
                 , _players = [ if j <= 2 then p else p { _pOut = True }
                              | (j, p) <- zip [0 :: Int ..] initialPlayers ]
                 , _board = [ c Two Clubs, c Seven Diamonds, c Nine Hearts
                            , c Jack Spades, c Four Clubs ]
                 })
             [ (0, [ c Ace Spades, c King Hearts ])
             , (1, [ c Ace Diamonds, c King Clubs ])
             ])
  check "equal hands split the pot (with dead money)"
    (_pWon (seatAt sd 0) + _pWon (seatAt sd 1) == 225
      && abs (_pWon (seatAt sd 0) - _pWon (seatAt sd 1)) <= 1)
  check "the odd chip goes to the first winner left of the button"
    (_pWon (seatAt sd 1) == 113)
  check "the folder wins nothing"
    (_pWon (seatAt sd 2) == 0)

  putStrLn "-- bot tournament fuzz --------------------------------------"
  -- Bots play whole tournaments; every intermediate state must conserve
  -- chips, produce only legal states, and every hand must terminate.
  forM_ [1 .. 6 :: Int] $ \seed -> do
    let tourney = playTournament (lcg (seed * 977)) 250
        (okConserve, okSteps, hands) = tourney
    check ("seed " <> show seed <> ": conservation over " <> show hands <> " hands")
      okConserve
    check ("seed " <> show seed <> ": every hand terminated") okSteps

  n <- readIORef failures
  if n == 0
    then putStrLn "\nAll tests passed."
    else do
      putStrLn ("\n" <> show n <> " test(s) failed.")
      exitFailure
-----------------------------------------------------------------------------
-- | Run a bot-only tournament; returns (chips always conserved, no hand
-- exceeded the step cap, hands played).
playTournament :: [Double] -> Int -> (Bool, Bool, Int)
playTournament supply0 maxHands = go supply0 (initialModel { _phase = Playing }) 0 True True
  where
    total = seats * startingStack
    chipsTotal m = sum [ _pStack p + _pTotal p | p <- _players m ]
    go supply m hands okC okS
      | hands >= maxHands = (okC, okS, hands)
      | otherwise =
          let (mine, rest) = splitAt 64 supply
              m1 = dealHand mine m
              (m2, usedOk) = playHand rest m1 (0 :: Int)
              okC' = okC && chipsTotal m2 == total
              (m3, next) = settle m2
          in case next of
               Continue ->
                 go (drop 64 rest) m3 (hands + 1) okC' (okS && usedOk)
               _ -> (okC' , okS && usedOk, hands + 1)
    playHand supply m steps
      | steps > 300 = (m, False)
      | otherwise = case (_phase m, _toAct m) of
          (Playing, Just j) ->
            let (mine, rest) = splitAt 6 supply
            in playHand rest (applyMove j (botMove mine m j) m) (steps + 1)
          _ -> (m, True)
