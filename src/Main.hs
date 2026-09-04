-----------------------------------------------------------------------------
-- | miso-texasholdem: six-max no-limit hold 'em in a modern casino
-- lounge, in the miso game family.
-----------------------------------------------------------------------------
module Main where
-----------------------------------------------------------------------------
import           Control.Concurrent (threadDelay)
import           Control.Monad (when, unless)
import           Data.List (nub)
import qualified Data.IntSet as IS
-----------------------------------------------------------------------------
import           Miso hiding ((!!), view, button)
import           Miso.Lens hiding (view)
import           Miso.Random (replicateRM)
import qualified Miso.CSS as CSS
import qualified Miso.Html.Element as H
import qualified Miso.Html.Event as HE
import qualified Miso.Html.Property as HP
import           Miso.String (intercalate, pack, fromMisoString)
-----------------------------------------------------------------------------
import           Bots
import           Cards
import           Eval
import           Model
import           Poker
import           Sound
import           Styles (skin)
-----------------------------------------------------------------------------
main :: IO ()
main = startApp defaultEvents app
-----------------------------------------------------------------------------
app :: App Model Action
app = (component initialModel updateModel viewModel)
  { styles = [ Sheet skin ]
  , subs = [ keyboardSub Keys ]
  }
-----------------------------------------------------------------------------
#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif
-----------------------------------------------------------------------------
type Fx = Effect () () Model Action
-----------------------------------------------------------------------------
-- * Update
-----------------------------------------------------------------------------
updateModel :: Action -> Fx
updateModel = \case
  NoOp -> pure ()

  ToggleSound -> soundOn %= not

  ShowHelp -> showHelp .= True

  CloseHelp -> showHelp .= False

  BackToTitle -> do
    m <- get
    put initialModel { _soundOn = m ^. soundOn }

  StartGame -> do
    io_ soundInit
    m <- get
    put initialModel
      { _soundOn = m ^. soundOn
      , _heldKeys = m ^. heldKeys
      , _phase = Playing
      }
    playFx "shuffle"
    scheduleDeal

  DealHand supply -> do
    m <- get
    when (m ^. phase == Playing || m ^. phase == HandOver) $ do
      put (dealHand supply m)
      playFx "deal"
      animSeq += 1
      afterStep

  HeroMove mv -> do
    m <- get
    when (m ^. phase == Playing
            && m ^. toAct == Just heroSeat
            && not (m ^. showHelp)) $
      stepMove heroSeat mv

  BotMove stamp seat supply -> do
    m <- get
    when (m ^. pacer == stamp
            && m ^. phase == Playing
            && m ^. toAct == Just seat) $
      stepMove seat (botMove supply m seat)

  NextHand -> do
    pacer += 1 -- cancels any scheduled auto-advance
    m <- get
    when (m ^. phase == HandOver) goNextHand

  NextHandAuto stamp -> do
    m <- get
    when (m ^. pacer == stamp && m ^. phase == HandOver) goNextHand

  SetRaise n -> do
    m <- get
    let lg = legalFor m heroSeat
    raiseAmt .= max (lMinTo lg) (min (lMaxTo lg) n)

  ToggleRaise -> do
    m <- get
    let lg = legalFor m heroSeat
    raiseOpen %= not
    unless (m ^. raiseOpen) $
      raiseAmt .= max (lMinTo lg) (min (lMaxTo lg) (m ^. raiseAmt))
    playFx "click"

  Keys ks -> do
    m <- get
    let fresh = IS.toList (ks IS.\\ (m ^. heldKeys))
    heldKeys .= ks
    mapM_ (issueKey m) fresh
-----------------------------------------------------------------------------
issueKey :: Model -> Int -> Fx
issueKey m k
  | m ^. showHelp = when (k == 27 || k == 72) (issue CloseHelp)
  | Title <- m ^. phase =
      if | k == 13 || k == 32 -> issue StartGame
         | k == 72 -> issue ShowHelp
         | otherwise -> pure ()
  | GameOver _ <- m ^. phase =
      when (k == 13 || k == 32) (issue StartGame)
  | HandOver <- m ^. phase =
      when (k == 13 || k == 32) (issue NextHand)
  | heroTurn =
      if | k == 70 -> issue (HeroMove MFold)                 -- F
         | k == 67 || k == 32 -> issue (HeroMove MCheckCall) -- C, space
         | k == 82 -> issue ToggleRaise                      -- R
         | k == 65 -> issue (HeroMove (MRaiseTo maxTo))      -- A: all-in
         | k == 13 && m ^. raiseOpen ->
             issue (HeroMove (MRaiseTo (m ^. raiseAmt)))     -- enter
         | k == 38 || k == 39 -> issue (SetRaise (m ^. raiseAmt + bump))
         | k == 40 || k == 37 -> issue (SetRaise (m ^. raiseAmt - bump))
         | k == 72 -> issue ShowHelp                         -- H
         | k == 27 -> raiseOpen .= False
         | otherwise -> pure ()
  | k == 72 = issue ShowHelp
  | otherwise = pure ()
  where
    heroTurn = m ^. phase == Playing && m ^. toAct == Just heroSeat
    maxTo = lMaxTo (legalFor m heroSeat)
    bump = snd (blinds (m ^. handNo))
-----------------------------------------------------------------------------
-- | Apply a move, make noise, advance the choreography.
stepMove :: Int -> Move -> Fx
stepMove seat mv = do
  m <- get
  let lg = legalFor m seat
      m' = applyMove seat mv m
      boardGrew = length (m' ^. board) > length (m ^. board)
      wentAllIn = _pAllIn (seatAt m' seat) && not (_pAllIn (seatAt m seat))
  put m'
  pacer += 1
  raiseOpen .= False
  animSeq += 1
  playFx $ if
    | wentAllIn -> "allin"
    | MFold <- mv, not (lCheck lg) -> "fold"
    | MRaiseTo _ <- mv, lMayRaise lg -> "chips"
    | lCheck lg -> "check"
    | otherwise -> "chip"
  when boardGrew (playFx "flip")
  afterStep
-----------------------------------------------------------------------------
-- | After any state advance: hand the turn to a bot, prime the hero's
-- slider, or celebrate and queue the next hand.
afterStep :: Fx
afterStep = do
  m <- get
  let stamp = m ^. pacer
  case m ^. phase of
    Playing -> case m ^. toAct of
      Just j
        | j == heroSeat -> do
            let lg = legalFor m heroSeat
            raiseAmt .= defaultRaise m lg
        | otherwise -> io $ do
            supply <- replicateRM 9
            let jitter = case supply of (r : _) -> r; _ -> 0.5
            threadDelay (700000 + floor (jitter * 1000000))
            pure (BotMove stamp j (drop 1 supply))
      Nothing -> pure ()
    HandOver -> do
      let heroWon = sum [ awAmount a | a <- m ^. awards, awSeat a == heroSeat ]
          heroShowed = _pRevealed (seatAt m heroSeat)
      when (heroWon > 0) (biggestPot %= max heroWon)
      playFx $ if
        | heroWon > 0 -> "win"
        | heroShowed -> "lose"
        | otherwise -> "chips"
      io $ do
        threadDelay 4600000
        pure (NextHandAuto stamp)
    _ -> pure ()
-----------------------------------------------------------------------------
-- | A friendly opening slider position: a pot-sized raise, clamped.
defaultRaise :: Model -> Legal -> Int
defaultRaise m lg =
  max (lMinTo lg) (min (lMaxTo lg) (potSize m + m ^. currentBet))
-----------------------------------------------------------------------------
goNextHand :: Fx
goNextHand = do
  m <- get
  let (m', next) = settle m
  case next of
    Continue -> do
      put m'
      playFx "shuffle"
      scheduleDeal
    HeroBusted -> do
      put m' { _phase = GameOver False }
      playFx "lose"
    HeroChampion -> do
      put m' { _phase = GameOver True }
      playFx "win"
-----------------------------------------------------------------------------
scheduleDeal :: Fx
scheduleDeal = io $ do
  threadDelay 500000
  supply <- replicateRM 52
  pure (DealHand supply)
-----------------------------------------------------------------------------
playFx :: MisoString -> Fx
playFx name = do
  m <- get
  io_ (playSound (m ^. soundOn) name)
-----------------------------------------------------------------------------
-- * View
-----------------------------------------------------------------------------
viewModel :: () -> () -> Model -> View () Model Action
viewModel _ _ m = case m ^. phase of
  Title -> H.div_ []
    ( titleView : [ helpOverlay | m ^. showHelp ] )
  _ -> H.div_ []
    ( [ topbar m
      , H.div_ [ HP.class_ "room" ] [ tableView m ]
      ]
      ++ [ actionBar m | heroTurn ]
      ++ [ bannerView m | m ^. phase == HandOver ]
      ++ [ gameOverView m won | GameOver won <- [ m ^. phase ] ]
      ++ [ helpOverlay | m ^. showHelp ]
    )
  where
    heroTurn =
      m ^. phase == Playing && m ^. toAct == Just heroSeat
-----------------------------------------------------------------------------
fmtChips :: Int -> MisoString
fmtChips n
  | n < 0 = "-" <> fmtChips (negate n)
  | n < 1000 = ms n
  | otherwise = intercalate "," (map pack (groups (show n)))
  where
    groups s =
      let r = length s `mod` 3
          h = if r == 0 then [] else [take r s]
      in h ++ chunk (drop r s)
    chunk [] = []
    chunk s = take 3 s : chunk (drop 3 s)
-----------------------------------------------------------------------------
titleView :: View () Model Action
titleView = H.div_ [ HP.class_ "titleWrap" ] $
  [ deco f red x y r dl
  | (f, red, x, y, r, dl) <-
      [ ("A♠", False, "7%",  "12%", "-11deg", "0s")
      , ("K♥", True,  "89%", "10%", "8deg",   ".8s")
      , ("Q♦", True,  "10%", "72%", "7deg",   "1.6s")
      , ("J♣", False, "88%", "74%", "-7deg",  ".4s")
      , ("10♥", True, "13%", "40%", "12deg",  "2.2s")
      , ("A♦", True,  "86%", "42%", "-12deg", "1.1s")
      ]
  ] ++
  [ H.div_ [ HP.class_ "titleSuits" ] [ text "♠ ♥ ♦ ♣" ]
  , H.h1_ [ HP.class_ "titleH" ] [ text "TEXAS HOLD 'EM" ]
  , H.div_ [ HP.class_ "titleSub" ] [ text "NO-LIMIT · SIX-MAX · MISO CASINO" ]
  , H.button_ [ HP.class_ "btn gold seatBtn", HE.onClick StartGame ]
      [ text "TAKE A SEAT" ]
  , H.button_ [ HP.class_ "btn ghost howBtn", HE.onClick ShowHelp ]
      [ text "HOW TO PLAY" ]
  , H.div_ [ HP.class_ "titleHint" ]
      [ text ("five rivals · " <> fmtChips startingStack
          <> " chips each · blinds rise every "
          <> ms handsPerLevel <> " hands · built with miso 🍜") ]
  ]
  where
    deco f red x y r dl = H.div_
      [ HP.class_ (joinCls [ "floatCard", clsWhen red "red" ])
      , CSS.style_
          [ CSS.left x, CSS.top y, "--fr" =: r, CSS.animationDelay dl ]
      ] [ text f ]
-----------------------------------------------------------------------------
topbar :: Model -> View () Model Action
topbar m = H.div_ [ HP.class_ "topbar" ]
  [ H.div_ [ HP.class_ "brand" ] [ text "♠ TEXAS HOLD 'EM" ]
  , H.div_ [ HP.class_ "hudStats" ]
      [ stat ("HAND " <> ms (m ^. handNo))
      , stat ("BLINDS " <> fmtChips sb <> "/" <> fmtChips bb)
      , stat (streetName (m ^. street))
      ]
  , H.div_ [ HP.class_ "tbBtns" ]
      [ iconBtn ShowHelp "❓" "help"
      , iconBtn ToggleSound
          (if m ^. soundOn then "🔊" else "🔇")
          (if m ^. soundOn then "sound" else "muted")
      , iconBtn BackToTitle "🚪" "leave"
      ]
  ]
  where
    (sb, bb) = blinds (max 1 (m ^. handNo))
    stat v = H.span_ [ HP.class_ "hudStat" ] [ text v ]
    iconBtn act icon label = H.button_
      [ HP.class_ "iconBtn", HE.onClick act ]
      [ text icon
      , H.span_ [ HP.class_ "btnLabel" ] [ text (" " <> label) ]
      ]
-----------------------------------------------------------------------------
streetName :: Street -> MisoString
streetName = \case
  Preflop -> "PRE-FLOP"; Flop -> "FLOP"; Turn -> "TURN"; River -> "RIVER"
-----------------------------------------------------------------------------
tableView :: Model -> View () Model Action
tableView m = H.div_ [ HP.class_ "tableWrap" ] $
  [ H.div_ [ HP.class_ "felt" ]
      [ H.div_ [ HP.class_ "feltRing" ] []
      , H.div_ [ HP.class_ "feltLogo" ] [ text "MISO CASINO" ]
      , boardView m
      , potView m
      ]
  ]
  ++ [ betSpot m j | j <- [0 .. seats - 1] ]
  ++ [ seatView m j | j <- [0 .. seats - 1] ]
-----------------------------------------------------------------------------
potView :: Model -> View () Model Action
potView m
  | total == 0 = H.div_ [ HP.class_ "pot dimmed" ] []
  | otherwise = H.div_ [ HP.class_ "pot", key_ (ms total) ]
      [ H.span_ [ HP.class_ "potChip" ] []
      , text ("POT " <> fmtChips total)
      ]
  where
    -- _pTotal already contains this street's bets
    total = potSize m
-----------------------------------------------------------------------------
boardView :: Model -> View () Model Action
boardView m = H.div_ [ HP.class_ "boardRow" ]
  [ slot k | k <- [0 .. 4] ]
  where
    lits = litCards m
    dulls = not (null lits)
    slot k = case drop k (m ^. board) of
      (card : _) -> cardFace
        (joinCls [ "bcard"
                 , clsWhen (card `elem` lits) "lit"
                 , clsWhen (dulls && card `notElem` lits) "dull"
                 ])
        (Just (ms k)) card
      [] -> H.div_ [ HP.class_ "cardSlot" ] []
-----------------------------------------------------------------------------
-- | Cards that belong to a winning five at showdown.
litCards :: Model -> [Card]
litCards m
  | m ^. phase == HandOver =
      nub (concat [ hvCards hv | Award _ _ _ (Just hv) <- m ^. awards ])
  | otherwise = []
-----------------------------------------------------------------------------
cardFace :: MisoString -> Maybe MisoString -> Card -> View () Model Action
cardFace cls mdelay card = H.div_
  ( HP.class_ (joinCls [ "card", cls, clsWhen (suitRed (suit card)) "red" ])
  : [ CSS.style_ [ CSS.animationDelay (d <> "00ms") ] | Just d <- [mdelay] ]
  )
  [ H.span_ [ HP.class_ "crank" ] [ text (rankFace (rank card)) ]
  , H.span_ [ HP.class_ "csuit" ] [ text (suitFace (suit card)) ]
  , H.span_ [ HP.class_ "cmark" ] [ text (suitFace (suit card)) ]
  ]
-----------------------------------------------------------------------------
cardBack :: View () Model Action
cardBack = H.div_ [ HP.class_ "card back" ]
  [ H.span_ [ HP.class_ "backPip" ] [ text "🍜" ] ]
-----------------------------------------------------------------------------
betSpot :: Model -> Int -> View () Model Action
betSpot m j
  | amt == 0 = H.div_ [] []
  | otherwise = H.div_
      [ HP.class_ ("betSpot bs" <> ms j), key_ ("bet" <> ms j <> "-" <> ms amt) ]
      [ chipStack amt
      , H.span_ [ HP.class_ "betAmt" ] [ text (fmtChips amt) ]
      ]
  where
    amt = _pBet (seatAt m j)
-----------------------------------------------------------------------------
-- | A little stack of chips for an amount (denomination-colored).
chipStack :: Int -> View () Model Action
chipStack amt = H.div_ [ HP.class_ "chips" ]
  [ H.div_
      [ HP.class_ ("chip " <> denomCls d)
      , CSS.style_ [ CSS.bottom (ms (k * 4) <> "px") ]
      ]
      []
  | (k, d) <- zip [0 :: Int ..] (take 5 (denoms amt))
  ]
  where
    denoms n = concat
      [ replicate (min 3 (n `div` d)) d
      | d <- [1000, 500, 100, 25, 5]
      , n `div` d > 0
      ]
    denomCls = \case
      1000 -> "c1000"; 500 -> "c500"; 100 -> "c100"; 25 -> "c25"; _ -> "c5"
-----------------------------------------------------------------------------
seatView :: Model -> Int -> View () Model Action
seatView m j = H.div_
  [ HP.class_ (joinCls
      [ "seat", "s" <> ms j
      , clsWhen isHero "hero"
      , clsWhen acting "acting"
      , clsWhen (_pFolded p) "folded"
      , clsWhen (_pOut p) "out"
      , clsWhen won "winner"
      ])
  ]
  ( [ H.div_ [ HP.class_ "bubble", key_ bubbleKey ] [ text lbl ]
    | Just lbl <- [_pLastAct p], m ^. phase == Playing ]
    ++
    [ H.div_ [ HP.class_ "seatCards" ] (holeViews m j) ]
    ++
    [ H.div_ [ HP.class_ "plate" ]
        [ H.div_ [ HP.class_ "avatar" ] [ text (_pAvatar p) ]
        , H.div_ [ HP.class_ "pinfo" ]
            [ H.div_ [ HP.class_ "pname" ]
                ( text (_pName p)
                : [ H.span_ [ HP.class_ "dbtn" ] [ text "D" ]
                  | m ^. button == j && not (_pOut p) ]
                )
            , H.div_ [ HP.class_ "pstack" ]
                [ text stackText ]
            ]
        ]
    ]
    ++
    [ H.div_ [ HP.class_ "winFloat", key_ ("w" <> ms (m ^. handNo)) ]
        [ text ("+" <> fmtChips (_pWon p)) ]
    | won ]
  )
  where
    p = seatAt m j
    isHero = j == heroSeat
    acting = m ^. phase == Playing && m ^. toAct == Just j
    won = m ^. phase == HandOver && _pWon p > 0
    bubbleKey = ms j <> "-" <> ms (m ^. animSeq)
    stackText
      | _pOut p = "BUSTED"
      | _pAllIn p && m ^. phase == Playing = "ALL-IN"
      | otherwise = fmtChips (_pStack p)
-----------------------------------------------------------------------------
holeViews :: Model -> Int -> [View () Model Action]
holeViews m j
  | null (_pHole p) || _pOut p || _pFolded p && not isHero = []
  | isHero || _pRevealed p =
      [ cardFace
          (joinCls
            [ clsWhen (card `elem` lits) "lit"
            , clsWhen (dulls && card `notElem` lits && m ^. phase == HandOver) "dull"
            , clsWhen (_pFolded p) "foldedCard"
            ])
          Nothing card
      | card <- _pHole p
      ]
  | otherwise = [ cardBack, cardBack ]
  where
    p = seatAt m j
    isHero = j == heroSeat
    lits = litCards m
    dulls = not (null lits)
-----------------------------------------------------------------------------
actionBar :: Model -> View () Model Action
actionBar m = H.div_ [ HP.class_ "abar" ] $
  [ H.div_ [ HP.class_ "tray" ]
      ( [ preset "MIN" (lMinTo lg)
        , preset "½ POT" (halfPot)
        , preset "POT" (fullPot)
        , preset "MAX" (lMaxTo lg)
        , H.input_
            [ HP.type_ "range"
            , HP.class_ "rslider"
            , HP.min_ (ms (lMinTo lg))
            , HP.max_ (ms (lMaxTo lg))
            , HP.step_ "5"
            , HP.value_ (ms (m ^. raiseAmt))
            , HE.onInput (SetRaise . parseAmt)
            ]
        , H.button_
            [ HP.class_ "btn gold confirm"
            , HE.onClick (HeroMove (MRaiseTo (m ^. raiseAmt)))
            ]
            [ text (raiseWord <> " " <> fmtChips (m ^. raiseAmt)) ]
        ] )
  | m ^. raiseOpen && lMayRaise lg
  ] ++
  [ H.div_ [ HP.class_ "abtns" ]
      ( [ H.button_ [ HP.class_ "btn afold", HE.onClick (HeroMove MFold) ]
            [ text "FOLD", keyHint "F" ]
        | not (lCheck lg) ]
        ++
        [ H.button_ [ HP.class_ "btn acall", HE.onClick (HeroMove MCheckCall) ]
            [ text callLabel, keyHint "C" ]
        ]
        ++
        [ H.button_
            [ HP.class_ (joinCls [ "btn gold araise", clsWhen (m ^. raiseOpen) "open" ])
            , HE.onClick ToggleRaise
            ]
            [ text raiseWord, keyHint "R" ]
        | lMayRaise lg ]
      )
  ]
  where
    lg = legalFor m heroSeat
    callAllIn = lCall lg >= _pStack (seatAt m heroSeat) && lCall lg > 0
    callLabel
      | lCheck lg = "CHECK"
      | callAllIn = "CALL ALL-IN " <> fmtChips (lCall lg)
      | otherwise = "CALL " <> fmtChips (lCall lg)
    raiseWord = if m ^. currentBet == 0 then "BET" else "RAISE"
    pot = potSize m
    clampTo n = max (lMinTo lg) (min (lMaxTo lg) n)
    halfPot = clampTo (m ^. currentBet + pot `div` 2)
    fullPot = clampTo (m ^. currentBet + pot)
    preset lbl n = H.button_
      [ HP.class_ (joinCls [ "btn pre", clsWhen (m ^. raiseAmt == n) "sel" ])
      , HE.onClick (SetRaise n)
      ] [ text lbl ]
    keyHint kb = H.span_ [ HP.class_ "kb" ] [ text kb ]
-----------------------------------------------------------------------------
parseAmt :: MisoString -> Int
parseAmt s = case reads (fromMisoString s) of
  [(n, "")] -> n
  _ -> 0
-----------------------------------------------------------------------------
bannerView :: Model -> View () Model Action
bannerView m = H.div_ [ HP.class_ "banner", key_ ("b" <> ms (m ^. handNo)) ]
  [ H.div_ [ HP.class_ "bpanel" ]
      ( [ H.div_ [ HP.class_ "bline" ]
            [ H.span_ [ HP.class_ "bwho" ]
                [ text (winnerLabel a <> " " <> fmtChips (awAmount a)) ]
            , H.span_ [ HP.class_ "bhand" ]
                [ text (maybe "— unchallenged" handName (awHand a)) ]
            ]
        | a <- m ^. awards
        ]
        ++
        [ H.button_ [ HP.class_ "btn gold next", HE.onClick NextHand ]
            [ text "NEXT HAND", H.span_ [ HP.class_ "kb" ] [ text "↵" ] ]
        ]
      )
  ]
  where
    winnerLabel a
      | awSeat a == heroSeat = "YOU WIN"
      | otherwise = awName a <> " WINS"
-----------------------------------------------------------------------------
gameOverView :: Model -> Bool -> View () Model Action
gameOverView m won = H.div_ [ HP.class_ "overlay" ]
  [ H.div_ [ HP.class_ "panel goPanel" ]
      [ H.div_ [ HP.class_ "goEmoji" ] [ text (if won then "🏆" else "💔") ]
      , H.div_ [ HP.class_ "goTitle" ]
          [ text (if won then "CHAMPION" else "BUSTED") ]
      , H.div_ [ HP.class_ "goSub" ]
          [ text (if won
              then "every chip on the table is yours"
              else "the table keeps your chips — this round") ]
      , statRow 0 "Hands played" (ms (m ^. handNo))
      , statRow 1 "Biggest pot won" (fmtChips (m ^. biggestPot))
      , statRow 2 "Blinds reached"
          (let (sb, bb) = blinds (max 1 (m ^. handNo))
           in fmtChips sb <> "/" <> fmtChips bb)
      , H.button_ [ HP.class_ "btn gold", HE.onClick StartGame ]
          [ text "PLAY AGAIN" ]
      , H.button_ [ HP.class_ "btn ghost", HE.onClick BackToTitle ]
          [ text "TITLE" ]
      ]
  ]
  where
    statRow :: Int -> MisoString -> MisoString -> View () Model Action
    statRow k label v = H.div_
      [ HP.class_ "statRow"
      , CSS.style_ [ CSS.animationDelay (ms (200 + k * 130) <> "ms") ]
      ]
      [ H.span_ [] [ text label ], H.b_ [] [ text v ] ]
-----------------------------------------------------------------------------
helpOverlay :: View () Model Action
helpOverlay = H.div_ [ HP.class_ "overlay help" ]
  [ H.div_ [ HP.class_ "panel helpPanel" ]
      [ H.button_ [ HP.class_ "helpClose", HE.onClick CloseHelp ] [ text "✕" ]
      , H.div_ [ HP.class_ "helpH" ] [ text "HOW TO PLAY" ]
      , H.div_ [ HP.class_ "helpSub" ]
          [ text "no-limit texas hold 'em · six-max" ]
      , sec "THE DEAL"
      , para $
          "You get two hidden cards; five community cards land face up "
          <> "in the middle — the flop (three), the turn, and the river. "
          <> "Your hand is the best five cards out of your two plus the "
          <> "board's five."
      , sec "THE BETTING"
      , para $
          "Four betting rounds: before the flop, and after each street. "
          <> "CHECK passes when nothing is owed, CALL matches the current "
          <> "bet, RAISE puts the pressure on — drag the slider or tap "
          <> "½ POT / POT / MAX. Run out of chips and you're ALL-IN for "
          <> "whatever you can cover; side pots are handled for you."
      , sec "THE SHOWDOWN"
      , para $
          "Last player standing takes the pot uncontested. Otherwise "
          <> "hands go face up and the best five-card hand wins — the "
          <> "winning cards light up so you can see why."
      , sec "HAND RANKS, LOW TO HIGH"
      , para $
          "high card · pair · two pair · trips · straight · flush · "
          <> "full house · quads · straight flush"
      , sec "THE TABLE"
      , para $
          "Five rivals with five temperaments, blinds rise every "
          <> "eight hands, and the dealer button (D) moves every hand. "
          <> "Outlast everyone to take it all."
      , sec "KEYS"
      , para $
          "F fold · C or space check/call · R raise (arrows size it, "
          <> "enter confirms) · A all-in · H help"
      , H.button_ [ HP.class_ "btn gold", HE.onClick CloseHelp ]
          [ text "GOT IT" ]
      ]
  ]
  where
    sec s = H.div_ [ HP.class_ "helpSec" ] [ text s ]
    para s = H.p_ [ HP.class_ "helpP" ] [ text s ]
-----------------------------------------------------------------------------
joinCls :: [MisoString] -> MisoString
joinCls = mconcat . map (<> " ")
-----------------------------------------------------------------------------
clsWhen :: Bool -> MisoString -> MisoString
clsWhen True c = c
clsWhen False _ = ""
