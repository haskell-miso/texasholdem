-----------------------------------------------------------------------------
{-# LANGUAGE TemplateHaskell #-}
-----------------------------------------------------------------------------
-- | Core types for miso-texasholdem, with lenses via "Miso.Lens.TH".
-----------------------------------------------------------------------------
module Model where
-----------------------------------------------------------------------------
import           Data.IntSet (IntSet)
import qualified Data.IntSet as IS
-----------------------------------------------------------------------------
import           Miso.Lens.TH (makeLenses)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
import           Cards
import           Eval (HandValue)
-----------------------------------------------------------------------------
data Street = Preflop | Flop | Turn | River
  deriving (Eq, Ord, Show, Enum, Bounded)
-----------------------------------------------------------------------------
-- | A normalized player decision. @MRaiseTo n@ is the raise-*to* total
-- for the street (covers open bets too); the engine clamps it to legal
-- bounds, so an over-stack raise becomes all-in.
data Move = MFold | MCheckCall | MRaiseTo Int
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | Bot temperament, fixed per seat.
data Style = Style
  { aggro :: Double -- ^ 0..1, how often value turns into raises
  , loose :: Double -- ^ 0..1, how wide they play preflop
  , bluff :: Double -- ^ 0..1, chance to fire with nothing
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
data Player = Player
  { _pName :: MisoString
  , _pAvatar :: MisoString
  , _pStyle :: Maybe Style     -- ^ Nothing = the human hero
  , _pStack :: Int             -- ^ chips behind
  , _pBet :: Int               -- ^ committed this street
  , _pTotal :: Int             -- ^ committed this hand (side pots)
  , _pHole :: [Card]
  , _pFolded :: Bool
  , _pAllIn :: Bool
  , _pOut :: Bool              -- ^ eliminated from the table
  , _pActed :: Bool            -- ^ acted since the last full raise
  , _pRevealed :: Bool         -- ^ cards face up at showdown
  , _pLastAct :: Maybe MisoString -- ^ speech bubble ("RAISE 120")
  , _pWon :: Int               -- ^ chips won in the hand just finished
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
makeLenses ''Player
-----------------------------------------------------------------------------
data Phase
  = Title
  | Playing
  | HandOver          -- ^ pots awarded, banner up, next hand pending
  | GameOver Bool     -- ^ True = hero is the champion
  deriving (Eq, Show)
-----------------------------------------------------------------------------
-- | One line of the winner banner.
data Award = Award
  { awSeat :: Int
  , awName :: MisoString
  , awAmount :: Int
  , awHand :: Maybe HandValue -- ^ Nothing when everyone folded
  } deriving (Show, Eq)
-----------------------------------------------------------------------------
data Model = Model
  { _players :: [Player]      -- ^ six seats, hero at 0
  , _button :: Int
  , _street :: Street
  , _board :: [Card]
  , _deck :: [Card]
  , _toAct :: Maybe Int
  , _currentBet :: Int        -- ^ to match this street
  , _minRaise :: Int          -- ^ size of the last full raise
  , _handNo :: Int
  , _phase :: Phase
  , _awards :: [Award]
  , _raiseAmt :: Int          -- ^ hero's slider, a raise-to amount
  , _raiseOpen :: Bool        -- ^ hero's raise tray expanded
  , _pacer :: Int             -- ^ guards scheduled bot/advance actions
  , _animSeq :: Int           -- ^ remount key for pop animations
  , _biggestPot :: Int        -- ^ hero stat for the game-over screen
  , _soundOn :: Bool
  , _showHelp :: Bool
  , _heldKeys :: IntSet
  } deriving (Eq, Show)
-----------------------------------------------------------------------------
makeLenses ''Model
-----------------------------------------------------------------------------
data Action
  = NoOp
  | StartGame               -- ^ title button; inits audio, shuffles
  | DealHand [Double]       -- ^ randomness supply for the shuffle
  | HeroMove Move
  | BotMove Int Int [Double] -- ^ pacer stamp, seat, decision randomness
  | NextHand                -- ^ the button in the banner
  | SetRaise Int
  | ToggleRaise
  | Keys IntSet
  | ToggleSound
  | ShowHelp
  | CloseHelp
  | BackToTitle
-----------------------------------------------------------------------------
heroSeat :: Int
heroSeat = 0
-----------------------------------------------------------------------------
seats :: Int
seats = 6
-----------------------------------------------------------------------------
startingStack :: Int
startingStack = 3000
-----------------------------------------------------------------------------
-- | Blind levels, stepping up every 'handsPerLevel' hands.
blindLevels :: [(Int, Int)]
blindLevels =
  [ (10, 20), (15, 30), (25, 50), (40, 80), (60, 120)
  , (100, 200), (150, 300), (250, 500), (400, 800), (600, 1200)
  ]
-----------------------------------------------------------------------------
handsPerLevel :: Int
handsPerLevel = 8
-----------------------------------------------------------------------------
blinds :: Int -> (Int, Int)
blinds hand = blindLevels !! lvl
  where
    lvl = min (length blindLevels - 1) ((hand - 1) `div` handsPerLevel)
-----------------------------------------------------------------------------
mkBot :: MisoString -> MisoString -> Style -> Player
mkBot name avatar sty = (emptyPlayer name avatar) { _pStyle = Just sty }
-----------------------------------------------------------------------------
emptyPlayer :: MisoString -> MisoString -> Player
emptyPlayer name avatar = Player
  { _pName = name
  , _pAvatar = avatar
  , _pStyle = Nothing
  , _pStack = startingStack
  , _pBet = 0
  , _pTotal = 0
  , _pHole = []
  , _pFolded = False
  , _pAllIn = False
  , _pOut = False
  , _pActed = False
  , _pRevealed = False
  , _pLastAct = Nothing
  , _pWon = 0
  }
-----------------------------------------------------------------------------
initialPlayers :: [Player]
initialPlayers =
  [ emptyPlayer "You" "🤵"
  , mkBot "Ruby"   "💃" (Style 0.75 0.55 0.30)
  , mkBot "Tex"    "🤠" (Style 0.85 0.70 0.35)
  , mkBot "Prof"   "🧐" (Style 0.35 0.30 0.10)
  , mkBot "Viper"  "🐍" (Style 0.65 0.45 0.25)
  , mkBot "Lucky"  "🎰" (Style 0.55 0.80 0.20)
  ]
-----------------------------------------------------------------------------
initialModel :: Model
initialModel = Model
  { _players = initialPlayers
  , _button = seats - 1  -- so hand #1 puts the button on seat 0's right… rotated in dealHand
  , _street = Preflop
  , _board = []
  , _deck = []
  , _toAct = Nothing
  , _currentBet = 0
  , _minRaise = 0
  , _handNo = 0
  , _phase = Title
  , _awards = []
  , _raiseAmt = 0
  , _raiseOpen = False
  , _pacer = 0
  , _animSeq = 0
  , _biggestPot = 0
  , _soundOn = True
  , _showHelp = False
  , _heldKeys = IS.empty
  }
-----------------------------------------------------------------------------
-- | Total chips in the middle: everything committed this hand.
potSize :: Model -> Int
potSize m = sum [ _pTotal p | p <- _players m ]
-----------------------------------------------------------------------------
-- | Update seat @i@.
updateSeat :: Int -> (Player -> Player) -> Model -> Model
updateSeat i f m =
  m { _players = [ if j == i then f p else p | (j, p) <- zip [0 ..] (_players m) ] }
-----------------------------------------------------------------------------
seatAt :: Model -> Int -> Player
seatAt m i = _players m !! i
