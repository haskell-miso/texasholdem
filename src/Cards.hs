-----------------------------------------------------------------------------
-- | Cards: ranks, suits, the 52-card deck, and display helpers.
-- Shuffling is pure, driven by a supply of random 'Double's, so the
-- whole deal is reproducible and testable.
-----------------------------------------------------------------------------
module Cards where
-----------------------------------------------------------------------------
import           Data.List (sortOn)
-----------------------------------------------------------------------------
import           Miso.String (MisoString, ms)
-----------------------------------------------------------------------------
-- | Two = 2 .. Ace = 14, so 'fromEnum' is the poker rank value.
data Rank
  = Two | Three | Four | Five | Six | Seven | Eight
  | Nine | Ten | Jack | Queen | King | Ace
  deriving (Eq, Ord, Show, Enum, Bounded)
-----------------------------------------------------------------------------
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Show, Enum, Bounded)
-----------------------------------------------------------------------------
data Card = Card
  { rank :: Rank
  , suit :: Suit
  } deriving (Eq, Ord, Show)
-----------------------------------------------------------------------------
-- | Numeric rank value: Two = 2 .. Ace = 14.
rankVal :: Rank -> Int
rankVal r = fromEnum r + 2
-----------------------------------------------------------------------------
fullDeck :: [Card]
fullDeck = [ Card r s | s <- [minBound ..], r <- [minBound ..] ]
-----------------------------------------------------------------------------
-- | Order a deck by a supply of random keys (same trick as the rest of
-- the miso game family): zip, sort on the key, forget the key.
shuffleWith :: [Double] -> [a] -> [a]
shuffleWith keys xs = map snd (sortOn fst (zip keys xs))
-----------------------------------------------------------------------------
-- * Display
-----------------------------------------------------------------------------
rankFace :: Rank -> MisoString
rankFace = \case
  Jack -> "J"; Queen -> "Q"; King -> "K"; Ace -> "A"; Ten -> "10"
  r -> ms (rankVal r)
-----------------------------------------------------------------------------
suitFace :: Suit -> MisoString
suitFace = \case
  Clubs -> "♣"; Diamonds -> "♦"; Hearts -> "♥"; Spades -> "♠"
-----------------------------------------------------------------------------
suitRed :: Suit -> Bool
suitRed s = s == Diamonds || s == Hearts
-----------------------------------------------------------------------------
-- | Long rank names for winner banners ("Kings full of Fours").
rankName :: Rank -> MisoString
rankName = \case
  Two -> "Two"; Three -> "Three"; Four -> "Four"; Five -> "Five"
  Six -> "Six"; Seven -> "Seven"; Eight -> "Eight"; Nine -> "Nine"
  Ten -> "Ten"; Jack -> "Jack"; Queen -> "Queen"; King -> "King"
  Ace -> "Ace"
-----------------------------------------------------------------------------
-- | Plural rank names ("Kings", "Sixes").
rankPlural :: Rank -> MisoString
rankPlural r = case r of
  Six -> "Sixes"
  _ -> rankName r <> "s"
