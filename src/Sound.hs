-----------------------------------------------------------------------------
-- | Sound effects synthesized with the Web Audio API — no audio assets.
-- The synth itself is JavaScript, in @static/sound.js@.
-- Card slides and flips are filtered noise, chips are detuned ceramic
-- clicks, and the fanfares are little additive arpeggios. 'soundInit'
-- must run inside a user gesture (the title-screen button) so the
-- AudioContext is allowed to play.
-----------------------------------------------------------------------------
module Sound
  ( soundInit
  , playSound
  ) where
-----------------------------------------------------------------------------
import           Control.Monad (void, when)
-----------------------------------------------------------------------------
import           Miso (jsg0, jsg1)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Build the tiny synth once and park it on @globalThis.__mth@
-- (@__mthInit@ in @static/sound.js@).
soundInit :: IO ()
soundInit = void (jsg0 "__mthInit")
-----------------------------------------------------------------------------
-- | Play a named effect (respecting the model's sound toggle).
playSound :: Bool -> MisoString -> IO ()
playSound enabled name = when enabled $
  void (jsg1 "__mthPlay" name)
