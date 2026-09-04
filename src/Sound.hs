-----------------------------------------------------------------------------
-- | Sound effects synthesized with the Web Audio API — no audio assets.
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
import           Control.Monad (when)
-----------------------------------------------------------------------------
import           Miso.FFI.QQ (js)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- | Build the tiny synth once and park it on @globalThis.__mth@.
soundInit :: IO ()
soundInit = [js|
  if (!globalThis.__mth) {
    var AC = window.AudioContext || window.webkitAudioContext;
    var ctx = new AC();
    var master = ctx.createGain();
    master.gain.value = 0.45;
    master.connect(ctx.destination);
    function noiseBuf(dur) {
      var n = Math.floor(ctx.sampleRate * dur);
      var b = ctx.createBuffer(1, n, ctx.sampleRate);
      var d = b.getChannelData(0);
      for (var i = 0; i < n; i++) d[i] = Math.random() * 2 - 1;
      return b;
    }
    function env(g, t0, a, peak, d) {
      g.gain.setValueAtTime(0, t0);
      g.gain.linearRampToValueAtTime(peak, t0 + a);
      g.gain.exponentialRampToValueAtTime(0.0001, t0 + a + d);
    }
    function tone(freq, type, t0, a, peak, d) {
      var o = ctx.createOscillator();
      o.type = type;
      o.frequency.value = freq;
      var g = ctx.createGain();
      env(g, t0, a, peak, d);
      o.connect(g);
      g.connect(master);
      o.start(t0);
      o.stop(t0 + a + d + 0.05);
    }
    function slide(t0, dur, f0, f1, peak, q) {
      var s = ctx.createBufferSource();
      s.buffer = noiseBuf(dur + 0.02);
      var f = ctx.createBiquadFilter();
      f.type = 'bandpass';
      f.frequency.setValueAtTime(f0, t0);
      f.frequency.exponentialRampToValueAtTime(Math.max(f1, 40), t0 + dur);
      f.Q.value = q || 1.2;
      var g = ctx.createGain();
      env(g, t0, 0.004, peak, dur);
      s.connect(f);
      f.connect(g);
      g.connect(master);
      s.start(t0);
    }
    function chipClick(t0, peak) {
      // ceramic chip: two very short detuned pings + a click of noise
      tone(2350 + Math.random() * 300, 'sine', t0, 0.001, peak, 0.045);
      tone(3150 + Math.random() * 400, 'sine', t0 + 0.004, 0.001, peak * 0.6, 0.03);
      slide(t0, 0.018, 5000, 2500, peak * 0.5, 2.5);
    }
    globalThis.__mth = {
      ctx: ctx,
      play: function (name) {
        if (ctx.state === 'suspended') ctx.resume();
        var t = ctx.currentTime + 0.01;
        var i;
        if (name === 'deal') {
          for (i = 0; i < 4; i++) {
            slide(t + i * 0.07, 0.05, 1800 + (i % 2) * 500, 700, 0.3);
          }
        } else if (name === 'flip') {
          slide(t, 0.045, 2600, 1100, 0.4);
          tone(190, 'sine', t + 0.01, 0.002, 0.18, 0.06);
        } else if (name === 'chip') {
          for (i = 0; i < 3; i++) chipClick(t + i * 0.045, 0.35 - i * 0.08);
        } else if (name === 'chips') {
          for (i = 0; i < 7; i++) {
            chipClick(t + i * 0.03 + Math.random() * 0.012, 0.3 - i * 0.03);
          }
        } else if (name === 'check') {
          tone(160, 'sine', t, 0.002, 0.5, 0.07);
          tone(150, 'sine', t + 0.09, 0.002, 0.4, 0.07);
        } else if (name === 'fold') {
          slide(t, 0.14, 1400, 300, 0.2);
        } else if (name === 'shuffle') {
          for (i = 0; i < 9; i++) {
            slide(t + i * 0.045, 0.035, 2400 + (i % 3) * 400, 900, 0.16);
          }
          slide(t + 0.42, 0.11, 1500, 500, 0.3);
        } else if (name === 'allin') {
          slide(t, 0.5, 300, 2400, 0.2, 3);
          for (i = 0; i < 10; i++) chipClick(t + 0.1 + i * 0.04, 0.24);
        } else if (name === 'win') {
          var ns = [523.25, 659.25, 783.99, 1046.5];
          for (i = 0; i < ns.length; i++) {
            tone(ns[i], 'triangle', t + i * 0.1, 0.01, 0.4, 0.6);
            tone(ns[i] * 2, 'sine', t + i * 0.1, 0.01, 0.12, 0.4);
          }
          for (i = 0; i < 8; i++) chipClick(t + 0.15 + i * 0.05, 0.2);
        } else if (name === 'lose') {
          tone(220, 'triangle', t, 0.01, 0.3, 0.5);
          tone(174.6, 'triangle', t + 0.22, 0.01, 0.3, 0.8);
        } else if (name === 'click') {
          tone(900, 'sine', t, 0.001, 0.12, 0.04);
        }
      }
    };
  }
|]
-----------------------------------------------------------------------------
-- | Play a named effect (respecting the model's sound toggle).
playSound :: Bool -> MisoString -> IO ()
playSound enabled name = when enabled
  [js| if (globalThis.__mth) { globalThis.__mth.play(${name}); } |]
