-----------------------------------------------------------------------------
-- | The look of the room: a dark lounge in warm charcoal, one pool of
-- light over an emerald stadium of felt, champagne-gold chrome, and
-- crisp ivory cards. Modern casino, no clutter.
-----------------------------------------------------------------------------
module Styles (skin) where
-----------------------------------------------------------------------------
import           Miso ((=:))
import qualified Miso.CSS as CSS
import           Miso.CSS
  ( StyleSheet, sheet_, selector_, keyframes_, from_, to_, at, pct
  , media_, rule_, screen_, and_, maxWidth_, maxHeight_, px
  )
import           Miso.CSS.Types (MediaQuery(..))
import           Miso.CSS.Color hiding (ivory, crimson)
import           Miso.String (MisoString)
-----------------------------------------------------------------------------
-- palette: the room, the felt, the gold, the ink
goldHi, goldMid, goldDeep, feltHi, ivory, mist, inkDark, crimson :: Color
goldHi   = RGB 240 208 138   -- bright champagne
goldMid  = RGB 201 164  92   -- worked brass
goldDeep = RGB 138 109  53   -- shadowed gold
feltHi   = RGB  27 122  86   -- lit felt
ivory    = RGB 242 237 224   -- text on dark
mist     = RGB 148 158 170   -- muted text
inkDark  = RGB  30  34  40   -- card ink / dark chip
crimson  = RGB 206  63  59   -- hearts, diamonds, fold
-----------------------------------------------------------------------------
serif, sans :: MisoString
serif = "'Fraunces', 'Playfair Display', Georgia, serif"
sans = "'Outfit', 'Avenir Next', 'Segoe UI', system-ui, sans-serif"
-----------------------------------------------------------------------------
roomBackground :: MisoString
roomBackground = mconcat
  [ "radial-gradient(90% 70% at 50% 12%, rgba(232,199,125,.075), rgba(0,0,0,0) 60%), "
  , "radial-gradient(140% 110% at 50% 115%, rgba(10,46,32,.5), rgba(0,0,0,0) 55%), "
  , "linear-gradient(180deg, #14181D 0%, #0E1114 55%, #0A0C0F 100%)"
  ]
-----------------------------------------------------------------------------
feltBackground :: MisoString
feltBackground = mconcat
  [ "radial-gradient(closest-side at 50% 38%, rgba(255,255,255,.09), rgba(0,0,0,0) 72%), "
  , "radial-gradient(140% 130% at 50% 30%, #1B7A56 0%, #115C3F 52%, #0B4230 82%, #093527 100%)"
  ]
-----------------------------------------------------------------------------
goldGrad :: MisoString
goldGrad = "linear-gradient(180deg, #F0D08A 0%, #D9B26A 55%, #C9A45C 100%)"
-----------------------------------------------------------------------------
glassBg :: MisoString
glassBg = "linear-gradient(180deg, rgba(32,36,43,.92), rgba(17,20,24,.94))"
-----------------------------------------------------------------------------
skin :: StyleSheet
skin = sheet_
  [ selector_ ":root"
      [ "--tw" =: "min(1060px, 96vw, calc((100dvh - 250px) * 1.85))"
      , "--th" =: "calc(var(--tw) / 1.85)"
      , "--cw" =: "calc(var(--tw) * 0.052)"
      , "--serif" =: serif
      , "--sans" =: sans
      ]
  , selector_ "*" [ CSS.boxSizing "border-box" ]
  , selector_ "html, body"
      [ CSS.margin "0", CSS.height "100%", CSS.overflow "hidden" ]
  , selector_ "body"
      [ CSS.background roomBackground
      , CSS.color ivory
      , CSS.fontFamily sans
      , CSS.userSelect "none"
      , "-webkit-tap-highlight-color" =: "transparent"
      , "-webkit-text-size-adjust" =: "100%"
      , "overscroll-behavior" =: "none"
      ]
  , selector_ "button"
      [ CSS.fontFamily sans ]
  , selector_ "button:focus-visible"
      [ CSS.outline "2px solid #E8C77D", CSS.outlineOffset "2px" ]
  -- top chrome ---------------------------------------------------------------
  , selector_ ".topbar"
      [ CSS.position "fixed"
      , "inset" =: "0 0 auto 0"
      , CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.justifyContent "space-between"
      , CSS.padding "10px 18px"
      , CSS.zIndex 90
      , CSS.pointerEvents "none"
      , CSS.gap "12px"
      ]
  , selector_ ".topbar > *" [ CSS.pointerEvents "auto" ]
  , selector_ ".brand"
      [ CSS.fontFamily "var(--serif)"
      , CSS.fontWeight "600"
      , CSS.letterSpacing ".14em"
      , CSS.fontSize "15px"
      , CSS.color goldHi
      , CSS.whiteSpace "nowrap"
      ]
  , selector_ ".hudStats"
      [ CSS.display "flex"
      , CSS.gap "clamp(10px, 2.4vw, 30px)"
      , CSS.alignItems "center"
      , CSS.fontSize "12px"
      , CSS.letterSpacing ".14em"
      , CSS.color mist
      , CSS.whiteSpace "nowrap"
      , "font-variant-numeric" =: "tabular-nums"
      ]
  , selector_ ".tbBtns"
      [ CSS.display "flex", CSS.gap "8px"
      , CSS.flexWrap "wrap", CSS.justifyContent "flex-end" ]
  , selector_ ".iconBtn"
      [ CSS.background "rgba(255,255,255,.06)"
      , CSS.border "1px solid rgba(255,255,255,.13)"
      , CSS.color ivory
      , CSS.borderRadius (px 9)
      , CSS.padding "7px 13px"
      , CSS.fontSize "13px"
      , CSS.letterSpacing ".06em"
      , CSS.cursor "pointer"
      , CSS.backdropFilter "blur(10px)"
      , CSS.transition "transform .15s ease, border-color .2s ease"
      , CSS.whiteSpace "nowrap"
      , "touch-action" =: "manipulation"
      ]
  , media_ (MediaQuery "(hover: hover)")
      [ rule_ ".iconBtn:hover"
          [ CSS.transform "translateY(-1px)"
          , "border-color" =: "rgba(232,199,125,.5)" ]
      ]
  -- the room and the table ---------------------------------------------------
  , selector_ ".room"
      [ CSS.position "fixed"
      , "inset" =: "46px 0 0 0"
      , CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.justifyContent "center"
      ]
  , selector_ ".tableWrap"
      [ CSS.position "relative"
      , CSS.width "var(--tw)"
      , CSS.height "var(--th)"
      ]
  , selector_ ".felt"
      [ CSS.position "absolute"
      , "inset" =: "0"
      , CSS.background feltBackground
      , CSS.borderRadius "calc(var(--th) / 2)"
      , CSS.border "calc(var(--tw) * 0.013) solid #221A15"
      , CSS.boxShadow (mconcat
          [ "inset 0 0 calc(var(--tw)*.06) rgba(0,0,0,.55), "
          , "inset 0 2px 0 rgba(255,255,255,.06), "
          , "0 0 0 2px rgba(232,199,125,.14), "
          , "0 calc(var(--tw)*.02) calc(var(--tw)*.05) rgba(0,0,0,.6)"
          ])
      ]
  , selector_ ".feltRing"
      [ CSS.position "absolute"
      , "inset" =: "7.5% 6%"
      , CSS.border "1.5px solid rgba(232,199,125,.16)"
      , CSS.borderRadius "calc(var(--th) / 2.4)"
      , CSS.pointerEvents "none"
      ]
  , selector_ ".feltLogo"
      [ CSS.position "absolute"
      , CSS.left "50%", CSS.top "70%"
      , CSS.transform "translate(-50%,-50%)"
      , CSS.fontFamily "var(--serif)"
      , CSS.fontWeight "600"
      , CSS.letterSpacing ".5em"
      , "text-indent" =: ".5em"
      , CSS.fontSize "calc(var(--tw) * .020)"
      , CSS.color (RGBA 232 199 125 0.14)
      , CSS.whiteSpace "nowrap"
      , CSS.pointerEvents "none"
      ]
  -- board & pot ----------------------------------------------------------------
  , selector_ ".boardRow"
      [ CSS.position "absolute"
      , CSS.left "50%", CSS.top "40%"
      , CSS.transform "translate(-50%,-50%)"
      , CSS.display "flex"
      , CSS.gap "calc(var(--tw) * .008)"
      ]
  , selector_ ".cardSlot"
      [ CSS.width "var(--cw)"
      , "aspect-ratio" =: ".72"
      , CSS.border "1.5px dashed rgba(255,255,255,.14)"
      , CSS.borderRadius "calc(var(--cw) * .12)"
      ]
  , selector_ ".card"
      [ CSS.position "relative"
      , CSS.width "var(--cw)"
      , "aspect-ratio" =: ".72"
      , CSS.background "linear-gradient(160deg, #FDFBF4 0%, #F4EFE2 100%)"
      , CSS.borderRadius "calc(var(--cw) * .12)"
      , CSS.boxShadow "0 2px 6px rgba(0,0,0,.45), inset 0 0 0 1px rgba(0,0,0,.06)"
      , CSS.color inkDark
      , CSS.overflow "hidden"
      , CSS.flexShrink 0
      , CSS.transition "transform .2s ease, box-shadow .25s ease, filter .3s ease"
      ]
  , selector_ ".card .crank"
      [ CSS.position "absolute"
      , CSS.left "9%", CSS.top "4%"
      , CSS.fontWeight "700"
      , CSS.fontSize "calc(var(--cw) * .34)"
      , CSS.lineHeight "1.05"
      , "font-variant-numeric" =: "tabular-nums"
      ]
  , selector_ ".card .csuit"
      [ CSS.position "absolute"
      , CSS.left "9%", CSS.top "38%"
      , CSS.fontSize "calc(var(--cw) * .3)"
      , CSS.lineHeight "1"
      ]
  , selector_ ".card .cmark"
      [ CSS.position "absolute"
      , CSS.right "-12%", CSS.bottom "-16%"
      , CSS.fontSize "calc(var(--cw) * .95)"
      , CSS.opacity 0.16
      , CSS.lineHeight "1"
      ]
  , selector_ ".card.red" [ CSS.color crimson ]
  , selector_ ".card.back"
      [ CSS.background (mconcat
          [ "repeating-linear-gradient(45deg, rgba(255,255,255,.06) 0 2px, rgba(0,0,0,0) 2px 7px), "
          , "repeating-linear-gradient(-45deg, rgba(255,255,255,.05) 0 2px, rgba(0,0,0,0) 2px 7px), "
          , "linear-gradient(160deg, #7E2B33 0%, #5E1F26 100%)"
          ])
      , CSS.boxShadow "0 2px 6px rgba(0,0,0,.5), inset 0 0 0 2px rgba(255,255,255,.14)"
      ]
  , selector_ ".card.back .backPip"
      [ CSS.position "absolute"
      , "inset" =: "0"
      , CSS.display "grid"
      , "place-items" =: "center"
      , CSS.fontSize "calc(var(--cw) * .4)"
      , CSS.opacity 0.75
      ]
  , selector_ ".bcard"
      [ CSS.animation "dealIn .5s cubic-bezier(.2,.9,.3,1.15) backwards" ]
  , selector_ ".card.lit"
      [ CSS.boxShadow "0 0 0 2px #E8C77D, 0 0 20px rgba(232,199,125,.55), 0 2px 8px rgba(0,0,0,.5)"
      , CSS.transform "translateY(calc(var(--cw) * -.07))"
      , CSS.zIndex 5
      ]
  , selector_ ".card.dull" [ CSS.filter "grayscale(.5) brightness(.55)" ]
  , selector_ ".card.foldedCard" [ CSS.filter "grayscale(.7) brightness(.6)" ]
  , selector_ ".pot"
      [ CSS.position "absolute"
      , CSS.left "50%", CSS.top "58.5%"
      , CSS.transform "translate(-50%,-50%)"
      , CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.gap "7px"
      , CSS.background "rgba(6,10,9,.5)"
      , CSS.border "1px solid rgba(232,199,125,.35)"
      , CSS.borderRadius (px 999)
      , CSS.padding "5px 14px"
      , CSS.color goldHi
      , CSS.fontWeight "600"
      , CSS.fontSize "calc(var(--tw) * .017)"
      , CSS.letterSpacing ".1em"
      , "font-variant-numeric" =: "tabular-nums"
      , CSS.animation "potIn .3s ease backwards"
      , CSS.whiteSpace "nowrap"
      ]
  , selector_ ".pot.dimmed" [ CSS.display "none" ]
  , selector_ ".potChip"
      [ CSS.width "12px", CSS.height "12px"
      , CSS.borderRadius (pct 50)
      , CSS.background goldGrad
      , CSS.boxShadow "inset 0 0 0 2px rgba(0,0,0,.25)"
      , CSS.display "inline-block"
      ]
  -- bets ----------------------------------------------------------------------
  , selector_ ".betSpot"
      [ CSS.position "absolute"
      , CSS.transform "translate(-50%,-50%)"
      , CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.gap "6px"
      , CSS.zIndex 6
      , CSS.animation "betIn .3s cubic-bezier(.2,.9,.3,1.2) backwards"
      , CSS.pointerEvents "none"
      ]
  , selector_ ".chips"
      [ CSS.position "relative"
      , CSS.width "22px", CSS.height "30px"
      ]
  , selector_ ".chip"
      [ CSS.position "absolute"
      , CSS.left "0"
      , CSS.width "22px", CSS.height "22px"
      , CSS.borderRadius (pct 50)
      , CSS.boxShadow "inset 0 0 0 2px rgba(255,255,255,.3), inset 0 -2px 0 rgba(0,0,0,.35), 0 1px 2px rgba(0,0,0,.5)"
      , CSS.border "2px dashed rgba(255,255,255,.4)"
      ]
  , selector_ ".chip.c5" [ CSS.background "#C0443B" ]
  , selector_ ".chip.c25" [ CSS.background "#2F9E63" ]
  , selector_ ".chip.c100" [ CSS.background "#2A2E35" ]
  , selector_ ".chip.c500" [ CSS.background "#7A4FB3" ]
  , selector_ ".chip.c1000"
      [ CSS.background goldGrad, CSS.border "2px dashed rgba(0,0,0,.35)" ]
  , selector_ ".betAmt"
      [ CSS.fontSize "12px"
      , CSS.fontWeight "700"
      , CSS.color goldHi
      , CSS.background "rgba(6,10,9,.55)"
      , CSS.borderRadius (px 999)
      , CSS.padding "2px 8px"
      , "font-variant-numeric" =: "tabular-nums"
      ]
  -- seats ---------------------------------------------------------------------
  , selector_ ".seat"
      [ CSS.position "absolute"
      , CSS.transform "translate(-50%,-50%)"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.alignItems "center"
      , CSS.gap "5px"
      , CSS.zIndex 10
      , CSS.transition "opacity .35s ease, filter .35s ease"
      ]
  , selector_ ".seat.s0" [ CSS.left "50%", CSS.top "97%" ]
  , selector_ ".seat.s1" [ CSS.left "13%", CSS.top "84%" ]
  , selector_ ".seat.s2" [ CSS.left "5%",  CSS.top "24%" ]
  , selector_ ".seat.s3" [ CSS.left "50%", CSS.top "2%" ]
  , selector_ ".seat.s4" [ CSS.left "95%", CSS.top "24%" ]
  , selector_ ".seat.s5" [ CSS.left "87%", CSS.top "84%" ]
  , selector_ ".betSpot.bs0" [ CSS.left "50%", CSS.top "72%" ]
  , selector_ ".betSpot.bs1" [ CSS.left "24%", CSS.top "63%" ]
  , selector_ ".betSpot.bs2" [ CSS.left "18%", CSS.top "31%" ]
  , selector_ ".betSpot.bs3" [ CSS.left "50%", CSS.top "21%" ]
  , selector_ ".betSpot.bs4" [ CSS.left "82%", CSS.top "31%" ]
  , selector_ ".betSpot.bs5" [ CSS.left "76%", CSS.top "63%" ]
  , selector_ ".seat.folded" [ CSS.opacity 0.4, CSS.filter "saturate(.4)" ]
  , selector_ ".seat.out" [ CSS.opacity 0.2, CSS.filter "grayscale(1)" ]
  , selector_ ".seatCards"
      [ CSS.display "flex"
      , CSS.minHeight "calc(var(--cw) * .6)"
      ]
  , selector_ ".seat .card"
      [ "--cw" =: "calc(var(--tw) * 0.036)" ]
  , selector_ ".seat .card + .card"
      [ CSS.marginLeft "calc(var(--cw) * -.3)"
      , CSS.transform "rotate(7deg) translateY(2%)"
      ]
  , selector_ ".seat.hero .card"
      [ "--cw" =: "calc(var(--tw) * 0.058)" ]
  , selector_ ".seat.hero .card + .card"
      [ CSS.transform "rotate(6deg) translateY(2%)" ]
  , selector_ ".seat.hero .card.lit"
      [ CSS.transform "translateY(calc(var(--cw) * -.07))" ]
  , selector_ ".plate"
      [ CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.gap "8px"
      , CSS.background glassBg
      , CSS.border "1px solid rgba(255,255,255,.09)"
      , CSS.borderRadius (px 14)
      , CSS.padding "5px 12px 5px 6px"
      , CSS.backdropFilter "blur(8px)"
      , CSS.minWidth "104px"
      , CSS.boxShadow "0 4px 14px rgba(0,0,0,.4)"
      , CSS.transition "box-shadow .25s ease, border-color .25s ease"
      ]
  , selector_ ".seat.acting .plate"
      [ "border-color" =: "rgba(232,199,125,.8)"
      , CSS.animation "actingPulse 1.4s ease-in-out infinite"
      ]
  , selector_ ".seat.winner .plate"
      [ "border-color" =: "rgba(120,220,160,.8)"
      , CSS.boxShadow "0 0 22px rgba(90,200,140,.45), 0 4px 14px rgba(0,0,0,.4)"
      ]
  , selector_ ".avatar"
      [ CSS.width "32px", CSS.height "32px"
      , CSS.borderRadius (pct 50)
      , CSS.display "grid"
      , "place-items" =: "center"
      , CSS.fontSize "17px"
      , CSS.background "radial-gradient(circle at 35% 30%, #3A4048, #22262C)"
      , CSS.border "1px solid rgba(232,199,125,.3)"
      , CSS.flexShrink 0
      ]
  , selector_ ".pinfo" [ CSS.display "flex", CSS.flexDirection "column", CSS.gap "1px" ]
  , selector_ ".pname"
      [ CSS.fontSize "12px"
      , CSS.fontWeight "600"
      , CSS.letterSpacing ".08em"
      , CSS.color ivory
      , CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.gap "5px"
      , CSS.whiteSpace "nowrap"
      ]
  , selector_ ".dbtn"
      [ CSS.width "14px", CSS.height "14px"
      , CSS.borderRadius (pct 50)
      , CSS.background goldGrad
      , CSS.color (RGB 36 26 8)
      , CSS.fontSize "9px"
      , CSS.fontWeight "800"
      , CSS.display "grid"
      , "place-items" =: "center"
      ]
  , selector_ ".pstack"
      [ CSS.fontSize "12px"
      , CSS.fontWeight "700"
      , CSS.color goldHi
      , CSS.letterSpacing ".06em"
      , "font-variant-numeric" =: "tabular-nums"
      , CSS.whiteSpace "nowrap"
      ]
  , selector_ ".bubble"
      [ CSS.position "absolute"
      , CSS.top "-27px"
      , CSS.background "rgba(6,10,9,.72)"
      , CSS.border "1px solid rgba(255,255,255,.18)"
      , CSS.color ivory
      , CSS.fontSize "10px"
      , CSS.fontWeight "700"
      , CSS.letterSpacing ".14em"
      , "text-transform" =: "uppercase"
      , CSS.padding "3px 9px"
      , CSS.borderRadius (px 999)
      , CSS.whiteSpace "nowrap"
      , CSS.animation "bubbleIn .25s cubic-bezier(.2,.9,.3,1.3) backwards"
      , CSS.zIndex 12
      , "font-variant-numeric" =: "tabular-nums"
      ]
  , selector_ ".winFloat"
      [ CSS.position "absolute"
      , CSS.top "-14px"
      , CSS.color (RGB 141 226 175)
      , CSS.fontWeight "800"
      , CSS.fontSize "16px"
      , "font-variant-numeric" =: "tabular-nums"
      , CSS.animation "floatUp 1.8s ease-out forwards"
      , CSS.pointerEvents "none"
      , CSS.zIndex 20
      , "text-shadow" =: "0 2px 8px rgba(0,0,0,.7)"
      ]
  -- action bar ------------------------------------------------------------------
  , selector_ ".abar"
      [ CSS.position "fixed"
      , CSS.left "50%"
      , CSS.bottom "calc(10px + env(safe-area-inset-bottom))"
      , CSS.transform "translateX(-50%)"
      , CSS.width "min(600px, 96vw)"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.gap "8px"
      , CSS.zIndex 60
      ]
  , selector_ ".abtns"
      [ CSS.display "flex", CSS.gap "8px" ]
  , selector_ ".btn"
      [ CSS.border "1px solid rgba(255,255,255,.14)"
      , CSS.background "rgba(255,255,255,.07)"
      , CSS.color ivory
      , CSS.borderRadius (px 13)
      , CSS.padding "13px 18px"
      , CSS.fontSize "14px"
      , CSS.fontWeight "700"
      , CSS.letterSpacing ".1em"
      , CSS.cursor "pointer"
      , CSS.backdropFilter "blur(10px)"
      , CSS.transition "transform .12s ease, box-shadow .2s ease, border-color .2s ease"
      , "touch-action" =: "manipulation"
      , CSS.whiteSpace "nowrap"
      ]
  , media_ (MediaQuery "(hover: hover)")
      [ rule_ ".btn:hover" [ CSS.transform "translateY(-1px)" ] ]
  , selector_ ".btn:active" [ CSS.transform "translateY(1px) scale(.99)" ]
  , selector_ ".btn.gold"
      [ CSS.background goldGrad
      , CSS.color (RGB 36 26 8)
      , CSS.border "1px solid rgba(255,244,214,.5)"
      , CSS.boxShadow "0 4px 18px rgba(201,164,92,.35)"
      ]
  , selector_ ".btn.ghost"
      [ CSS.background "rgba(255,255,255,.04)" ]
  , selector_ ".abtns .btn" [ CSS.flex "1", CSS.textAlign "center" ]
  , selector_ ".btn.afold"
      [ "border-color" =: "rgba(206,63,59,.5)"
      , CSS.color (RGB 235 158 152 )
      ]
  , selector_ ".btn.araise.open"
      [ CSS.boxShadow "0 0 0 2px rgba(232,199,125,.6), 0 4px 18px rgba(201,164,92,.35)" ]
  , selector_ ".kb"
      [ CSS.marginLeft "8px"
      , CSS.fontSize "10px"
      , CSS.opacity 0.55
      , CSS.border "1px solid currentColor"
      , CSS.borderRadius (px 4)
      , CSS.padding "1px 4px"
      , CSS.fontWeight "600"
      ]
  , media_ (MediaQuery "(hover: none)")
      [ rule_ ".kb" [ CSS.display "none" ] ]
  , selector_ ".tray"
      [ CSS.display "grid"
      , "grid-template-columns" =: "repeat(4, 1fr)"
      , CSS.gap "8px"
      , CSS.background glassBg
      , CSS.border "1px solid rgba(255,255,255,.12)"
      , CSS.borderRadius (px 16)
      , CSS.padding "12px"
      , CSS.animation "riseIn .2s ease backwards"
      , CSS.boxShadow "0 10px 30px rgba(0,0,0,.5)"
      ]
  , selector_ ".btn.pre"
      [ CSS.padding "9px 6px"
      , CSS.fontSize "12px"
      , CSS.textAlign "center"
      ]
  , selector_ ".btn.pre.sel"
      [ "border-color" =: "rgba(232,199,125,.7)", CSS.color goldHi ]
  , selector_ ".rslider"
      [ "grid-column" =: "1 / -1"
      , CSS.width "100%"
      , CSS.height "34px"
      , "accent-color" =: "#D9B26A"
      , CSS.cursor "pointer"
      , "touch-action" =: "manipulation"
      ]
  , selector_ ".btn.confirm"
      [ "grid-column" =: "1 / -1"
      , CSS.fontSize "15px"
      , "font-variant-numeric" =: "tabular-nums"
      ]
  -- winner banner ---------------------------------------------------------------
  -- anchored to the table, in the gap between the board and the hero's cards,
  -- so every card on the felt (and in the seats) stays readable
  , selector_ ".banner"
      [ CSS.position "absolute"
      , CSS.left "50%", CSS.top "66%"
      , CSS.transform "translate(-50%,-50%)"
      , CSS.maxWidth "62%"
      , CSS.zIndex 70
      ]
  , selector_ ".bpanel"
      [ CSS.background "rgba(9,11,14,.9)"
      , CSS.border "1px solid rgba(232,199,125,.4)"
      , CSS.borderRadius (px 18)
      , CSS.padding "18px 30px"
      , CSS.textAlign "center"
      , CSS.backdropFilter "blur(12px)"
      , CSS.boxShadow "0 20px 60px rgba(0,0,0,.6), 0 0 40px rgba(232,199,125,.12)"
      , CSS.animation "panelIn .4s cubic-bezier(.2,.9,.25,1.2) backwards"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.gap "10px"
      , CSS.alignItems "center"
      , CSS.maxWidth "100%"
      ]
  , selector_ ".bline" [ CSS.display "flex", CSS.flexDirection "column", CSS.gap "3px" ]
  , selector_ ".bwho"
      [ CSS.fontFamily "var(--serif)"
      , CSS.fontWeight "700"
      , CSS.fontSize "clamp(17px, 3.4vmin, 24px)"
      , CSS.color goldHi
      , CSS.letterSpacing ".06em"
      , "font-variant-numeric" =: "tabular-nums"
      ]
  , selector_ ".bhand"
      [ CSS.fontSize "12px"
      , CSS.letterSpacing ".2em"
      , "text-transform" =: "uppercase"
      , CSS.color mist
      ]
  , selector_ ".bbtns"
      [ CSS.display "flex"
      , CSS.gap "8px"
      , CSS.flexWrap "wrap"
      , CSS.justifyContent "center"
      , CSS.alignItems "center"
      , CSS.marginTop "4px"
      ]
  , selector_ ".btn.next" [ CSS.padding "10px 24px" ]
  , selector_ ".btn.peek" [ CSS.padding "10px 16px", CSS.fontSize "13px" ]
  -- a peeked muck stays greyed, but readable
  , selector_ ".peeking .seat.folded" [ CSS.opacity 0.75, CSS.filter "saturate(.7)" ]
  -- overlays -----------------------------------------------------------------
  , selector_ ".overlay"
      [ CSS.position "fixed"
      , "inset" =: "0"
      , CSS.background "rgba(5,6,8,.7)"
      , CSS.backdropFilter "blur(6px)"
      , CSS.display "flex"
      , CSS.alignItems "center"
      , CSS.justifyContent "center"
      , CSS.zIndex 100
      , CSS.animation "overlayIn .25s ease"
      , CSS.padding "16px"
      ]
  , selector_ ".panel"
      [ CSS.background glassBg
      , CSS.border "1px solid rgba(232,199,125,.35)"
      , CSS.borderRadius (px 20)
      , CSS.padding "28px 34px"
      , CSS.textAlign "center"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.gap "10px"
      , CSS.alignItems "center"
      , CSS.boxShadow "0 24px 80px rgba(0,0,0,.65)"
      , CSS.animation "panelIn .4s cubic-bezier(.2,.9,.25,1.2) backwards"
      , CSS.maxWidth "min(480px, 94vw)"
      , "max-height" =: "calc(100dvh - 40px)"
      , CSS.overflow "auto"
      ]
  , selector_ ".goEmoji" [ CSS.fontSize "54px", CSS.animation "stampIn .5s .15s cubic-bezier(.34,1.56,.64,1) backwards" ]
  , selector_ ".goTitle"
      [ CSS.fontFamily "var(--serif)"
      , CSS.fontWeight "700"
      , CSS.fontSize "38px"
      , CSS.letterSpacing ".14em"
      , CSS.color goldHi
      ]
  , selector_ ".goSub" [ CSS.color mist, CSS.fontSize "13px", CSS.letterSpacing ".1em" ]
  , selector_ ".statRow"
      [ CSS.display "flex"
      , CSS.justifyContent "space-between"
      , CSS.gap "60px"
      , CSS.width "100%"
      , CSS.fontSize "14px"
      , CSS.color mist
      , CSS.animation "riseIn .4s ease backwards"
      , "font-variant-numeric" =: "tabular-nums"
      ]
  , selector_ ".statRow b" [ CSS.color ivory ]
  -- help ----------------------------------------------------------------------
  , selector_ ".helpPanel"
      [ CSS.textAlign "left"
      , CSS.alignItems "stretch"
      , CSS.position "relative"
      , CSS.gap "4px"
      , CSS.maxWidth "min(560px, 94vw)"
      ]
  , selector_ ".helpClose"
      [ CSS.position "absolute"
      , CSS.top "12px", CSS.right "12px"
      , CSS.background "rgba(255,255,255,.07)"
      , CSS.border "1px solid rgba(255,255,255,.15)"
      , CSS.color ivory
      , CSS.width "32px", CSS.height "32px"
      , CSS.borderRadius (pct 50)
      , CSS.cursor "pointer"
      ]
  , selector_ ".helpH"
      [ CSS.fontFamily "var(--serif)"
      , CSS.fontWeight "700"
      , CSS.fontSize "26px"
      , CSS.letterSpacing ".1em"
      , CSS.color goldHi
      ]
  , selector_ ".helpSub"
      [ CSS.color mist, CSS.fontSize "12px", CSS.letterSpacing ".22em"
      , "text-transform" =: "uppercase", CSS.marginBottom "6px" ]
  , selector_ ".helpSec"
      [ CSS.color goldMid
      , CSS.fontSize "11px"
      , CSS.fontWeight "800"
      , CSS.letterSpacing ".24em"
      , CSS.marginTop "12px"
      ]
  , selector_ ".helpP"
      [ CSS.color (RGB 205 210 218)
      , CSS.fontSize "13.5px"
      , CSS.lineHeight "1.65"
      , CSS.margin "4px 0"
      ]
  -- title ----------------------------------------------------------------------
  , selector_ ".titleWrap"
      [ CSS.position "fixed"
      , "inset" =: "0"
      , CSS.display "flex"
      , CSS.flexDirection "column"
      , CSS.alignItems "center"
      , CSS.justifyContent "center"
      , CSS.gap "18px"
      , CSS.background roomBackground
      , CSS.overflow "hidden"
      , CSS.padding "20px"
      , CSS.textAlign "center"
      ]
  , selector_ ".titleSuits"
      [ CSS.color goldMid
      , CSS.letterSpacing ".8em"
      , "text-indent" =: ".8em"
      , CSS.fontSize "15px"
      , CSS.animation "riseIn .7s ease backwards"
      ]
  , selector_ ".titleH"
      [ CSS.margin "0"
      , CSS.fontFamily "var(--serif)"
      , CSS.fontWeight "700"
      , CSS.fontSize "clamp(34px, 9.5vmin, 88px)"
      , CSS.letterSpacing ".06em"
      , CSS.lineHeight "1.05"
      , "background" =: "linear-gradient(180deg, #F7E3AE 20%, #E8C77D 55%, #B98F44 100%)"
      , "-webkit-background-clip" =: "text"
      , "background-clip" =: "text"
      , CSS.color (RGBA 0 0 0 0)
      , "text-shadow" =: "0 1px 0 rgba(255,235,180,.15)"
      , CSS.animation "riseIn .7s .1s cubic-bezier(.2,.9,.25,1.2) backwards"
      ]
  , selector_ ".titleSub"
      [ CSS.color mist
      , CSS.letterSpacing ".4em"
      , "text-indent" =: ".4em"
      , CSS.fontSize "clamp(10px, 1.8vmin, 14px)"
      , CSS.animation "riseIn .7s .22s cubic-bezier(.2,.9,.25,1.2) backwards"
      ]
  , selector_ ".seatBtn"
      [ CSS.fontSize "17px"
      , CSS.padding "16px 44px"
      , CSS.marginTop "16px"
      , CSS.animation "riseIn .7s .34s cubic-bezier(.2,.9,.25,1.2) backwards"
      ]
  , selector_ ".howBtn"
      [ CSS.animation "riseIn .7s .44s cubic-bezier(.2,.9,.25,1.2) backwards" ]
  , selector_ ".titleHint"
      [ CSS.color (RGB 100 110 122)
      , CSS.fontSize "12px"
      , CSS.letterSpacing ".08em"
      , CSS.marginTop "10px"
      , CSS.animation "riseIn .7s .54s ease backwards"
      , CSS.maxWidth "90vw"
      ]
  , selector_ ".floatCard"
      [ CSS.position "absolute"
      , CSS.width "clamp(44px, 7vmin, 68px)"
      , "aspect-ratio" =: ".72"
      , CSS.background "linear-gradient(160deg, #FDFBF4 0%, #F1EBDC 100%)"
      , CSS.borderRadius (px 8)
      , CSS.color inkDark
      , CSS.fontWeight "700"
      , CSS.fontSize "clamp(15px, 2.4vmin, 22px)"
      , CSS.display "grid"
      , "place-items" =: "center"
      , CSS.boxShadow "0 10px 26px rgba(0,0,0,.5)"
      , CSS.opacity 0.85
      , CSS.animation "floaty 7s ease-in-out infinite"
      , CSS.pointerEvents "none"
      ]
  , selector_ ".floatCard.red" [ CSS.color crimson ]
  -- responsive: portrait phones -------------------------------------------------
  , media_ (screen_ `and_` maxWidth_ (px 740))
      [ rule_ ":root"
          [ "--tw" =: "min(96vw, calc((100dvh - 320px) * 1.15))"
          , "--th" =: "calc(var(--tw) / 0.62)"
          ]
      , rule_ ".room" [ "inset" =: "44px 0 150px 0" ]
      , rule_ ".felt" [ CSS.borderRadius "calc(var(--tw) / 2)" ]
      , rule_ ".feltRing" [ CSS.borderRadius "calc(var(--tw) / 2.3)", "inset" =: "5% 8%" ]
      , rule_ ".seat.s0" [ CSS.left "50%", CSS.top "98%" ]
      , rule_ ".seat.s1" [ CSS.left "16%", CSS.top "70%" ]
      , rule_ ".seat.s2" [ CSS.left "15%", CSS.top "29%" ]
      -- low enough that a revealed hand above the plate stays on screen
      , rule_ ".seat.s3" [ CSS.left "50%", CSS.top "9%" ]
      , rule_ ".seat.s4" [ CSS.left "85%", CSS.top "29%" ]
      , rule_ ".seat.s5" [ CSS.left "84%", CSS.top "70%" ]
      , rule_ ".betSpot.bs0" [ CSS.left "50%", CSS.top "80%" ]
      , rule_ ".betSpot.bs1" [ CSS.left "26%", CSS.top "63%" ]
      , rule_ ".betSpot.bs2" [ CSS.left "25%", CSS.top "34%" ]
      , rule_ ".betSpot.bs3" [ CSS.left "50%", CSS.top "16%" ]
      , rule_ ".betSpot.bs4" [ CSS.left "75%", CSS.top "34%" ]
      , rule_ ".betSpot.bs5" [ CSS.left "74%", CSS.top "63%" ]
      , rule_ ".boardRow" [ CSS.top "44%" ]
      , rule_ ".pot" [ CSS.top "57%", CSS.fontSize "12px" ]
      , rule_ ".feltLogo" [ CSS.top "68%", CSS.fontSize "calc(var(--tw)*.035)" ]
      , rule_ ":root" [ "--cw" =: "calc(var(--tw) * 0.155)" ]
      , rule_ ".seat .card" [ "--cw" =: "calc(var(--tw) * 0.1)" ]
      , rule_ ".seat.hero .card" [ "--cw" =: "calc(var(--tw) * 0.15)" ]
      , rule_ ".plate" [ CSS.minWidth "84px", CSS.padding "4px 9px 4px 5px" ]
      , rule_ ".avatar" [ CSS.width "26px", CSS.height "26px", CSS.fontSize "14px" ]
      , rule_ ".pname" [ CSS.fontSize "10px" ]
      , rule_ ".pstack" [ CSS.fontSize "11px" ]
      , rule_ ".btnLabel" [ CSS.display "none" ]
      , rule_ ".brand" [ CSS.display "none" ]
      , rule_ ".hudStats" [ CSS.fontSize "10px" ]
      , rule_ ".topbar" [ CSS.padding "6px 10px" ]
      , rule_ ".abar" [ CSS.width "calc(100vw - 12px)" ]
      , rule_ ".abtns .btn" [ CSS.padding "14px 6px", CSS.fontSize "13px" ]
      , rule_ ".bubble" [ CSS.fontSize "9px" ]
      -- keep clear of the side seats' plates and cards on either flank
      , rule_ ".banner" [ CSS.top "69%", CSS.maxWidth "52%" ]
      , rule_ ".bpanel" [ CSS.padding "14px 18px" ]
      ]
  -- responsive: short landscape ---------------------------------------------------
  , media_ (screen_ `and_` maxHeight_ (px 520))
      [ rule_ ":root"
          [ "--tw" =: "min(88vw, calc((100dvh - 150px) * 1.85))" ]
      , rule_ ".room" [ "inset" =: "40px 0 76px 0" ]
      , rule_ ".topbar" [ CSS.padding "4px 8px" ]
      , rule_ ".btnLabel" [ CSS.display "none" ]
      , rule_ ".abar"
          [ CSS.width "min(480px, 92vw)" ]
      , rule_ ".abtns .btn" [ CSS.padding "9px 6px", CSS.fontSize "12px" ]
      , rule_ ".tray" [ CSS.padding "8px" ]
      -- no room between board and hero here: park a slim bar under the table
      , rule_ ".banner"
          [ CSS.position "fixed"
          , CSS.left "50%", CSS.top "auto", CSS.bottom "6px"
          , CSS.transform "translateX(-50%)"
          , CSS.maxWidth "96vw"
          ]
      , rule_ ".bpanel"
          [ CSS.flexDirection "row"
          , CSS.flexWrap "wrap"
          , CSS.justifyContent "center"
          , CSS.alignItems "center"
          , CSS.padding "7px 14px"
          , CSS.gap "6px 16px"
          ]
      , rule_ ".bline"
          [ CSS.flexDirection "row", CSS.alignItems "baseline", CSS.gap "8px" ]
      , rule_ ".bwho" [ CSS.fontSize "15px" ]
      , rule_ ".bhand" [ CSS.fontSize "10px" ]
      , rule_ ".bbtns" [ CSS.marginTop "0" ]
      , rule_ ".btn.next" [ CSS.padding "7px 16px" ]
      , rule_ ".btn.peek" [ CSS.padding "7px 12px", CSS.fontSize "11px" ]
      ]
  -- reduced motion -----------------------------------------------------------------
  , media_ (MediaQuery "(prefers-reduced-motion: reduce)")
      [ rule_ "*"
          [ "animation-duration" =: ".01ms"
          , "animation-iteration-count" =: "1"
          , "transition-duration" =: ".01ms"
          ]
      ]
  -- keyframes ------------------------------------------------------------------
  , keyframes_ "dealIn"
      [ from_ [ CSS.transform "translateY(-18px) rotateY(80deg) scale(.92)", CSS.opacity 0 ]
      , at (pct 60) [ CSS.transform "translateY(2px) rotateY(0deg) scale(1.02)", CSS.opacity 1 ]
      , to_ [ CSS.transform "translateY(0) rotateY(0deg) scale(1)", CSS.opacity 1 ]
      ]
  , keyframes_ "bubbleIn"
      [ from_ [ CSS.transform "translateY(6px) scale(.7)", CSS.opacity 0 ]
      , to_ [ CSS.transform "translateY(0) scale(1)", CSS.opacity 1 ]
      ]
  , keyframes_ "betIn"
      [ from_ [ CSS.transform "translate(-50%,-90%) scale(.5)", CSS.opacity 0 ]
      , to_ [ CSS.transform "translate(-50%,-50%) scale(1)", CSS.opacity 1 ]
      ]
  , keyframes_ "potIn"
      [ from_ [ CSS.transform "translate(-50%,-50%) scale(.85)" ]
      , at (pct 60) [ CSS.transform "translate(-50%,-50%) scale(1.06)" ]
      , to_ [ CSS.transform "translate(-50%,-50%) scale(1)" ]
      ]
  , keyframes_ "actingPulse"
      [ from_ [ CSS.boxShadow "0 0 0 0 rgba(232,199,125,.55), 0 4px 14px rgba(0,0,0,.4)" ]
      , at (pct 60) [ CSS.boxShadow "0 0 0 9px rgba(232,199,125,0), 0 4px 14px rgba(0,0,0,.4)" ]
      , to_ [ CSS.boxShadow "0 0 0 0 rgba(232,199,125,0), 0 4px 14px rgba(0,0,0,.4)" ]
      ]
  , keyframes_ "floatUp"
      [ from_ [ CSS.transform "translateY(6px)", CSS.opacity 0 ]
      , at (pct 20) [ CSS.opacity 1 ]
      , to_ [ CSS.transform "translateY(-34px)", CSS.opacity 0 ]
      ]
  , keyframes_ "overlayIn" [ from_ [ CSS.opacity 0 ], to_ [ CSS.opacity 1 ] ]
  , keyframes_ "panelIn"
      [ from_ [ CSS.transform "translateY(22px) scale(.93)", CSS.opacity 0 ]
      , to_ [ CSS.transform "translateY(0) scale(1)", CSS.opacity 1 ]
      ]
  , keyframes_ "riseIn"
      [ from_ [ CSS.transform "translateY(16px)", CSS.opacity 0 ]
      , to_ [ CSS.transform "translateY(0)", CSS.opacity 1 ]
      ]
  , keyframes_ "stampIn"
      [ from_ [ CSS.transform "scale(2) rotate(-14deg)", CSS.opacity 0 ]
      , to_ [ CSS.transform "scale(1) rotate(0deg)", CSS.opacity 1 ]
      ]
  , keyframes_ "floaty"
      [ from_ [ CSS.transform "translateY(0) rotate(var(--fr, 0deg))" ]
      , at (pct 50) [ CSS.transform "translateY(-16px) rotate(var(--fr, 0deg))" ]
      , to_ [ CSS.transform "translateY(0) rotate(var(--fr, 0deg))" ]
      ]
  ]
