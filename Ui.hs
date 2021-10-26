{-# Language RecordWildCards #-}
module Ui(
    Ui(..)
  , readUi
  , mixNoise
) where

import Csound.Base

import Csound.Control.Midi.LaunchKey
import qualified Csound.Control.Midi.LaunchKey as L (knob')

-- | UI for the midi controller.
data Ui = Ui
  { ui'drumKeys   :: Evt (D, Tab, Tab, Tab)
    -- ^ select drum beat patterns
  , ui'padKeys    :: Evt (D, D, Tab, Tab, Tab)
    -- ^ select pad patterns
  , ui'masterVol  :: Sig
    -- ^ master volume knob
  , ui'beatVol    :: Sig
    -- ^ drums volume
  , ui'percVol    :: Sig
    -- ^ percussion volume
  , ui'padVol     :: Sig
    -- ^ pad volume
  , ui'tickVol    :: Sig
    -- ^ metronome volume (only for rehearsals)
  , ui'arpVol     :: Sig
    -- ^ arpegiators volume
  , ui'noiseVol   :: Sig
    -- ^ environmental noises volume
  , ui'padMix     :: Sig
    -- ^ blend of two pads
  , ui'noiseMix   :: Sig
    -- ^ blend of four noises
  , ui'vcfFreq    :: Sig
    -- ^ Frequency of master Low-pass filter
  , ui'vcfRes     :: Sig
    -- ^ resonance of master Low-pass filter
  , ui'beatCuts   :: Sig
    -- ^ amount of random beat shuffle
  , ui'tanpuraVol :: Sig
    -- ^ volume of the tanpura sound
  , ui'kbdVol     :: Sig
    -- ^ volume of midi keyboard instruments
  , ui'extMetroVol :: Sig
    -- ^ external metronome
  , ui'intMetroVol :: Sig
    -- ^ internal metronome (sended to headphones, TODO)
  }

mchn = 9

-- Read midi messages
readUi :: SE Ui
readUi = do
  ui'drumKeys <- beatKey
  ui'padKeys  <- padKey
  ui'masterVol <- knb 1 0.5
  ui'padVol    <- knb 9  0.5
  ui'beatVol   <- knb 10 0.5
  ui'percVol   <- knb 11 0
  ui'tickVol   <- knb 7 0
  ui'arpVol    <- knb 12 0
  ui'noiseVol  <- knb 13 0.5
  ui'padMix    <- knb 2 1
  ui'noiseMix  <- knb 5 0
  ui'vcfFreq   <- knb 8 1
  ui'vcfRes    <- knb 16 0
  ui'beatCuts  <- knb 3 0
  ui'tanpuraVol <- knb 4 0
  ui'kbdVol     <- knb 15 0.5
  ui'extMetroVol <- knb 6 0
  ui'intMetroVol <- knb 14 0
  return Ui{..}
  where
    knb = L.knob' (LkChn mchn)

-- Select beats
beatKey :: SE (Evt (D, Tab, Tab, Tab))
beatKey = do
  arrUp <- arrowUpSig (LkChn mchn)
  arrDn <- arrowDownSig (LkChn mchn)
  let noDn          ev = filterE ( <=* 0.5) $ snapshot const arrDn ev
      filtArrExt    ev = noDn $ filterE ( ==* 1) $ snapshot const arrUp ev
      filtArrNormal ev = noDn $ filterE ( <=* 0.5) $ snapshot const arrUp ev
  tap <- fmap mconcat $ mapM (\idx -> fmap (fmap (const $ vals !! idx) . filtArrNormal)
                      $ tapBtn (LkChn mchn) (btns !! idx)) [0 .. 4]
  tapExt <- fmap mconcat $ mapM (\idx -> fmap (fmap (const $ valsExt !! idx) . filtArrExt)
                      $ tapBtn (LkChn mchn) (btnsExt !! idx)) [0 .. 1]
  return $ tap <> tapExt
  where
    vals =
      [ ins 80  "Beat-80.wav" "Perc-80.wav"   "metro-80.wav"
      , ins 85  "Beat-85.wav" "Perc-85.wav"   "metro-85.wav"
      , ins 90  "Beat-90.wav" "Perc-90.wav"   "metro-90.wav"
      , ins 100 "Beat-100.wav" "Perc-100.wav" "metro-100.wav"
      , ins 125 "Beat-125.wav" "Perc-125.wav" "metro-125.wav"
      ]

    btns = [1, 2, 3, 4, 5, 1, 2]

    valsExt =
      [ ins 77.5 "beat-7-8-159.wav" "perc-7-8-159.wav" "metro-7-8-159.wav"
      , ins 91 "beat-9-8-182.wav" "perc-9-8-182.wav"   "metro-9-8-182.wav"
      ]

    btnsExt = [1, 2]

    ins bpm name1 name2 name3 =
      (bpm, wavs ("Beats/" ++ name1) 0 WavAll, wavs ("Perc/" ++ name2) 0 WavAll, wavs ("Metros/" ++ name3) 0 WavAll)

-- | Select PADs
padKey :: SE (Evt (D, D, Tab, Tab, Tab))
padKey = do
  arrDn <- arrowDownSig (LkChn mchn)
  arrUp <- arrowUpSig (LkChn mchn)
  let noUp          ev = filterE ( <=* 0.5) $ snapshot const arrUp ev
  let filtArrExt    ev = noUp $ filterE ( ==* 1) $ snapshot const arrDn ev
      filtArrNormal ev = noUp $ filterE ( <=* 0.5) $ snapshot const arrDn ev
  tap <- fmap mconcat $ mapM (\x -> fmap (fmap (const $ vals !! x) . filtArrNormal) $ tapBtn (LkChn mchn) (nums !! x)) [0..2]
  tapExt <- fmap mconcat $ mapM (\x -> fmap (fmap (const $ valsExt !! x) . filtArrExt) $ tapBtn (LkChn mchn) (numsExt !! x)) [0..2]
  return $ tap <> tapExt
  where
    nums = [6, 7, 8]

    chans = [9, 9, 9]

    ins ix cps name1 name2 name3 = (ix, cps, wavs ("Pads/" ++ name1) 0 WavAll, wavs ("Pads/" ++ name2) 0 WavAll, wavs ("Tanpuras/" ++ name3) 0 WavAll)

    vals =
      [ ins 0 (cpspch 5.02) "D-Pad.wav" "D-Pad-2.wav" "glorian-tanpura-D-short-3.wav"
      , ins 1 (cpspch 5.04) "E-Pad.wav" "E-Pad-3.wav" "glorian-tanpura-E-short-3.wav"
      , ins 2 (cpspch 4.11) "B-Pad.wav" "B-Pad-2.wav" "glorian-tanpura-B-short-3.wav"
      ]

    valsExt =
      [ ins 3 (cpspch 4.10) "Eb-Pad.wav" "Eb-Pad-2.wav" "glorian-tanpura-Eb-short.wav"
      , ins 4 (cpspch 4.10) "F-Pad.wav" "F-Pad-2.wav" "glorian-tanpura-F-short.wav"
      , ins 5 (cpspch 4.10) "A-Pad.wav" "A-Pad-2.wav" "glorian-tanpura-A-short-3.wav"
      ]

    numsExt = [6, 7, 8]

-- | Mix four noise sources with a single knob.
-- We adjust them on a single line.
mixNoise :: Sig -> Sig2 -> Sig2 -> Sig2 -> Sig2 -> Sig2
mixNoise k = cfd4 x y
  where
    (x, y) = toSquare k

toSquare :: Sig -> Sig2
toSquare x =
  ifB (x <=* 0.25)
    (seq1 (x * 4))
    (ifB (x <=* 0.5)
      (seq2 (4 * (x - 0.25)))
      (ifB (x <=* 0.75)
        (seq3 (4 * (x - 0.5)))
        (seq4 (4 * (x - 0.75)))
      )
    )
  where
    seq1 a = (0, a)
    seq2 a = (a, 1)
    seq3 a = (1, 1 - a)
    seq4 a = (1 - a, 0)


