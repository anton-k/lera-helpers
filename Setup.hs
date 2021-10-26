{-# Language RecordWildCards #-}
{-# Language TupleSections #-}
module Setup where

import Csound.Base
import Ui
import System.Directory
import System.FilePath
import Data.Time
import Data.Time.Format

import Csound.Catalog.Wave

setRawControllerMode :: Bool -> Options
setRawControllerMode val = def { csdFlags = def { midiIO = def { rawControllerMode = val }}}

type Save = String -> Sig2 -> SE Sig2

-- Where to save multitrack
mDir :: Maybe String
mDir = Nothing -- Just "rec"

main :: IO ()
main = do
  mFullDir <- mapM getFullDir mDir
  mapM_ createDirectory mFullDir
  let save name = maybe pure (\dir -> dumpWav (dir </> name)) mFullDir

  dacBy (setAlsa <> {-setJack "csd" <>-} setRates 44100 64 <> setRawControllerMode True <> setBufs 512 256 ) $ do
    ui <- readUi
    snd <- synt save ui
    sum
      [ pure snd
      , save "keys" =<< keyboard ui
      ]


getFullDir :: String -> IO String
getFullDir prefix =
  fmap ((prefix <>) . toFormat) getCurrentTime
  where
    toFormat = formatTime defaultTimeLocale "-%F-%T"


-----------------------------------------
-- keyboard


keyboard :: Ui -> SE Sig2
keyboard ui = hall 0.18 $ mul (ui'kbdVol ui) $ sum
  [ mul 0.4 $ midin 3 $ (onMsg $ fmap  fromMono . mul (leg 0.01 1 1 1.5) . polySynth)
  , mul 1 $ midin 2 $ (onMsg $ fromMono . mul (leg 0.1 1 1 0.3) . toneWheel)
  , mul 0.7 $ midin 4 $ (onMsg $ fromMono . mul (leg 0.0001 1 1 1) . vibraphone1)
  , mul 1 $ midin 5 $ (onMsg $ fromMono . mul (leg 0.0001 1 1 0.5) . xylophone)
  , mul 1 $ midin 6 $ (onMsg $ fromMono . mul (leg 0.5 1 1 1.5) . tibetan 9 0.015)
  , mul 1 $ midin 7 $ (onMsg $ fromMono . mul (leg 0.5 1 1 1.5) . nightPad 0.5)
  , mul 1 $ midin 13 $ (onMsg $ fmap fromMono . mul (leg 0.005 1 1 0.5) . amPiano)
  , mul 1 $ midin 14 $ (onMsg $ fmap fromMono . mul (leg 0.5 1 1 1.5) . padSharcOsc shEnglishHorn)
  , mul 1 $ midin 15 $ (onMsg $ fmap fromMono . mul (leg 0.15 1 1 0.5) . soloSharcOsc  shViola)
  , mul 1 $ midin 16 $ (onMsg $ fmap fromMono . mul (leg 0.15 1 1 1) . soloSharcOsc shOboe)
  ]
  where
    rev (al, ar) = freeverb al ar 0.9 0.75

-----------------------------------------

beatIns :: Sig -> Sig -> Sig -> Sig -> Sig -> Ref Sig -> Sig -> (D, Tab, Tab, Tab) -> SE (Sig2, Sig2, Sig2)
beatIns beatVol percVol tickVol metroIntVol metroExtVol arpRef beatCuts (bpm, beatTab, percTab, metroTab) = do
  baseNoteIndex <- readRef arpRef
  return $ (sum
        [ get k1 beatVol beatTab
        , mul tickVol $ tickSig
        ]
      , get k1 percVol percTab
      , mul (3 * metroExtVol) $ losc metroTab
      )
  where
    get k vol tb = toCuts bpm k $ mul vol $ losc tb

    k1 = evtToSig 0 $ freqOf [(1 - beatCuts, 0), (beatCuts, 1)] $ metro (0.25 * 90 / 120)
    k2 = evtToSig 0 $ freqOf [(1 - 0.75 * beatCuts, 0), (0.75 * beatCuts, 1)] $ metro (0.25 * 90 / 120)

    tickSig = fromMono $ ticks 4 (sig bpm)

padIns :: Sig -> (D, Tab, Tab) -> SE Sig2
padIns blend (cps, file1, file2) = do
  rndPad <- fmap kr $ rspline (-0.15) 0.15 0.07 0.25
  pure $ mul (linsegr [0, 0.2, 1] 0.5 0) $
  --  cfd (blend + rndPad) (loscCfd' (0, 18) 1 file1) (loscCfd' (0, 18) 1 file2)
    cfd blend (losc file1) (losc file2)

tanpuraIns :: Tab -> SE Sig2
tanpuraIns file = do
  cutTanp <- fmap kr $ rspline 0 1 0.07 0.25
  pure $ mul (1.5 * linsegr [0, 0.2, 1] 0.5 0) $ at (lp (2000 + 500 * cutTanp) 0.1) $
   -- loscCfd' (0, 20) 1 file
    losc file


nature :: Sig -> Sig2
nature k = mixNoise k
  (get "BIN - Airport_Atmosphere_II.wav")
  (get "BIN - Bird Symphony. Binaural Garden.wav")
  (get "DPA - Ocean Roar.wav")
  (get "ZH2 - Forest Surroundings.wav")
  where
    get name = ldisk (text $ "Nature/" ++ name)

-------------------------------------------------------

synt :: Save -> Ui -> SE Sig2
synt save ui@Ui{..} = do
  arpRef <- newGlobalCtrlRef 3
  (beat, perc, metr) <- schedStream 0 0  (beatIns ui'beatVol ui'percVol ui'tickVol ui'intMetroVol ui'extMetroVol arpRef ui'beatCuts) ui'drumKeys
  let padEvts  = fmap (\(_, c, x, y, _) -> (c, x, y)) ui'padKeys
      tanpEvts = fmap (\(_, _, _, _, t) -> t) ui'padKeys
  pad  <- schedStream 0   0.5 (padIns ui'padMix) padEvts
  tanp <- schedStream 0.2 0.5 tanpuraIns tanpEvts
  let nat = nature ui'noiseMix
  at (syntFx ui) $ mul (0.5 * ui'masterVol) $
    sum
      [ mul 2.7 $ save "beat" beat
      , save "perc" perc
      , save "click" metr
      , mul 0.5 $ mixAt 0.45 (echo 0.5 0.45 . echo 1 0.4)
            ( (mul 2.5 $ save "pad" $ mul ui'padVol pad)
            + (mul 1.5 $ save "tanp" $ mul ui'tanpuraVol tanp))
      , mul 0.8 $ save "noise" $ mul ui'noiseVol $ nat]

syntFx :: Ui -> Sig -> Sig
syntFx Ui{..} a =
  (\x -> zdf_2pole x (filtScale ui'vcfFreq) (0.5 + 20 * ui'vcfRes)) a
  -- (\x -> lowpass2 x (filtScale ui'vcfFreq) 1) a
  where
    filtScale x = 25 + 19000 * expcurve x 30

toCuts :: D -> Sig -> Sig2 -> Sig2
toCuts bpm compl (al, ar) = cfd compl (al, ar) (bbcuts al ar (4 * bpm / 120) 8 4 1 2)

----------------------------------------------------------------------------------



{- interesting idea to make instances of renderable for show
 - and have specific instances for our cases. and we can do not write dac all the time in ghci
 -
instance RenderCsd Int where
  renderCsdBy _ n = print 1 >> return (show n)
  csdArity _ = CsdArity 0 0
-}


b1 = wavAll "Beats/Beat-90.wav"
b2 = wavAll "Beats/Beat-100.wav"
p1 = wavAll "Pads/E-Pad.wav"
p2 = wavAll "Pads/D-Pad.wav"

{-
    arps baseNoteIndex = atTuple
      [ toArp "Arps/D-Arp-90.wav"
      , toArp "Arps/E-Arp-90.wav"
      , toArp "Arps/B-Arp-90.wav"
      , 0
      ]
      baseNoteIndex

    toArp name = (chn WavLeft, chn WavRight)
      where
        chn x = mincer (len * phasor (speed / len)) arpVol 1 (wavs name 0 x) 1
        len = sig $ filelen $ text name
        speed = sig bpm / 90
-}

bass :: Sig -> Sig -> Sig -> Sig -> SE Sig2
bass basVol bpm pulse base = pure $ fromMono bas1
  where
    bas1 =
      let cps = constSeq (fmap (base * ) [1, 9/8, 6/5, 9/8, 3/2, 8/9, 6/5, 1]) (bpm / 120)
      in  mul (basVol* 4) $ {-at (mlp (80 + 270 * pulse) 0.2)-} at (mlp (20 + 130 * uosc' (-0.25) (8 * bpm / 120)) 0.3) $ saw cps + sqr (cps * cent 15)
    dt = constSeq [2, 4, 2] (bpm / 120) * bpm / 120
