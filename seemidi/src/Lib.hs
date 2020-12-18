module Lib
    ( someFunc
    ) where

-- import qualified Codec.Wav ( importFile )
import Codec.Midi -- ( importFile )
import Data.Audio ( Audio(Audio) )
import Data.Array.Unboxed ( listArray, elems )
import Data.Int ( Int32 )
import Data.List ( null, maximum )
import Data.Maybe ( fromMaybe, catMaybes )
import Control.Monad ( forM_ )
import System.IO (FilePath)

someFunc :: IO ()
someFunc = main

-- filename = "../vrijdag.wav"
--                  
-- inMain :: FilePath -> IO ()
-- inMain path = do
--   maybeAudio <- Codec.Wav.importFile path
--   case maybeAudio :: Either String (Audio Int32) of
--     Left s -> putStrLn $ "wav decoding error: " ++ s
--     Right (Audio rate channels samples) -> do
--       putStrLn $ "rate = " ++ show rate
--       putStrLn $ "channels: " ++ show channels
--       -- print $ elems samples
 
midfile = "../cherrylips.mid"

checkMidi :: FilePath -> IO ()
checkMidi path = do
  maybeMidi <- importFile path
  case maybeMidi :: Either String Midi of
    Left s -> putStrLn $ "midi decoding error: " ++ s
    Right (Midi fileType timeDiv tracks) -> do
      putStrLn $ "fileType = " ++ show fileType
      putStrLn $ "timeDiv = " ++ show timeDiv
      --case timeDiv of
      --  TicksPerBeats i -> i
      --  TicksPerSecond framesPerSecond ticksPerFrame -> 
      forM_ tracks $ \track -> do
        --forM_ track $ \(ticks, msg) -> do
        --  putStrLn $ "ticks = " ++ show ticks
        let highestKey = maximumMaybe . catMaybes $ msgKey . snd <$> track
        case highestKey of
          Nothing -> return ()
          Just key -> do
            -- putStrLn $ "key = " ++ show key
            let note = keyNote key
            putStrLn $ "note = " ++ show note

keyNote :: Int -> String
keyNote key = tone ++ show octave
  where
    octave = (div key 12) - 2
    toneIdx = mod key 12
    tone = case toneIdx of
      0 -> "C"
      1 -> "C#"
      2 -> "D"
      3 -> "Eb"
      4 -> "E"
      5 -> "F"
      6 -> "F#"
      7 -> "G"
      8 -> "Ab"
      9 -> "A"
      10 -> "Bb"
      11 -> "B"

msgKey :: Message -> Maybe Key
msgKey msg = case msg of
  NoteOff channel key velocity -> Just key
  NoteOn channel key velocity -> Just key
  KeyPressure channel key pressure -> Just key
  _ -> Nothing

safeListCall :: Foldable t => (t a -> b) -> t a -> Maybe b
safeListCall f xs
  | Data.List.null xs = Nothing
  | otherwise = Just $ f xs

maximumMaybe :: (Ord a, Foldable t) => t a -> Maybe a
maximumMaybe = safeListCall Data.List.maximum


main = do
  -- putStrLn $ "* Printing the content of "++filename
  -- inMain filename
  checkMidi midfile

