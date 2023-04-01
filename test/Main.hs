{-
ToDo:

Check parsing and serialization of single MIDI messages.
-}
module Main where

import qualified Example
import qualified Parser

import qualified Test.QuickCheck as QC
import Common (check, )

import qualified Sound.MIDI.File      as MidiFile
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File.Save as Save
import qualified Sound.MIDI.File.Event as Event

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.Parser.Report as Report

import qualified Data.EventList.Relative.TimeBody as EventList

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.List as List
import qualified Data.List.HT as ListHT
import qualified Data.List.Match as Match
import Data.Int (Int64, )
import Data.Ord.HT (comparing, )

import Control.Monad (liftM, when, )



testMidiName :: FilePath
testMidiName = "quickcheck-test.mid"

runExample :: MidiFile.T s -> IO ()
runExample example =
   let bin    = Save.toByteString example
       struct = Load.maybeFromByteString bin
       report = Report.Cons [] (Right example)
   in  BL.writeFile testMidiName bin >>
       print (struct == report) >>
       when (struct/=report)
          (print struct >> print report)

-- provoke a test failure in order to see some examples of Arbitrary MIDI files
checkArbitrary :: MidiFile.T s -> Bool
checkArbitrary (MidiFile.Cons _typ _division tracks) =
   all ((< 10) . length . EventList.toPairList) tracks


{- |
Increase the probability of similar adjacent messages
that can be compressed using the running status.
-}
newtype SortedFile = SortedFile {getSortedFile :: MidiFile.T B.ByteString}
   deriving Show

sortedFile :: MidiFile.T s -> SortedFile
sortedFile =
   SortedFile .
   MidiFile.mapTrack
      (EventList.fromPairList .
       List.sortBy (comparing snd) . EventList.toPairList)

instance QC.Arbitrary SortedFile where
   arbitrary = liftM sortedFile QC.arbitrary
   shrink = map sortedFile . QC.shrink . getSortedFile


saveLoadByteString :: MidiFile.T s -> Bool
saveLoadByteString midi =
   let bin    = Save.toByteString midi
       struct = Load.maybeFromByteString bin
       report = Report.Cons [] (Right midi)
   in  struct == report

saveLoadCompressedByteString :: MidiFile.T s -> Bool
saveLoadCompressedByteString midi =
   let bin    = Save.toCompressedByteString midi
       struct = Load.maybeFromByteString bin
       report = Report.Cons [] (Right (MidiFile.implicitNoteOff midi))
   in  struct == report

compressionShortens :: MidiFile.T s -> Bool
compressionShortens midi =
   BL.length (Save.toByteString midi)
   >=
   BL.length (Save.toCompressedByteString midi)


{-
This does not cover all cases of possible running status compression,
but the most common ones.
-}
equalStatus :: Event.T s -> Event.T s -> Bool
equalStatus x y =
   case (Event.maybeVoice x, Event.maybeVoice y) of
      (Just (ch0, ev0), Just (ch1, ev1)) ->
         ch0 == ch1
         &&
         case (ev0, ev1) of
            (VoiceMsg.Control _ _, VoiceMsg.Control _ _) -> True
            _ ->
               VoiceMsg.isNoteOn ev0 && VoiceMsg.isNoteOn ev1
               ||
               VoiceMsg.isNoteOff ev0 && VoiceMsg.isNoteOff ev1
      _ -> False

{-
You may test manually with Example.status
which is definitely compressible.
-}
compressible :: MidiFile.T s -> Bool
compressible =
   any (or . ListHT.mapAdjacent equalStatus . EventList.getBodies) .
   MidiFile.getTracks . MidiFile.implicitNoteOff

compressionStrictlyShortens :: MidiFile.T s -> QC.Property
compressionStrictlyShortens midi =
   compressible midi
   QC.==>
   BL.length (Save.toByteString midi)
   >
   BL.length (Save.toCompressedByteString midi)

saveLoadMaybeByteList :: MidiFile.T s -> Bool
saveLoadMaybeByteList midi =
   let bin    = Save.toByteList midi
       struct = Load.maybeFromByteList bin
       report = Report.Cons [] (Right midi)
   in  struct == report

saveLoadByteList :: MidiFile.T B.ByteString -> Bool
saveLoadByteList midi =
   midi == Load.fromByteList (Save.toByteList midi)


saveLoadFile :: MidiFile.T B.ByteString -> IO Bool
saveLoadFile midi =
   do Save.toSeekableFile testMidiName midi
      struct <- Load.fromFile testMidiName
      return $ struct == midi


loadSaveByteString :: MidiFile.T s -> Bool
loadSaveByteString midi0 =
   let bin0 = Save.toByteString midi0
   in  case Load.maybeFromByteString bin0 of
          Report.Cons [] (Right midi1) ->
               bin0 == Save.toByteString midi1
          _ -> False

loadSaveCompressedByteString :: MidiFile.T s -> Bool
loadSaveCompressedByteString midi0 =
   let bin0 = Save.toCompressedByteString midi0
   in  case Load.maybeFromByteString bin0 of
          Report.Cons [] (Right midi1) ->
               bin0 == Save.toCompressedByteString midi1
          _ -> False

loadSaveByteList :: MidiFile.T s -> Bool
loadSaveByteList midi0 =
   let bin0 = Save.toByteList midi0
   in  case Load.maybeFromByteList bin0 of
          Report.Cons [] (Right midi1) ->
               bin0 == Save.toByteList midi1
          _ -> False


restrictionByteList :: MidiFile.T s -> Bool
restrictionByteList midi =
   let bin = Save.toByteList midi
   in  Load.fromByteList bin ==
       Load.fromByteList (bin++[undefined])


lazinessByteList :: MidiFile.T s -> Bool
lazinessByteList (MidiFile.Cons typ divsn tracks00) =
   let tracks0 = filter (not . EventList.null) tracks00
       bin0 = Save.toByteList (MidiFile.Cons typ divsn tracks0)
       {- remove trailing EndOfTrack and its time stamp and replace the last by 
       bin1 = take (length bin0 - 5) bin0 ++ [undefined]
       -}
       bin1 = init bin0 ++ [undefined]
       (MidiFile.Cons _ _ tracks1) = Load.fromByteList bin1
   in  case ListHT.viewR tracks0 of
          Just (initTracks0, lastTrack0) ->
             List.isPrefixOf initTracks0 tracks1 &&
               let (lastTrack1:_) = Match.drop initTracks0 tracks1
               in  List.isPrefixOf
                      (init (EventList.toPairList lastTrack0))
                      (EventList.toPairList lastTrack1)
{-
              fmap fst (EventList.viewR lastTrack0) ==
              fmap fst (EventList.viewR lastTrack1)
-}
          _ -> True


{- |
Check whether corruptions in a file are properly detected
and do not trap into errors.

Sometimes it may yield an error which is @binary@'s fault.
See 'readAfterEnd'.
-}
corruptionByteString :: Int64 -> Int -> MidiFile.T B.ByteString -> Bool
corruptionByteString pos replacement midi =
   let bin = Save.toByteString midi
       n = mod pos (BL.length bin + 1)
       (pre, post) = BL.splitAt n bin
       replaceByte = fromIntegral replacement
       corruptBin =
          BL.append pre
             (if BL.null post
                then BL.singleton replaceByte
                else BL.cons replaceByte (BL.tail post))
   in  -- trace (show (BL.unpack corruptBin)) $
       case Load.maybeFromByteString corruptBin of
          Report.Cons _ _ -> True

corruptionByteList :: Int -> Int -> MidiFile.T B.ByteString -> Bool
corruptionByteList pos replacement midi =
   let bin = Save.toByteList midi
       n = mod pos (length bin + 1)
       (pre, post) = splitAt n bin
       corruptBin =
          pre ++ fromIntegral replacement :
             if null post then [] else tail post
   in  case Load.maybeFromByteList corruptBin of
          Report.Cons _ _ -> True

{- |
This demonstrates a problem of the @binary@ package.
If the binary @Get@ parser reads past the end of the file,
then @runGet@ yields an error not a parser failure.
This example provokes the problem by setting the @MTrk@ length to 0,
which makes the following data look like an alien chunk with large size.
When skipping following data of this size, 'runGet' yields an 'error'.
-}
readAfterEnd :: IO ()
readAfterEnd = do
   let bin = Save.toByteString $ Example.readAfterEnd
       fails =
          filter
             (\pos -> not $ corruptionByteString pos 0 Example.readAfterEnd)
             [0 .. BL.length bin]
   when (not $ null fails) $
      putStr $ " fails at positions " ++ show fails
   putStrLn ""


main :: IO ()
main = do
   runExample Example.empty
   runExample Example.meta
   runExample Example.status
   saveLoadFile Example.status >>= print
   check "saveLoadByteString" saveLoadByteString
   check "saveLoadCompressedByteString" saveLoadCompressedByteString
   check "saveLoadCompressedByteString sorted"
      (saveLoadCompressedByteString . getSortedFile)
   check "saveLoadMaybeByteList" saveLoadMaybeByteList
   check "saveLoadByteList" saveLoadByteList
--   check "saveLoadFile" saveLoadFile
   check "loadSaveByteString" loadSaveByteString
   check "loadSaveCompressedByteString" loadSaveCompressedByteString
   check "loadSaveCompressedByteString sorted"
      (loadSaveCompressedByteString . getSortedFile)
   check "loadSaveByteList" loadSaveByteList

   check "compressionShortens" compressionShortens
   check "compressionStrictlyShortens"
      (compressionStrictlyShortens . getSortedFile)

   check "restrictionByteList" restrictionByteList

   check "lazinessZeroOrMoreByteList" Parser.lazinessZeroOrMoreByteList
   check "lazinessByteList" lazinessByteList

   check "corruptionByteList" corruptionByteList
   check "corruptionByteString" corruptionByteString
   when False $ putStr "readAfterEnd" >> readAfterEnd

{-
laziness test:
The following expressions should return the prefix of the track before running into "undefined".
I don't know, how to formalize that.

Load.fromByteList [77,84,104,100,0,0,0,6,0,1,0,1,0,10,77,84,114,107,0,0,0,28,0,147,20,64,4,147,24,64,4,147,27,64,7,131,20,64,4,131,24,64,4,131,27,64,0,255,47,undefined]

Report.result $ StreamParser.runIncomplete Load.getTrackChunk $ StreamParser.ByteList [77,84,114,107,0,0,0,28,0,147,20,64,4,147,24,64,4,147,27,64,7,131,20,64,4,131,24,64,4,131,27,64,0,255,47,undefined]
-}
