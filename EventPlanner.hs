{-# LANGUAGE OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
-- All comments are original, commented out code is original code we were testing


module Main where
import GHC.Generics
--import Data.Text.Lazy (Text)
-- all our crazy imports to make this work (some unused due to lack of "setNewEvent" full functionality)
import qualified Data.Text.Lazy.IO as I
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Internal  as BS
import qualified Data.ByteString as WOW
import qualified Data.Maybe (maybeToList)
import Data.Aeson.Text (encodeToLazyText)
import Control.Applicative
import Control.Monad
import Data.Aeson (ToJSON)
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.HashMap.Strict as HM
import Data.Text
import qualified Data.Vector as V
import System.Environment
import System.IO
import Data.List.Split
import qualified Data.Maybe as M



-- Our type declarations
type Date = Text
type Time = Text
type Thing = Text

-- Our Data types
data Event = Event { date :: Date
                     , time :: Time
                     , thing :: Thing
                     } deriving (Show, Generic, ToJSON)

type EventMap = HM.HashMap Date Event --Based on an outside resource, but adapted to our type
newtype EventBook = Events EventMap
                      deriving Show

-- JSON decoding, Outside resource, edited for our data type
instance FromJSON Event where
  parseJSON (Object v) = Event <$> v .: "date" <*> v .: "time" <*> v .: "thing" --if we get an object (Event) convert it to our data type
  parseJSON _ = mzero -- handles error
--Outside resource, edited for our data type
instance FromJSON EventBook where
  parseJSON (Array v) = do
    map <- V.foldl insertEvents (return HM.empty) v --using foldl, map the array of events to a new empty hashmap
    return $ Events map
    where
      insertEvents :: Parser EventMap -> Value -> Parser EventMap
      insertEvents m pObj = do
        event <- (return pObj >>= parseJSON) :: Parser (Maybe Event) --event becomes the Maybe Event returned from parsing on the pObj
        hmap <- m --reference to m is preserved in hmap
        case event of
         Just p -> return $ HM.insert (date p) p hmap --return the updated hash map of events
         Nothing -> m --return has map we originally started with
  parseJSON _ = mzero -- handles error

-- The function call from within main that we wrote to edit the file with a new data type
setNewEvent a b c = do
  x <- L.readFile "events.json"
  let list = decode x :: Maybe [Event] -- create a list of of events currently in the file by making Events using our data constructor
  let temp = newEvent (pack a) (pack b) (pack c) -- the new event being created through the newEvent function
  let out = list : [temp] --Append the new Event to the list of already existing events
  --  writeFile "events.json" "[" -- was giving us an error since the file is still technically open and therefore "locked"
  --save out -- This method fails because of the file being locked, commented out for demonstration purposes
  return out




-- Creating our New Event
newEvent :: Text -> Text -> Text -> (Maybe [Event])
newEvent "" _ _ = Nothing --Empty input check
newEvent a b c = Just [Event {date = a, time = b, thing = c}] --Creates the new Event

-- An attempt at updating the file, which we later scraped for another version

--setNewEvent = do
  --previous <- I.readFile "events.json"
  --show previous
  --I.appendFile "events.json" (encodeToLazyText newEvent)
{-get :: IO ()
get = do
  temp <- readFile "events.json"
  book <- return (decode $ C.pack temp :: Maybe EventBook)
  Prelude.concat . Data.Maybe.maybeToList $ decode book
-}

--setNewEvent = L.appendFile "events.json" $ encodePretty (newEvent) `L.snoc` BS.c2w '\n'
--testThis = do
    --temp <- readFile "events.json"
--save:: (Maybe [Event]) -> (FilePath -> String)
{-
save [Nothing] = appendFile "events.json" "]"
save [] = appendFile "events.json" "]"
save (x:xs) = do
  appendFile "events.json" (showAux (x :: Maybe [Event]))
  where
    showAux x = do
      let y = show x

  if (Prelude.length xs) >= 1
  then do
    appendFile "events.json" ","
    save xs
  else save xs
-}
-- Program execution (Outside Resource, edited)
main :: IO () -- IO declaration means main produces an "action" that will be executed
main = do
  json <- readFile "events.json" --read in the file we want to reference
  book <- return (decode $ C.pack json :: Maybe EventBook) --Edited to match our data type
  case book of --expect to be getting Events back from book
   Just (Events eventbook) -> putStrLn "Please type a date to search for. Using 'exit' will quit the program." >> queryLoop eventbook ""
   _ -> putStrLn "Could not parse event book json correctly." -- if we DONT get Events back, throw an error
  return () --returns the result of these actions

queryLoop :: EventMap
          -> Date
          -> IO ()
queryLoop book date
  | date == "exit" = return () --when a user types "exit" we want to be done, so return
  -- | date == "new" = setNewEvent --Where we call the function to add our new date
  | date == "" = getQuery >>= queryLoop book --if they type nothing, just reprompt for input
  | otherwise = do
      event <- return $ HM.lookup date book --using out hash map of Dates, look up this given event using its date
      case event of
       Just p -> do --if we get an event for this date back, do the following
                 putStrLn $ (show date) ++ " found:"
                 putStrLn $ "  Date: " ++ (show date) -- Edited to reflect our data type
                 putStrLn $ "  Time: " ++ (show $ time p) --Edited to relfect our data type
                 putStrLn $"  Event: " ++ (show $ thing p) --Added to reflect our new data type
       Nothing -> putStrLn "Event not found." --if we dont get an event back for the given date, say so
      queryLoop book "" --this prompts for another input
  where getQuery :: IO Text
        getQuery = putStr "query> " >> hFlush stdout >> getLine >>= return . pack --get the desired info from the file, and then pack the info
        -- which makes it of the type Text
