{-# LANGUAGE OverloadedStrings #-}
module Main where

-- Various imports for Spock and stuff
import Web.Spock
import Web.Spock.Config

import Control.Monad.Trans
import Data.IORef
import qualified Data.Text as T

import LAC as L


import Data.Aeson
import           Data.Text        (Text, pack)

import Data.HashMap.Strict as HM (lookup)
import Control.Monad (forM, mzero)


-- "Live long and prosper - Spock"

-- Stuff to make Spock work (documentation with examples is awful) (ノಠ益ಠ)ノ彡┻━┻
data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

-- Initialise spock with defaults etc
main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg () PCNoDatabase ()
       runSpock 8080 (spock spockCfg app)

-- Add cors headers, doesn't work locally for me, but works on my server ¯\_(ツ)_/¯
corsHeader =
  do ctx <- getContext
     setHeader "Access-Control-Allow-Origin" "*"
     pure ctx

-- Provides the webserver routes, and adds the cors header (when it feels like it)
app :: Api
app =
    prehook corsHeader $
       do
           get root $
               text "POST a JSON formatted grid to /grid and it will return the solution (if any)"
           post "grid-solve" $ do -- Solve a grid via 'solve'
               theGrid <- jsonBody' :: ApiAction Grid   
               Web.Spock.json (L.solve theGrid)
           post "grid-steps" $ do -- Solve a grid via 'steps', should only call this route after checking steps first
               theGrid <- jsonBody' :: ApiAction Grid   
               Web.Spock.json (L.steps theGrid) 




-- Not that these are from Level.hs an all credit goes to Michael Gale for their creation
-- (ﾉ◕ヮ◕)ﾉ*:･ﾟ✧ ✧ﾟ･: *ヽ(◕ヮ◕ヽ) 
instance ToJSON Action where 
    toJSON (Add n) = object [ "action"  .= ("add" :: String)
                            , "operand" .= n 
                            ]
    toJSON (Sub n) = object [ "action"  .= ("sub" :: String)
                            , "operand" .= n
                            ]

instance FromJSON Action where 
    parseJSON = withObject "Action" $ \obj -> 
        case HM.lookup "action" obj of 
            Just (String "add") -> Add <$> obj .: "operand"
            Just (String "sub") -> Sub <$> obj .: "operand"
            _ -> mzero

instance ToJSON Cell where 
    toJSON (MkCell truth a) = object ["truth" .= truth, "act" .= toJSON a] -- My single change ( ͡° ͜ʖ ͡°)

instance FromJSON Cell where 
    parseJSON v = MkCell False <$> parseJSON v

instance ToJSON Row where 
    toJSON (MkRow t cs) = 
        object [ "target" .= t 
               , "cells"  .= cs 
               ]

instance FromJSON Row where 
    parseJSON = withObject "Row" $ \obj ->
        MkRow <$> obj .: "target" <*> obj .: "cells"

instance ToJSON Grid where 
    toJSON (MkGrid cts rows) = 
        object [ "columns" .= cts
               , "rows"    .= rows 
               ]

instance FromJSON Grid where 
    parseJSON = withObject "Grid" $ \obj ->
        MkGrid <$> obj .: "columns" <*> obj .: "rows"