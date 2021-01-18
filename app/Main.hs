{-# LANGUAGE OverloadedStrings #-}
module Main where

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




data MySession = EmptySession
data MyAppState = DummyAppState (IORef Int)

type Api = SpockM () () () ()
type ApiAction a = SpockAction () () () a

main :: IO ()
main =
    do ref <- newIORef 0
       spockCfg <- defaultSpockCfg () PCNoDatabase ()
       runSpock 8080 (spock spockCfg app)


corsHeader =
  do ctx <- getContext
     setHeader "Access-Control-Allow-Origin" "*"
     pure ctx

app :: Api
app =
    prehook corsHeader $
       do
           get root $
               text "POST a JSON formatted grid to /grid and it will return the solution (if any)"
           post "grid" $ do
               theGrid <- jsonBody' :: ApiAction Grid
               Web.Spock.json (returnSolvedGrid theGrid)




returnSolvedGrid :: Grid -> [Grid]
returnSolvedGrid grid = do
    let solvedGrid = L.solve grid
    if solvedGrid == [] then L.steps grid else solvedGrid


-- Not that these are from Level.hs an all credit goes to Michael Gale for their creation
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
    toJSON (MkCell truth a) = object ["truth" .= truth, "act" .= toJSON a]

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