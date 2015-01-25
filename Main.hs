{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import Data.List
import Data.Random
import Data.Random.Extras (choice)
import qualified Data.ByteString.Char8 as B
import Network.SimpleIRC


onMessage :: EventFunc
onMessage server msg
   | B.pack "%rms" `B.isPrefixOf` mMsg msg = do
        r <- (sample . choice . lines) =<< readFile "puns.txt"
        sendMsg server chan (B.pack r)
   | otherwise = print msg
   where chan = fromJust $ mChan msg

events = [(Privmsg onMessage)]

server :: IrcConfig
server = (mkDefaultConfig "irc.physicsporn.org" "rms")
         { cChannels = ["#asozialesnetzwerk"]
         , cEvents   = events
         }

main = do
   connect server False True

