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
   | B.pack "Amazon" `B.isInfixOf` mMsg msg = sendMsg server chan (B.append nick ": https://stallman.org/amazon.html")
   | B.pack "Skype" `B.isInfixOf` mMsg msg = sendMsg server chan (B.append nick ": https://stallman.org/skype.html")
   | B.pack "Google" `B.isInfixOf` mMsg msg = sendMsg server chan (B.append nick ": https://stallman.org/google.html")
   | B.pack "Apple" `B.isInfixOf` mMsg msg = sendMsg server chan (B.append nick ": https://stallman.org/apple.html")
   | B.pack "Facebook" `B.isInfixOf` mMsg msg = sendMsg server chan (B.append nick ": https://stallman.org/facebook.html")
   | B.pack "Uber" `B.isInfixOf` mMsg msg = sendMsg server chan (B.append nick ": https://stallman.org/uber.html")
   | B.pack "Netflix" `B.isInfixOf` mMsg msg = sendMsg server chan (B.append nick ": https://stallman.org/netflix.html")
   | B.pack "Harry Potter" `B.isInfixOf` mMsg msg = sendMsg server chan (B.append nick ": https://stallman.org/harry-potter.html")
   | B.pack " Linux" `B.isInfixOf` mMsg msg = do -- TODO: schoenere Loesung finden
        sendMsg server chan (B.append nick ": I'd just like to interject for a moment. What you\'re referring to as Linux, is in fact, GNU/Linux, or as I\'ve recently taken to calling it, GNU plus Linux. Linux is not an operating system unto itself, but rather another free component of a fully functioning GNU system made useful by the GNU corelibs, shell utilities and vital system components comprising a full OS as defined by POSIX.")
        sendMsg server chan (B.append nick ": Many computer users run a modified version of the GNU system every day, without realizing it. Through a peculiar turn of events, the version of GNU which is widely used today is often called \"Linux\", and many of its users are not aware that it is basically the GNU system, developed by the GNU Project. There really is a Linux, and these people are using it, but it is just a part of the system they use.")
        sendMsg server chan (B.append nick ": Linux is the kernel: the program in the system that allocates the machine\'s resources to the other programs that you run. The kernel is an essential part of an operating system, but useless by itself; it can only function in the context of a complete operating system. Linux is normally used in combination with the GNU operating system: the whole system is basically GNU with Linux added, or GNU/Linux.")
        sendMsg server chan (B.append nick ": All the so-called \"Linux\" distributions are really distributions of GNU/Linux.")
   | B.pack " Stallman" `B.isInfixOf` mMsg msg = do -- TODO: schoenere Loesung finden
        sendMsg server chan (B.append nick ": I'd just like to interject for a moment. What you\'re referring to as Stallman, is in fact, GNU/Stallman, or as I\'ve recently taken to calling it, GNU plus Stallman. Stallman is not an operating system unto itself, but rather another free component of a fully functioning GNU system made useful by the GNU corelibs, shell utilities and vital system components comprising a full OS as defined by POSIX.")
        sendMsg server chan (B.append nick ": Many computer users run a modified version of the GNU system every day, without realizing it. Through a peculiar turn of events, the version of GNU which is widely used today is often called \"Stallman\", and many of its users are not aware that it is basically the GNU system, developed by the GNU Project. There really is a Stallman, and these people are using it, but it is just a part of the system they use.")
        sendMsg server chan (B.append nick ": Stallman is the kernel: the program in the system that allocates the machine\'s resources to the other programs that you run. The kernel is an essential part of an operating system, but useless by itself; it can only function in the context of a complete operating system. Stallman is normally used in combination with the GNU operating system: the whole system is basically GNU with Stallman added, or GNU/Stallman.")
        sendMsg server chan (B.append nick ": All the so-called \"Stallman\" distributions are really distributions of GNU/Stallman.")
   | otherwise = print msg
   where chan = fromJust $ mChan msg
         nick = fromJust $ mNick msg

events = [(Privmsg onMessage)]

server :: IrcConfig
server = (mkDefaultConfig "irc.physicsporn.org" "rms")
         { cChannels = ["#asozialesnetzwerk"]
         , cEvents   = events
         }

main = do
   connect server False True

