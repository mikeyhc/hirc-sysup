module Main where

import Control.Monad (when)
import Control.Monad.Trans (lift)
import HarkerIRC.Client
import System.Process (readProcess)

main = runPlugin "sysup" "0.1.0.0" sysup runHarkerClient

sysup :: HarkerClient ()
sysup = do
    msg <- getMsg
    if msg == "!help"           then help
    else if msg == "!syshealth" then syshealthmsg
    else when (msg == "!sysup")      sysupmsg

help :: HarkerClient ()
help = sendReply "!sysup:     shows the current system uptime"
    >> sendReply "!syshealth: shows the current system load"

sysupmsg :: HarkerClient ()
sysupmsg = lift (readProcess "uptime" [] []) >>= sendReply . getTime
  where
    getTime = takeWhile (/= ',') . dropHead
    dropHead = dropAndTail (/= ' ') . dropAndTail (/= ' ') . tail
    dropAndTail x = tail . dropWhile x

syshealthmsg :: HarkerClient ()
syshealthmsg = lift (readProcess "uptime" [] []) >>= sendReply . getHealth
  where
    getHealth = tail . reverse . tail . takeWhile (/= ':') . reverse
