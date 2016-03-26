module Main where

import System.Environment (getArgs)

import SendMessage

main :: IO ()
main = do
  args <- getArgs
  case args of
   [botAccount, botPassword, server, targetAccount] -> sendMessage botAccount botPassword server Nothing targetAccount "notification"
   _ -> error $ "usage: <botAccount> <botPassword> <server> <targetAccount>, e.g. name@server.dom pass server.dom to@server.dom"
