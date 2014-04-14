module Main where

import OpenNebula 

server_sara = "http://one-xmlrpc.calligo.sara.nl:2633/RPC2"
session_nikos = "envisage-nicolaosb:PUT-YOUR_PASSWORD"
proxy_cwi = Just "http://10.0.212.3:3128"

main = do
  (b, s, i) <- xmlrpc server_sara session_nikos proxy_cwi $ vm_info 32599
  (b', s', i') <- xmlrpc server_sara session_nikos proxy_cwi $ vmpool_info (-1) (-1) (-1) (-1)
  print s'
