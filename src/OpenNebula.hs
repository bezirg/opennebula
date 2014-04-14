module OpenNebula where

import Network.XmlRpc.Client
import Network.HTTP (rspBody, HandleStream)
import Network.Browser
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
-- import Network.XmlRpc.THDeriveXmlRpcType
import qualified Data.ByteString.UTF8 as U

-- -- | Record type used to represent the struct in Haskell.
-- data Info = Info { suc :: Bool, info :: String, errCode :: Int } deriving Show
-- $(asXmlRpcStruct ''Info)

type RPC = ReaderT (String, String) (BrowserAction (HandleStream U.ByteString))

vm_info :: Int -> RPC (Bool, String, Int)
vm_info objectId = do
  (serv, sess) <- ask
  lift $ remote serv "one.vm.info" sess objectId

vmpool_info :: Int -> Int -> Int -> Int -> RPC (Bool, String, Int)
vmpool_info filterFlag rangeStart rangeEnd filterState = do
  (serv, sess) <- ask
  lift $ remote serv "one.vmpool.info" sess filterFlag rangeStart rangeEnd filterState

vm_allocate :: String -> RPC (Bool, Int, Int)
vm_allocate templ = do
  (serv, sess) <- ask
  lift $ remote serv "one.vm.allocate" sess templ

xmlrpc :: String -> String -> Maybe String -> RPC a -> IO a
xmlrpc server session maybeProxy comms = browse $ do
                                           maybe (return ()) (\ proxy_server -> setProxy (Proxy proxy_server Nothing)) maybeProxy
                                           runReaderT comms (server, session)

