-- | The OpenNebula's XML-RPC API calls implemented as RPC-monadic actions
module OpenNebula.API where

import Network.XmlRpc.Client
import Network.HTTP (HandleStream)
import Network.Browser
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
-- import Network.XmlRpc.THDeriveXmlRpcType
import qualified Data.ByteString.UTF8 as U

-- -- | Record type used to represent the struct in Haskell.
-- data Info = Info { suc :: Bool, info :: String, errCode :: Int } deriving Show
-- $(asXmlRpcStruct ''Info)

type RPC = ReaderT (String, String) (BrowserAction (HandleStream U.ByteString))

-- | Reads info about a VM
--
-- Input: one.vm.info $VM_ID
-- Output: Success, Info or ErrMsg, ErrCode
vm_info :: Int -> RPC (Bool, String, Int)
vm_info vmId = do
  (serv, sess) <- ask
  lift $ remote serv "one.vm.info" sess vmId

-- | Reads information about all the VMS associated with this nebula user
--
-- Input: one.vmpool.info 
-- Output: Success, Info or ErrMsg, ErrCode
vmpool_info :: Int -> Int -> Int -> Int -> RPC (Bool, String, Int)
vmpool_info filterFlag rangeStart rangeEnd filterState = do
  (serv, sess) <- ask
  lift $ remote serv "one.vmpool.info" sess filterFlag rangeStart rangeEnd filterState

-- | Allocates a new VM with a given template
--
-- Input: one.vm.allocate $TEMPLATE
-- Output: Success, NEW_VM_ID, ErrCode
vm_allocate :: String -> RPC (Bool, Int, Int)
vm_allocate templ = do
  (serv, sess) <- ask
  lift $ remote serv "one.vm.allocate" sess templ

-- | Runs a VM action command for a given VM
--
-- Input: one.vm.action "shutdown|cancel|reboot|..." $VM_ID
-- Output: Success, SAME_VM_ID, ErrCode
vm_action :: String -> Int -> RPC (Bool, Int, Int)
vm_action actionName vmId = do
  (serv, sess) <- ask
  lift $ remote serv "one.vm.action" sess actionName vmId
  
-- | Queries the template contents of a saved template belonging to the user
-- 
-- Input: one.template.info $TEMPLATE_ID
-- Ouput: Success, TemplateContents or ErrMsg, ErrCode
template_info :: Int -> RPC (Bool, String, Int)
template_info templId = do
  (serv, sess) <- ask
  lift $ remote serv "one.template.info" sess templId

-- | The runner of an RPC-action
--
-- It requires 
--
-- 1. the server address
-- 2. a session string i.e. "username:pass" 
-- 3. maybe a proxy to connect through the server
-- 4. the RPC action to call
--
-- Returns the result of the RPC call
xmlrpc :: String -> String -> Maybe String -> RPC a -> IO a
xmlrpc server session maybeProxy comms = browse $ do
                                           maybe (return ()) (\ proxy_server -> setProxy (Proxy proxy_server Nothing)) maybeProxy
                                           runReaderT comms (server, session)
