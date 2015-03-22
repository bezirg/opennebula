{-# LANGUAGE LambdaCase #-}
module OpenNebula.Template 
    (
     PlainTemplate (..), PlainTemplateEntry (..), -- the PlainTemplate datatypes
     templateVmId,
     stripXmlTemplate, 
     xmlToPlainTemplate,
     cloneSlaveTemplate
    ) where

import Text.XML.Light as XML
import qualified Text.XML.Light.Cursor as Z
import Data.List (intersperse)
import qualified Data.Binary as Bin
import qualified Codec.Binary.Base64.String as Base64 (decode)

-- | Given an XML-Base64-encoded Nebula template as a string,
-- it returns (if there is one) the ID of the VM associated/instantiated by this template,
templateVmId :: String -> Maybe Int
templateVmId xmlStr = do
  let xmlDec = Base64.decode xmlStr
  vmElem <- parseXMLDoc xmlDec
  vmIdElem <- findChild (QName "ID" Nothing Nothing) vmElem
  return $ read $ strContent vmIdElem

-- | Parses an XML-Nebula template given (as a string)
-- After parsing, it strips data that are specific
-- to node, and leaves the template generic.
-- The stripped data will be automatically filled up by opennebula.
-- *NOTE*: the CONTEXT is stripped off before passed to $VM_TEMPLATE context variable.
stripXmlTemplate :: String ->  Maybe XML.Element
stripXmlTemplate xml_str = do
  doc <- parseXMLDoc xml_str
  templElem <- findElement (QName "TEMPLATE" Nothing Nothing) doc
  cleanedTempl <- cleanupTemplate templElem
  return cleanedTempl
         where
    cleanupTemplate :: XML.Element -> Maybe XML.Element
    cleanupTemplate templElem = do
      let c = Z.fromElement templElem
      -- remove NAME
      stop <- Z.removeGoUp =<< Z.findChild (\case 
                                           Z.Cur {Z.current = Elem (XML.Element (QName "NAME" _ _) _ _ _)} -> True
                                           _ -> False) c
      -- remove TEMPLATE_ID
      stop <- Z.removeGoUp =<< Z.findChild (\case 
                                           Z.Cur {Z.current = Elem (XML.Element (QName "TEMPLATE_ID" _ _) _ _ _)} -> True
                                           _ -> False) (Z.root stop)
      -- removed VMID
      stop <- Z.removeGoUp =<< Z.findChild (\case 
                                           Z.Cur {Z.current = Elem (XML.Element (QName "VMID" _ _) _ _ _)} -> True
                                           _ -> False) (Z.root stop)

      -- remove NIC[IP+MAC]
      nic <- Z.findChild (\case 
                          Z.Cur {Z.current = Elem (XML.Element (QName "NIC" _ _) _ _ _)} -> True
                          _ -> False) (Z.root stop)
      ip_or_mac <- Z.findChild (\case 
                          Z.Cur {Z.current = Elem (XML.Element (QName ipOrMac  _ _) _ _ _)} -> ipOrMac `elem` ["IP", "MAC"]
                          _ -> False) nic
      nic <- Z.removeGoUp ip_or_mac
      ip_or_mac <- Z.findChild (\case 
                                Z.Cur {Z.current = Elem (XML.Element (QName ipOrMac _ _) _ _ _)} -> ipOrMac `elem` ["IP", "MAC"]
                                _ -> False) nic
      stop <- Z.removeGoUp ip_or_mac
      --remove NIC[IP+MAC]
      nic <- Z.findRight (\case 
                          Z.Cur {Z.current = Elem (XML.Element (QName "NIC" _ _) _ _ _)} -> True
                          _ -> False) (stop)
      ip_or_mac <- Z.findChild (\case 
                          Z.Cur {Z.current = Elem (XML.Element (QName ipOrMac  _ _) _ _ _)} -> ipOrMac `elem` ["IP", "MAC"]
                          _ -> False) nic
      nic <- Z.removeGoUp ip_or_mac
      ip_or_mac <- Z.findChild (\case 
                                Z.Cur {Z.current = Elem (XML.Element (QName ipOrMac _ _) _ _ _)} -> ipOrMac `elem` ["IP", "MAC"]
                                _ -> False) nic
      stop <- Z.removeGoUp ip_or_mac
      return $ case Z.toTree (Z.root stop) of
                 Elem e -> e
                 _ -> error "transforming xml failed"



-- | Converting XML to Plain Template syntax
xmlToPlainTemplate :: XML.Element -> PlainTemplate
xmlToPlainTemplate = pTemplate
  where
    pTemplate :: XML.Element -> PlainTemplate
    pTemplate (XML.Element (QName "TEMPLATE" _ _) _attrs content _line ) = PlainTemplate (map pContent content)
    pTemplate _ = error "conversion to plain template failed"

    pElement :: XML.Element -> PlainTemplateEntry
    pElement (XML.Element (QName name _ _) _attrs [Text (CData _kind str _)] _line ) = SingleEntry name ('"' : str ++ "\"")
    pElement (XML.Element (QName name _ _) _attrs content _line) = ManyEntry name (map pContent content)

    pContent :: XML.Content -> PlainTemplateEntry
    pContent (XML.Elem e) = pElement e
    pContent _ = error "conversion to plain template failed"

-- | The root node of the PlainTemplate Datatype
newtype PlainTemplate = PlainTemplate { entries :: [PlainTemplateEntry] } 

-- | The attributes of each sub-node
data PlainTemplateEntry = SingleEntry String String
                        | ManyEntry String [PlainTemplateEntry]

-- Converting OneTemplate back to String
----------------------------------------
instance Show PlainTemplate where
    show (PlainTemplate es) = unlines $ map show es -- insert newline after each entry, no comma

instance Show PlainTemplateEntry where
    show (SingleEntry key value) = key ++ "=" ++ value
    show (ManyEntry key values) = key ++ "= [" ++ unlines (intersperse "," $ map show values) ++ "]"

-- | Given a XML-Base64-encoded template, it creates an inherited template
-- with the cpu, memory and CreatorPid specifications altered.
-- The cloned template is returned in plain format, so it can be used with old versions of OpenNebula =< 3.2)
cloneSlaveTemplate :: Bin.Binary a => String -> Int -> Int -> a -> String -> String -> String -> Maybe String
cloneSlaveTemplate templ newCpu newMem fromPid rpcServer rpcProxy session = do
  let templDec = Base64.decode templ
  xml <- stripXmlTemplate templDec
  xml' <- addSlaveContext xml
  xml'' <- changeCPUMemory newCpu newMem xml'
  return $ show $ xmlToPlainTemplate xml''
  where 
    -- | Because the CONTEXT is stripped off, we have to add it again, manually.
    addSlaveContext :: XML.Element -> Maybe XML.Element
    addSlaveContext templElem = do
      let fromPidStr = show $ Bin.encode fromPid
      let c = Z.fromElement templElem
      rest <- (Z.firstChild c)
      let added = Z.insertLeft (Elem (XML.Element (QName "CONTEXT" Nothing Nothing) []
           [
    --CONTEXT=[
    -- FROM_PID="0.0.0.0" or "creator_pid",
    -- RPC_SERVER="http://one-xmlrpc.calligo.sara.nl:2633/RPC2",
    -- RPC_PROXY="10.0.212.3:3128",
    -- SESSION="username:password",
    -- TYPE="master" or "slave",
    -- VM_TEMPLATE=$TEMPLATE ]
            (Elem (XML.Element (QName "FROM_PID" Nothing Nothing) [] [Text $ CData CDataText fromPidStr Nothing] Nothing)),
            (Elem (XML.Element (QName "RPC_SERVER" Nothing Nothing) [] [Text $ CData CDataText rpcServer Nothing] Nothing)),
            (Elem (XML.Element (QName "RPC_PROXY" Nothing Nothing) [] [Text $ CData CDataText rpcProxy Nothing] Nothing)),
            (Elem (XML.Element (QName "SESSION" Nothing Nothing) [] [Text $ CData CDataText session Nothing] Nothing)),
            (Elem (XML.Element (QName "TYPE" Nothing Nothing) [] [Text $ CData CDataText "slave" Nothing] Nothing)),
            (Elem (XML.Element (QName "VM_TEMPLATE" Nothing Nothing) [] [Text $ CData CDataText "$TEMPLATE" Nothing] Nothing))
           ]
                        Nothing)) rest
      return $ case Z.toTree (Z.root added) of
                 Elem e -> e
                 _ -> error "transforming xml failed"

    changeCPUMemory :: Int -> Int -> XML.Element -> Maybe XML.Element
    changeCPUMemory new_cpu new_mem templElem = do
      let c = Z.fromElement templElem
      cpu <- Z.findChild (\case 
                          Z.Cur {Z.current = Elem (XML.Element (QName "VCPU" _ _) _ _ _)} -> True
                          _ -> False) c
      cpuContents <- Z.firstChild cpu
      let cpuContents' = Z.setContent (Text (CData CDataVerbatim (show new_cpu) Nothing)) cpuContents
      mem <- Z.findChild (\case 
                      Z.Cur {Z.current = Elem (XML.Element (QName "MEMORY" _ _) _ _ _)} -> True
                      _ -> False) (Z.root cpuContents')
      memContents <-  Z.firstChild mem
      let memContents' = Z.setContent (Text (CData CDataVerbatim (show new_mem) Nothing)) memContents
      return $ case Z.toTree (Z.root memContents') of
                 Elem e -> e
                 _ -> error "transforming xml failed"
