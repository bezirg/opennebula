{-# LANGUAGE LambdaCase #-}
module OpenNebula.Template (inheritXmlTemplate, contextualize, toPlain, showPlain, extractVmId)  where

import Text.XML.Light 
import qualified Text.XML.Light.Cursor as Z
import Data.List (intersperse)

extractVmId :: String -> Maybe Int
extractVmId xml_str = do
  vmElem <- parseXMLDoc xml_str
  vmIdElem <- findChild (QName "ID" Nothing Nothing) vmElem
  return $ read $ strContent vmIdElem


-- | inheritXmlTemplate :: Input -> From -> Maybe XMLTree
inheritXmlTemplate :: String ->  Maybe Element
inheritXmlTemplate xml_str = do
  doc <- parseXMLDoc xml_str
  templElem <- findElement (QName "TEMPLATE" Nothing Nothing) doc
  cleanedTempl <- cleanupTemplate templElem
  return cleanedTempl
         where
    cleanupTemplate :: Element -> Maybe Element
    cleanupTemplate templElem = do
      let c = Z.fromElement templElem
      -- remove NAME
      stop <- Z.removeGoUp =<< Z.findChild (\case 
                                           Z.Cur {Z.current = Elem (Element (QName "NAME" _ _) _ _ _)} -> True
                                           _ -> False) c
      -- remove TEMPLATE_ID
      stop <- Z.removeGoUp =<< Z.findChild (\case 
                                           Z.Cur {Z.current = Elem (Element (QName "TEMPLATE_ID" _ _) _ _ _)} -> True
                                           _ -> False) (Z.root stop)
      -- removed VMID
      stop <- Z.removeGoUp =<< Z.findChild (\case 
                                           Z.Cur {Z.current = Elem (Element (QName "VMID" _ _) _ _ _)} -> True
                                           _ -> False) (Z.root stop)

      -- remove NIC[IP+MAC]
      nic <- Z.findChild (\case 
                          Z.Cur {Z.current = Elem (Element (QName "NIC" _ _) _ _ _)} -> True
                          _ -> False) (Z.root stop)
      ip_or_mac <- Z.findChild (\case 
                          Z.Cur {Z.current = Elem (Element (QName ipOrMac  _ _) _ _ _)} -> ipOrMac `elem` ["IP", "MAC"]
                          _ -> False) nic
      nic <- Z.removeGoUp ip_or_mac
      ip_or_mac <- Z.findChild (\case 
                                Z.Cur {Z.current = Elem (Element (QName ipOrMac _ _) _ _ _)} -> ipOrMac `elem` ["IP", "MAC"]
                                _ -> False) nic
      stop <- Z.removeGoUp ip_or_mac
      --remove NIC[IP+MAC]
      nic <- Z.findRight (\case 
                          Z.Cur {Z.current = Elem (Element (QName "NIC" _ _) _ _ _)} -> True
                          _ -> False) (stop)
      ip_or_mac <- Z.findChild (\case 
                          Z.Cur {Z.current = Elem (Element (QName ipOrMac  _ _) _ _ _)} -> ipOrMac `elem` ["IP", "MAC"]
                          _ -> False) nic
      nic <- Z.removeGoUp ip_or_mac
      ip_or_mac <- Z.findChild (\case 
                                Z.Cur {Z.current = Elem (Element (QName ipOrMac _ _) _ _ _)} -> ipOrMac `elem` ["IP", "MAC"]
                                _ -> False) nic
      stop <- Z.removeGoUp ip_or_mac
      return $ case Z.toTree (Z.root stop) of
                 Elem e -> e
                 _ -> error "transforming xml failed"


contextualize :: String -> Int -> Int -> Element -> Maybe Element
contextualize from new_cpu new_mem templElem = addSlaveContext from templElem >>= changeCPUMemory new_cpu new_mem
  where
    addSlaveContext :: String -> Element -> Maybe Element
    addSlaveContext from templElem = do
      --CONTEXT=[
      -- FROM=0.0.0.0 or creator_pid,
      -- TYPE=master,
      -- VM_TEMPLATE=$TEMPLATE ]
      let c = Z.fromElement templElem
      rest <- (Z.firstChild c)
      let added = Z.insertLeft (Elem (Element (QName "CONTEXT" Nothing Nothing) [] [
                                                 (Elem (Element (QName "FROM" Nothing Nothing) [] [Text $ CData CDataVerbatim from Nothing] Nothing)),
                                                 (Elem (Element (QName "TYPE" Nothing Nothing) [] [Text $ CData CDataVerbatim "slave" Nothing] Nothing)),
                                                 (Elem (Element (QName "VM_TEMPLATE" Nothing Nothing) [] [Text $ CData CDataVerbatim "$TEMPLATE" Nothing] Nothing))
                                     ]
                        Nothing)) rest
      return $ case Z.toTree (Z.root added) of
                 Elem e -> e
                 _ -> error "transforming xml failed"

    changeCPUMemory :: Int -> Int -> Element -> Maybe Element
    changeCPUMemory new_cpu new_mem templElem = do
      let c = Z.fromElement templElem
      cpu <- Z.findChild (\case 
                          Z.Cur {Z.current = Elem (Element (QName "VCPU" _ _) _ _ _)} -> True
                          _ -> False) c
      cpuContents <- Z.firstChild cpu
      let cpuContents' = Z.setContent (Text (CData CDataVerbatim (show new_cpu) Nothing)) cpuContents
      mem <- Z.findChild (\case 
                      Z.Cur {Z.current = Elem (Element (QName "MEMORY" _ _) _ _ _)} -> True
                      _ -> False) (Z.root cpuContents')
      memContents <-  Z.firstChild mem
      let memContents' = Z.setContent (Text (CData CDataVerbatim (show new_mem) Nothing)) memContents
      return $ case Z.toTree (Z.root memContents') of
                 Elem e -> e
                 _ -> error "transforming xml failed"

toPlain :: Element -> Template
toPlain = pTemplate

showPlain :: Template -> String
showPlain = show



-- Parsing and converting XML to Plain Template syntax
------------------------------------------------------

pTemplate :: Element -> Template
pTemplate (Element (QName "TEMPLATE" _ _) _attrs content _line ) = Template (map pContent content)

pElement :: Element -> TemplateEntry
pElement (Element (QName name _ _) _attrs [Text (CData _kind str _)] _line ) = SingleEntry name ('"' : str ++ "\"")
pElement (Element (QName name _ _) _attrs content _line) = ManyEntry name (map pContent content)

pContent :: Content -> TemplateEntry
pContent (Elem e) = pElement e
pContent _ = error "parsing xml failed"


newtype Template = Template { entries :: [TemplateEntry] } 

instance Show Template where
    show (Template es) = unlines $ map show es -- insert newline after each entry, no comma

data TemplateEntry = SingleEntry String String
                       | ManyEntry String [TemplateEntry]

instance Show TemplateEntry where
    show (SingleEntry key value) = key ++ "=" ++ value
    show (ManyEntry key values) = key ++ "= [" ++ unlines (intersperse "," $ map show values) ++ "]"

