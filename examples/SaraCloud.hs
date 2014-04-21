module Main where

import OpenNebula 
import Data.Maybe (fromJust)

server_sara = "http://one-xmlrpc.calligo.sara.nl:2633/RPC2"
session_nikos = "envisage-nicolaosb:PUT-YOUR-PASSWORD"
proxy_cwi = Just "http://10.0.212.3:3128"

main = do
  print =<< xmlrpc server_sara session_nikos proxy_cwi (vm_allocate $ slaveTemplateWithContext "0.0.0.0" 1 16000)

slaveTemplateWithContext my_ip new_cpu new_memory = 
    showPlain (toPlain (fromJust 
                        (contextualize my_ip new_cpu new_memory =<< inheritXmlTemplate xml_vm_template)))




xml_vm_template ="<VM><ID>32612</ID><UID>449</UID><GID>311</GID><UNAME>envisage-nicolaosb</UNAME><GNAME>envisage-cwi</GNAME><NAME>one-32612</NAME><PERMISSIONS><OWNER_U>1</OWNER_U><OWNER_M>1</OWNER_M><OWNER_A>0</OWNER_A><GROUP_U>0</GROUP_U><GROUP_M>0</GROUP_M><GROUP_A>0</GROUP_A><OTHER_U>0</OTHER_U><OTHER_M>0</OTHER_M><OTHER_A>0</OTHER_A></PERMISSIONS><LAST_POLL>0</LAST_POLL><STATE>1</STATE><LCM_STATE>0</LCM_STATE><STIME>1397474429</STIME><ETIME>0</ETIME><DEPLOY_ID></DEPLOY_ID><MEMORY>0</MEMORY><CPU>0</CPU><NET_TX>0</NET_TX><NET_RX>0</NET_RX><TEMPLATE><CPU><![CDATA[1]]></CPU><DISK><BUS><![CDATA[virtio]]></BUS><CLONE><![CDATA[YES]]></CLONE><DISK_ID><![CDATA[0]]></DISK_ID><IMAGE><![CDATA[2014-04-13 00:33:49 - Wizard image: Linux - Ubuntu 12.04 LTS Server ]]></IMAGE><IMAGE_ID><![CDATA[4921]]></IMAGE_ID><IMAGE_UNAME><![CDATA[envisage-nicolaosb]]></IMAGE_UNAME><READONLY><![CDATA[NO]]></READONLY><SAVE><![CDATA[NO]]></SAVE><SOURCE><![CDATA[/var/lib/one/images/32867ea5aaab4c06fae9b8437d3fa111]]></SOURCE><TARGET><![CDATA[hda]]></TARGET><TYPE><![CDATA[DISK]]></TYPE></DISK><GRAPHICS><TYPE><![CDATA[vnc]]></TYPE></GRAPHICS><INPUT><BUS><![CDATA[usb]]></BUS><TYPE><![CDATA[tablet]]></TYPE></INPUT><MEMORY><![CDATA[8192]]></MEMORY><NAME><![CDATA[one-32612]]></NAME><NIC><BRIDGE><![CDATA[br212]]></BRIDGE><IP><![CDATA[10.0.212.28]]></IP><MAC><![CDATA[02:00:0a:00:d4:1c]]></MAC><MODEL><![CDATA[e1000]]></MODEL><NETWORK><![CDATA[envisage-cwi]]></NETWORK><NETWORK_ID><![CDATA[199]]></NETWORK_ID><NETWORK_UNAME><![CDATA[envisage-nicolaosb]]></NETWORK_UNAME><VLAN><![CDATA[NO]]></VLAN></NIC><NIC><BRIDGE><![CDATA[br56]]></BRIDGE><IP><![CDATA[145.100.58.183]]></IP><LIBVIRT_FILTER><![CDATA[calligo-public-network]]></LIBVIRT_FILTER><MAC><![CDATA[02:00:91:64:3a:b7]]></MAC><MODEL><![CDATA[e1000]]></MODEL><NETWORK><![CDATA[internet]]></NETWORK><NETWORK_FILTER><![CDATA[226]]></NETWORK_FILTER><NETWORK_ID><![CDATA[0]]></NETWORK_ID><NETWORK_UNAME><![CDATA[oneadmin]]></NETWORK_UNAME><NF_PARAM_IP_0><![CDATA[0.0.0.0/0]]></NF_PARAM_IP_0><NF_PARAM_IP_1><![CDATA[0.0.0.0/0]]></NF_PARAM_IP_1><NF_PARAM_IP_2><![CDATA[0.0.0.0/0]]></NF_PARAM_IP_2><NF_PARAM_IP_3><![CDATA[127.0.0.1]]></NF_PARAM_IP_3><NF_PARAM_IP_4><![CDATA[127.0.0.1]]></NF_PARAM_IP_4><NF_PARAM_IP_5><![CDATA[127.0.0.1]]></NF_PARAM_IP_5><NF_PARAM_IP_6><![CDATA[127.0.0.1]]></NF_PARAM_IP_6><NF_PARAM_IP_7><![CDATA[127.0.0.1]]></NF_PARAM_IP_7><NF_PARAM_IP_8><![CDATA[127.0.0.1]]></NF_PARAM_IP_8><NF_PARAM_IP_9><![CDATA[127.0.0.1]]></NF_PARAM_IP_9><NF_PARAM_IP_OUT_0><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_0><NF_PARAM_IP_OUT_1><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_1><NF_PARAM_IP_OUT_2><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_2><NF_PARAM_IP_OUT_3><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_3><NF_PARAM_IP_OUT_4><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_4><NF_PARAM_IP_OUT_5><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_5><NF_PARAM_IP_OUT_6><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_6><NF_PARAM_IP_OUT_7><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_7><NF_PARAM_IP_OUT_8><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_8><NF_PARAM_IP_OUT_9><![CDATA[127.0.0.1]]></NF_PARAM_IP_OUT_9><NF_PARAM_PORT_0><![CDATA[22]]></NF_PARAM_PORT_0><NF_PARAM_PORT_1><![CDATA[80]]></NF_PARAM_PORT_1><NF_PARAM_PORT_2><![CDATA[443]]></NF_PARAM_PORT_2><NF_PARAM_PORT_3><![CDATA[0]]></NF_PARAM_PORT_3><NF_PARAM_PORT_4><![CDATA[0]]></NF_PARAM_PORT_4><NF_PARAM_PORT_5><![CDATA[0]]></NF_PARAM_PORT_5><NF_PARAM_PORT_6><![CDATA[0]]></NF_PARAM_PORT_6><NF_PARAM_PORT_7><![CDATA[0]]></NF_PARAM_PORT_7><NF_PARAM_PORT_8><![CDATA[0]]></NF_PARAM_PORT_8><NF_PARAM_PORT_9><![CDATA[0]]></NF_PARAM_PORT_9><NF_PARAM_PORT_OUT_0><![CDATA[0]]></NF_PARAM_PORT_OUT_0><NF_PARAM_PORT_OUT_1><![CDATA[0]]></NF_PARAM_PORT_OUT_1><NF_PARAM_PORT_OUT_2><![CDATA[0]]></NF_PARAM_PORT_OUT_2><NF_PARAM_PORT_OUT_3><![CDATA[0]]></NF_PARAM_PORT_OUT_3><NF_PARAM_PORT_OUT_4><![CDATA[0]]></NF_PARAM_PORT_OUT_4><NF_PARAM_PORT_OUT_5><![CDATA[0]]></NF_PARAM_PORT_OUT_5><NF_PARAM_PORT_OUT_6><![CDATA[0]]></NF_PARAM_PORT_OUT_6><NF_PARAM_PORT_OUT_7><![CDATA[0]]></NF_PARAM_PORT_OUT_7><NF_PARAM_PORT_OUT_8><![CDATA[0]]></NF_PARAM_PORT_OUT_8><NF_PARAM_PORT_OUT_9><![CDATA[0]]></NF_PARAM_PORT_OUT_9><VLAN><![CDATA[NO]]></VLAN></NIC><NODETYPE><![CDATA[NODE=L]]></NODETYPE><OS><ARCH><![CDATA[x86_64]]></ARCH><BOOT><![CDATA[hd]]></BOOT></OS><RAW><TYPE><![CDATA[kvm]]></TYPE></RAW><REQUIREMENTS><![CDATA[NODE=L]]></REQUIREMENTS><TEMPLATE_ID><![CDATA[6123]]></TEMPLATE_ID><VCPU><![CDATA[1]]></VCPU><VMID><![CDATA[32612]]></VMID></TEMPLATE><HISTORY_RECORDS/></VM>"
