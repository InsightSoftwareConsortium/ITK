import os
import sys
import ctypes
import logging
import comtypes
import comtypes.client

from . import impl_details

from ... import utils
from ... import declarations
from .. import config as msvc_cfg

msdia = comtypes.client.GetModule( msvc_cfg.msdia_path )

#~ comtypes_client_gen_dir = comtypes.client.gen_dir
#~ try:
    #~ comtypes.client.gen_dir = None
    #~ msdia = comtypes.client.GetModule( msvc_cfg.msdia_path )
#~ finally:
    #~ comtypes.client.gen_dir = comtypes_client_gen_dir

from loader import decl_loader_t
