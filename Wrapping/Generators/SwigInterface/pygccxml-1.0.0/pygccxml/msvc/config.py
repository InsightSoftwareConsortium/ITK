from __future__ import print_function

import os
import sys
import comtypes
from .. import utils
import comtypes.client
import _winreg as win_registry
from distutils import msvccompiler

class binaries_searcher_t:

    def get_msbsc_path( self ):
        relative_path = os.path.dirname( sys.modules[__name__].__file__)
        absolute_path = os.path.abspath (relative_path)
        return os.path.join( absolute_path, 'msbsc70.dll' )

    def get_msvcr70_path( self ):
        relative_path = os.path.dirname( sys.modules[__name__].__file__)
        absolute_path = os.path.abspath (relative_path)
        return os.path.join( absolute_path, 'msvcr70.dll' )


    def get_msvcr_path( self ):
        vss_installed = self.__get_installed_vs_dirs()
        for f in utils.files_walker( vss_installed, ["*.dll"], ):
            f_path, f_name = os.path.split( f.upper() )
            if f_name.startswith( 'MSVCR' ):
                return f
        else:
            raise RuntimeError( 'Unable to find msvcrXX.dll. Search path is: %s' % vss_installed  )

    def get_msdia_path( self ):
        vss_installed = self.__get_installed_vs_dirs()
        msdia_dlls = self.__get_msdia_dll_paths( vss_installed )
        if 1 == len(msdia_dlls):
            return msdia_dlls[0]
        else:
            #TODO find the highest version and use it.
            pass

    def __get_msdia_dll_paths( self, vss_installed ):
        msdia_dlls = []
        for vs in vss_installed:
            debug_dir = os.path.join( vs, 'Common7', 'Packages', 'Debugger' )
            files = filter( lambda f: f.startswith( 'msdia' ) and f.endswith( '.dll' )
                            , os.listdir( debug_dir ) )
            if not files:
                continue
            msdia_dlls.extend([ os.path.join( debug_dir, f ) for f in files ])
        if not msdia_dlls:
            raise RuntimeError( 'pygccxml unable to find out msdiaXX.dll location' )
        return msdia_dlls

    def __get_installed_vs_dirs( self ):
        vs_reg_path = 'Software\Microsoft\VisualStudio\SxS\VS7'
        values = self.read_values( win_registry.HKEY_LOCAL_MACHINE, vs_reg_path )
        return [ values.values()[0] ]

    def read_keys(self, base, key):
        return msvccompiler.read_keys(base, key)

    def read_values(self, base, key):
        return msvccompiler.read_values(base, key)

bs = binaries_searcher_t()

msdia_path = bs.get_msdia_path()
print('msdia path: ', msdia_path)

msbsc_path = bs.get_msbsc_path()
print('msbsc path: ', msbsc_path)

msvcr_path = bs.get_msvcr_path()
print('msvcr path: ', msvcr_path)
