# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0. (See
# accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

"""This module contains the implementation of the L{config_t} class.
"""

import os
import sys
import copy

class parser_configuration_t(object):
    """Configuration object to collect parameters for invoking C++ parser

    This class serves as a base class for the parameters that can be used
    to customize the call to C++ parser. This class also allows users to work with
    relative files paths. In this case files are searched in the following order:

    1. current directory

    2. working directory

    3. additional include paths specified by the user

    """
    def __init__( self
                  , working_directory='.'
                  , include_paths=None
                  , define_symbols=None
                  , undefine_symbols=None
                  , cflags=""
                  , compiler=None):
        """Constructor.
        """
        object.__init__( self )
        self.__working_directory = working_directory

        if not include_paths:
            include_paths = []
        self.__include_paths = include_paths

        if not define_symbols:
            define_symbols = []
        self.__define_symbols = define_symbols

        if not undefine_symbols:
            undefine_symbols = []
        self.__undefine_symbols = undefine_symbols

        self.__cflags = cflags

        self.__compiler = compiler

    def clone(self):
        raise NotImplementedError( self.__class__.__name__ )

    def __get_working_directory(self):
        return self.__working_directory
    def __set_working_directory(self, working_dir):
        self.__working_directory=working_dir
    working_directory = property( __get_working_directory, __set_working_directory )

    @property
    def include_paths(self):
        """list of include paths to look for header files"""
        return self.__include_paths

    @property
    def define_symbols(self):
        """list of "define" directives """
        return self.__define_symbols

    @property
    def undefine_symbols(self):
        """list of "undefine" directives """
        return self.__undefine_symbols

    @property
    def compiler(self):
        """compiler name to simulate"""
        return self.__compiler

    def __get_cflags(self):
        return self.__cflags
    def __set_cflags(self, val):
        self.__cflags = val
    cflags = property( __get_cflags, __set_cflags
                      , doc="additional flags to pass to compiler" )

    def __ensure_dir_exists( self, dir_path, meaning ):
        if os.path.isdir( dir_path ):
            return
        msg = None
        if os.path.exists( self.working_directory ):
            raise RuntimeError( '%s("%s") does not exist!' % ( meaning, dir_path ) )
        else:
            raise RuntimeError( '%s("%s") should be "directory", not a file.' % ( meaning, dir_path ) )


    def raise_on_wrong_settings( self ):
        """validates the configuration settings and raises RuntimeError on error"""
        self.__ensure_dir_exists( self.working_directory, 'working directory' )
        map( lambda idir: self.__ensure_dir_exists( idir, 'include directory' )
             , self.include_paths )


class gccxml_configuration_t(parser_configuration_t):
    """Configuration object to collect parameters for invoking gccxml.

    This class serves as a container for the parameters that can be used
    to customize the call to gccxml.
    """
    def __init__( self
                  , gccxml_path=''
                  , working_directory='.'
                  , include_paths=None
                  , define_symbols=None
                  , undefine_symbols=None
                  , start_with_declarations=None
                  , ignore_gccxml_output=False
                  , cflags=""
                  , compiler=None):
        """Constructor.
        """
        parser_configuration_t.__init__( self
                                         , working_directory=working_directory
                                         , include_paths=include_paths
                                         , define_symbols=define_symbols
                                         , undefine_symbols=undefine_symbols
                                         , cflags=cflags
                                         , compiler=compiler)

        self.__gccxml_path = gccxml_path

        if not start_with_declarations:
            start_with_declarations = []
        self.__start_with_declarations = start_with_declarations

        self.__ignore_gccxml_output = ignore_gccxml_output

    def clone(self):
        return copy.deepcopy( self )

    def __get_gccxml_path(self):
        return self.__gccxml_path
    def __set_gccxml_path(self, new_path ):
        self.__gccxml_path = new_path
    gccxml_path = property( __get_gccxml_path, __set_gccxml_path
                            , doc="gccxml binary location" )

    @property
    def start_with_declarations(self):
        """list of declarations gccxml should start with, when it dumps declaration tree"""
        return self.__start_with_declarations

    def __get_ignore_gccxml_output(self):
        return self.__ignore_gccxml_output
    def __set_ignore_gccxml_output(self, val=True):
        self.__ignore_gccxml_output = val
    ignore_gccxml_output = property( __get_ignore_gccxml_output, __set_ignore_gccxml_output
                                    , doc="set this property to True, if you want pygccxml to ignore any error\\warning that comes from gccxml" )


    def raise_on_wrong_settings( self ):
        super( gccxml_configuration_t, self ).raise_on_wrong_settings()
        if os.path.isfile( self.gccxml_path ):
            return
        if sys.platform == 'win32':
            gccxml_name = 'gccxml' + '.exe'
            environment_var_delimiter = ';'
        elif sys.platform == 'linux2' or sys.platform == 'darwin':
            gccxml_name = 'gccxml'
            environment_var_delimiter = ':'
        else:
            raise RuntimeError( 'unable to find out location of gccxml' )
        may_be_gccxml = os.path.join( self.gccxml_path, gccxml_name )
        if os.path.isfile( may_be_gccxml ):
            self.gccxml_path = may_be_gccxml
        else:
            for path in os.environ['PATH'].split( environment_var_delimiter ):
                gccxml_path = os.path.join( path, gccxml_name )
                if os.path.isfile( gccxml_path ):
                    self.gccxml_path = gccxml_path
                    break
            else:
                msg = 'gccxml_path("%s") should exists or to be a valid file name.' \
                      % self.gccxml_path
                raise RuntimeError( msg )

config_t = gccxml_configuration_t #backward computability
