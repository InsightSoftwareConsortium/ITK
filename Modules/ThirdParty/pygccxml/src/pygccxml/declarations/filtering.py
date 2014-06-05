# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0. (See
# accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

"""
deprecated!

This module defines few algorithms for filtering declarations.
"""

import os
import algorithm

class filtering:
    """deprecated!

    defines few algorithms for filtering declarations
    """

    @staticmethod
    def normalize_path( some_path ):
        """return os.path.normcase( os.path.normpath( some_path ) )"""
        return os.path.normcase( os.path.normpath( some_path ) )

    @staticmethod
    def contains_parent_dir( fpath, dirs ):
        #precondition: dirs and fpath should be normalize_path'ed before calling this function
        return bool( filter( lambda dir: fpath.startswith( dir ), dirs ) )

    @staticmethod
    def by_location( decls, locations ):
        """
        returns list of declarations that belongs to specified locations.

        This function works recursively. Pay attention: if you remove namespace,
        then you remove all declarations defined within the namespace.

        @param decls: declaration or list of declarations
        @type decls: L{declaration<declaration_t>} or list of L{declarations<declaration_t>}

        @param locations: list of directories and/or files names
        @type locations: list of strings

        @return: list of L{declarations<declaration_t>}
        """
        #precondition: decls is a list of op level namespaces
        #locations is list of directories and\or files
        temp_decls = algorithm.make_flatten( decls )
        locations = map( filtering.normalize_path, locations )
        dirs = filter( lambda location: os.path.isdir( location ), locations )
        files = filter( lambda location: os.path.isfile( location ), locations )
        result = []
        for decl in temp_decls:
            if not decl.location:
                result.append( decl )
                continue
            fpath = filtering.normalize_path( decl.location.file_name )
            if filtering.contains_parent_dir( fpath, dirs ) or fpath in files:
                result.append( decl )
        return result

    @staticmethod
    def user_defined( decls, matcher ):
        """
        returns list of declarations that match user specified criteria.

        This function works recursively.

        @param decls: declaration or list of declarations
        @type decls: L{declaration<declaration_t>} or list of L{declarations<declaration_t>}

        @param matcher: callable object, that takes 1 argument - declaration
                        and returns True if object should stay, and false otherwise

        @return: list of L{declarations<declaration_t>}
        """
        #precondition: decls is a list of op level namespaces
        return filter( matcher, algorithm.make_flatten( decls ) )
