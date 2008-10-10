# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0. (See
# accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

"""
this module contains class that keeps dependency information of some declaration
"""

import cpptypes

class dependency_info_t( object ):
    def __init__( self, declaration, depend_on_it, access_type=None, hint=None ):
        object.__init__( self )
        #prevent recursive import
        import class_declaration
        assert isinstance( depend_on_it, ( class_declaration.class_t, cpptypes.type_t ) )
        self._declaration = declaration
        self._depend_on_it = depend_on_it
        self._access_type = access_type
        self._hint = hint
        
    @property
    def declaration( self ):
        return self._declaration
    #short name
    decl = declaration

    @property 
    def depend_on_it( self ):
        return self._depend_on_it
    
    def _get_access_type( self ):
        return self._access_type
    def _set_access_type( self, access_type ):
        self._access_type = access_type   
    access_type = property( _get_access_type, _set_access_type )

    def __str__( self ):
        return 'declaration "%s" depends( %s ) on "%s" ' \
               % ( self.declaration, self.access_type, self.depend_on_it )

    @property
    def hint(self):
        """the declaration, that report dependency can put some additional inforamtion
        about dependency. It can be used later"""
        return self._hint

    def find_out_depend_on_declaration( self ):
        """if declaration depends on other declaration and not on some type
           this function will return reference to it. Otherwise None will be returned
        """
        #prevent recursive import
        from pygccxml import declarations
        
        if isinstance( self.depend_on_it, declarations.declaration_t ):
            return self.depend_on_it
        base_type = declarations.base_type( declarations.remove_alias( self.depend_on_it ) )
        if isinstance( base_type, cpptypes.declarated_t ):
            return base_type.declaration
        return None
