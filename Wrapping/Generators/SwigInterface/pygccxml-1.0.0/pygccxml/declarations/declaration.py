# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0. (See
# accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

"""
defines 2 important classes

This module defines:
* declaration_t - base class for all pygccxml defined classes, which describe
  a C++ declaration
* location_t - provides information about physical location of the declaration
"""

from __future__ import print_function

import algorithm
import templates
import algorithms_cache

class location_t(object):
    """provides information about the location of the declaration within the source file"""

    def __init__(self, file_name='', line=-1 ):
        self._file_name = file_name
        self._line = line

    def __eq__(self, other):
        if not isinstance( other, self.__class__ ):
            return False
        return self.line == other.line \
               and self.file_name == other.file_name

    def __ne__( self, other):
        return not self.__eq__( other )

    def __lt__( self, other ):
        if not isinstance( other, location_t ):
            return self.__class__.__name__ < other.__class__.__name__
        return ( self.file_name, self.line ) < ( other.file_name, other.line )

    def _get_file_name(self):
        return self._file_name
    def _set_file_name(self, new_file_name):
        self._file_name = new_file_name
    file_name = property( _get_file_name, _set_file_name
                          , doc="""absolute source file name, type string""" )

    def _get_line( self ):
        return self._line
    def _set_line( self, new_line ):
        self._line = new_line
    line = property( _get_line, _set_line, doc="""line number, type int""")

    def as_tuple( self ):
        """return tuple(self.file_name, self.line)"""
        return (self.file_name, self.line)

class declaration_t( object ):
    """base class for all classes that represent a C++ declaration"""

    def __init__( self, name='', location=None, is_artificial=False, mangled=None, demangled=None, attributes=None ):
        self._name = name
        self._location = location
        self._is_artificial = is_artificial
        self._mangled = mangled
        self._demangled = demangled
        self._attributes = attributes
        self._parent = None
        self._cache = algorithms_cache.declaration_algs_cache_t()
        self._compiler = None
        self._partial_name = None

    def __str__(self):
        """Default __str__ method.

        This version just returns the decl_string and the class.
        Derived classes may override this method to provide more detailed
        information.

        A __str__ method for a declaration should always provide enough
        information so that it uniquely identifies the declaration and
        the user is able to find the declaration in his source code.
        """
        name = self.decl_string
        if name[:2]=="::":
            name = name[2:]
        # Append the declaration class
        cls = self.__class__.__name__
        if cls[-2:]=="_t":
            cls = cls[:-2]
        cls = cls.replace( '_', ' ' )
        return "%s [%s]"%(name, cls)

    @staticmethod
    def _sorted_list( some_list ):
        """implementation details"""
        some_list.sort()
        return some_list

    def _get__cmp__items( self ):
        """implementation details"""
        #Every derived class should implement this method. This method should
        #return a list of items, that should be compared.

        print('_get__cmp__items not implemented for class ', self.__class__.__name__)
        raise NotImplemented()

    def _get__cmp__data(self):
        """implementation details"""
        data = [ algorithm.declaration_path( self.parent ), self.name, self.location ]
        data.extend( self._get__cmp__items() )
        return data

    def __eq__(self, other):
        """
        function will return true, if both declarations refers to the same object.
        This function could be implemented in terms of _get__cmp__data, but in
        this case it will downgrade performance. self.mangled property is not
        compared, because it could be chaned from one compilation time to an
        other.
        """
        if not isinstance( other, self.__class__ ):
            return False
        return self.name == other.name \
               and self.location == other.location \
               and algorithm.declaration_path( self.parent ) \
                   == algorithm.declaration_path( other.parent )

    def __ne__( self, other):
        """return not self.__eq__( other )"""
        return not self.__eq__( other )

    def __lt__(self, other):
        """
        C{if not isinstance( other, self.__class__ ):}
        C{    return self.__class__.__name__ < other.__class__.__name__}
        C{return self._get__cmp__data() < other._get__cmp__data()}
        """
        if not isinstance( other, self.__class__ ):
            return self.__class__.__name__ < other.__class__.__name__
        return self._get__cmp__data() < other._get__cmp__data()

    def _get_name_impl( self ):
        return self._name

    def _get_name( self ):
        return self._get_name_impl()

    def _on_rename( self ):
        pass

    def _set_name( self, new_name ):
        previous_name = self._name
        self._name = new_name
        self._partial_name = None
        self.cache.reset_name_based()
        if previous_name: #the was a rename and not initial "set"
            self._on_rename()

    name = property( _get_name, _set_name
                     , doc="""Declaration name
                     @type: str
                     """)

    def _get_partial_name_impl( self ):
        return self.name

    @property
    def partial_name( self ):
        """declaration name, without template default arguments
        Right now std containers is the only classes that support this functionality"""
        if None is self._partial_name:
            self._partial_name = self._get_partial_name_impl()
        return self._partial_name

    def _get_parent(self):
        return self._parent
    def _set_parent(self, new_parent):
        if new_parent:
            assert( isinstance( new_parent, declaration_t ) )
        self._parent = new_parent
    parent = property( _get_parent, _set_parent
                       , doc="""Reference to parent declaration
                       @type: declaration_t
                       """)

    def __get_top_parent(self):
        parent = self.parent
        me = self
        while True:
            if not parent:
                return me
            else:
                me = parent
                parent = me.parent
    top_parent = property( __get_top_parent,
                           doc="""reference to top parent declaration
                           @type: declaration_t
                           """ )

    def _get_location( self ):
        return self._location
    def _set_location( self, new_location ):
        self._location = new_location
    location = property( _get_location, _set_location
                        , doc="""Location of the declaration within source file
                        @type: L{location_t}
                        """)

    def _get_is_artificial( self ):
        return self._is_artificial
    def _set_is_artificial( self, new_artificial ):
        self._is_artificial = new_artificial
    is_artificial = property( _get_is_artificial, _set_is_artificial
                              , doc="""Describes whether declaration is compiler generated or not
                              @type: bool
                              """)

    def _get_mangled( self ):
        return self._mangled
    def _set_mangled( self, mangled ):
        self._mangled = mangled
    mangled = property( _get_mangled, _set_mangled
                        , doc="""Compiler generated declaration name
                        @type: str
                        """ )

    def _get_demangled( self ):
        return self._demangled
    def _set_demangled( self, demangled ):
        self._demangled = demangled
    demangled = property( _get_demangled, _set_demangled
                        , doc="""Demangled compiler generated declaration name
                        @type: str
                        """ )

    def _get_attributes( self ):
        return self._attributes
    def _set_attributes( self, attributes ):
        self._attributes = attributes
    attributes = property( _get_attributes, _set_attributes
                        , doc="""GCCXML attributes, set using __attribute__((gccxml("...")))
                        @type: str
                        """ )

    def create_decl_string(self, with_defaults=True):
        return algorithm.full_name( self, with_defaults )

    @property
    def decl_string(self):
        """declaration full name"""
        return self.create_decl_string()

    @property
    def partial_decl_string(self):
        """declaration full name"""
        return self.create_decl_string(with_defaults=False)

    @property
    def cache( self ):
        """implementation details

        reference to instance of L{algorithms_cache.algorithms_cache_t} class.
        """
        return self._cache

    def i_depend_on_them( self, recursive=True ):
        """return list of all types and declarations the declaration depends on"""
        print(self)
        raise NotImplementedError()

    def _get_compiler( self ):
        return self._compiler
    def _set_compiler( self, compiler ):
        self._compiler = compiler
    compiler = property( _get_compiler, _set_compiler
                        , doc="""compiler name + version
                        @type: str""" )
