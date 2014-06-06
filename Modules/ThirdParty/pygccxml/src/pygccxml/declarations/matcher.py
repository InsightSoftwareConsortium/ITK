# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0. (See
# accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

"""implements few "find" algorithms on declarations tree"""

import types
import algorithm

class matcher:
    """class-namespace, contains implementation of few "find" algorithms and
    definition of related exception classes"""

    class declaration_not_found_t( RuntimeError ):
        """exception, that will be raised, if the declaration could not be found"""
        def __init__( self, matcher ):
            RuntimeError.__init__( self )
            self.matcher = matcher

        def __str__( self ):
            return "Unable to find declaration.  matcher: [%s]"%str(self.matcher)

    class multiple_declarations_found_t( RuntimeError ):
        """exception, that will be raised, if more than one declaration was found"""
        def __init__( self, matcher ):
            RuntimeError.__init__( self )
            self.matcher = matcher

        def __str__( self ):
            return "Multiple declarations has been found. matcher: [%s]"%str(self.matcher)

    @staticmethod
    def find( decl_matcher, decls, recursive=True ):
        """returns a list of declarations that match "decl_matcher" defined criretia or None

        @param decl_matcher: Python callable object, that takes one argument - reference to declaration
        @param decls: reference to declaration or list of declarations to be searched in
        @param recursive: boolean, if True the method will run decl_matcher, on internal declarations too
        """

        where = []
        if isinstance( decls, types.ListType ):
            where.extend( decls )
        else:
            where.append( decls )
        if recursive:
            where = algorithm.make_flatten( where )
        return filter( decl_matcher, where )

    @staticmethod
    def find_single( decl_matcher, decls, recursive=True ):
        """returns a reference to declaration, that match "decl_matcher" defined
        criretia, if a unique declaration could not be found the method will return
        None.

        @param decl_matcher: Python callable object, that takes one argument - reference to declaration
        @param decls: reference to declaration or list of declarations to be searched in
        @param recursive: boolean, if True the method will run decl_matcher, on internal declarations too
        """
        answer = matcher.find( decl_matcher, decls, recursive )
        if len(answer) == 1:
            return answer[0]

    @staticmethod
    def get_single( decl_matcher, decls, recursive=True ):
        """returns a reference to declaration, that match "decl_matcher" defined
        criretia, if a unique declaration could not be found, an appropriate
        exception will be raised.

        @param decl_matcher: Python callable object, that takes one argument - reference to declaration
        @param decls: reference to declaration or list of declarations to be searched in
        @param recursive: boolean, if True the method will run decl_matcher, on internal declarations too
        """
        answer = matcher.find( decl_matcher, decls, recursive )
        if len(answer) == 1:
            return answer[0]
        elif len(answer) == 0:
            raise matcher.declaration_not_found_t( decl_matcher )
        else:
            raise matcher.multiple_declarations_found_t( decl_matcher )
