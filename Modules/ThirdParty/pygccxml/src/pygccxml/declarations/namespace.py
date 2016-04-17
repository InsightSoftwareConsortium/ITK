# Copyright 2014-2015 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Describe a C++ namespace declaration.

"""

from . import scopedef
from . import algorithm


class namespace_t(scopedef.scopedef_t):
    """
    Describes C++ namespace.

    """

    def __init__(self, name='', declarations=None):
        """
        Creates an object that describes a C++ namespace declaration.

        """

        scopedef.scopedef_t.__init__(self, name)
        if not declarations:
            declarations = []
        # list of all declarations belongs to this namespace
        self._declarations = declarations

    def __str__(self):
        name = algorithm.full_name(self)
        if name != "::" and name[:2] == "::":
            name = name[2:]
        return "%s [namespace]" % name

    def _get__cmp__scope_items(self):
        """
        Implementation detail.

        """

        return [self._sorted_list(self.declarations)]

    def _get_declarations_impl(self):
        return self._declarations

    @scopedef.scopedef_t.declarations.setter
    def declarations(self, declarations):
        """
        List of all declarations defined in the namespace.

        The getter is defined in scopedef.scopedef_t.

        """

        self._declarations = declarations

    def take_parenting(self, inst):
        """
        Takes parenting from inst and transfers it to self.

        """

        if self is inst:
            return
        for decl in inst.declarations:
            decl.parent = self
            self.declarations.append(decl)
        inst.declarations = []

    def adopt_declaration(self, decl):
        self.declarations.append(decl)
        decl.parent = self
        decl.cache.reset()

    def remove_declaration(self, decl):
        """
        Removes declaration from members list.

        :param decl: declaration to be removed
        :type decl: :class:`declaration_t`

        """

        del self.declarations[self.declarations.index(decl)]
        decl.cache.reset()
        # add more comment about this.
        # if not keep_parent:
        #    decl.parent=None

    def namespace(self, name=None, function=None, recursive=None):
        """
        Returns reference to namespace declaration that matches
        a defined criteria.

        """

        return (
            self._find_single(
                scopedef.scopedef_t._impl_matchers[namespace_t.namespace],
                name=name,
                function=function,
                recursive=recursive)
        )
    ns = namespace

    def namespaces(
            self,
            name=None,
            function=None,
            recursive=None,
            allow_empty=None):
        """
        Returns a set of namespace declarations that match
        a defined criteria.

        """

        return (
            self._find_multiple(
                scopedef.scopedef_t._impl_matchers[namespace_t.namespace],
                name=name,
                function=function,
                recursive=recursive,
                allow_empty=allow_empty)
        )
    nss = namespaces

    def free_function(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """
        Returns reference to free function declaration that matches
        a defined criteria.

        """

        return (
            self._find_single(
                scopedef.scopedef_t._impl_matchers[namespace_t.free_function],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[namespace_t.free_function],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )
    free_fun = free_function

    def free_functions(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """
        Returns a set of free function declarations that match
        a defined criteria.

        """

        return (
            self._find_multiple(
                scopedef.scopedef_t._impl_matchers[namespace_t.free_function],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[namespace_t.free_function],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )
    free_funs = free_functions

    def free_operator(
            self,
            name=None,
            function=None,
            symbol=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """
        Returns reference to free operator declaration that matches
        a defined criteria.

        """
        return (
            self._find_single(
                scopedef.scopedef_t._impl_matchers[namespace_t.free_operator],
                name=self._build_operator_name(name, function, symbol),
                symbol=symbol,
                function=self._build_operator_function(name, function),
                decl_type=self._impl_decl_types[namespace_t.free_operator],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def free_operators(
            self,
            name=None,
            function=None,
            symbol=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """
        Returns a set of free operator declarations that match
        a defined criteria.

        """

        return (
            self._find_multiple(
                scopedef.scopedef_t._impl_matchers[namespace_t.free_operator],
                name=self._build_operator_name(name, function, symbol),
                symbol=symbol,
                function=self._build_operator_function(name, function),
                decl_type=self._impl_decl_types[namespace_t.free_operator],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def i_depend_on_them(self, recursive=True):
        answer = []
        if recursive:
            for decl in self.declarations:
                answer.extend(decl.i_depend_on_them())
        return answer
