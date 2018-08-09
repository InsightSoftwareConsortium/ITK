# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Describe a C++ namespace declaration.

"""

from . import scopedef
from . import declaration_utils


class namespace_t(scopedef.scopedef_t):
    """
    Describes C++ namespace.

    """

    def __init__(self, name='', declarations=None):
        """
        Creates an object that describes a C++ namespace declaration.

        Args:
            name (str): name of the namespace
            declarations (list[declaration_t]): list of declarations

        """
        scopedef.scopedef_t.__init__(self, name)
        if not declarations:
            declarations = []
        # List of all declarations belonging to this namespace
        self._declarations = declarations

    def __str__(self):
        name = declaration_utils.full_name(self)
        if name != "::" and name[:2] == "::":
            name = name[2:]
        return "%s [namespace]" % name

    def _get__cmp__scope_items(self):
        """
        Implementation detail.

        """
        return [self.declarations.sort()]

    def _get_declarations_impl(self):
        return self._declarations

    @property
    def declarations(self):
        """
        List of children declarations.

        Returns:
            list[declaration_t]
        """
        return scopedef.scopedef_t.declarations.fget(self)

    @declarations.setter
    def declarations(self, declarations):
        """
        Set list of all declarations defined in the namespace.

        Args:
            declarations (list[declaration_t]): list of declarations

        """
        self._declarations = declarations

    def take_parenting(self, inst):
        """
        Takes parenting from inst and transfers it to self.

        Args:
            inst (namespace_t): a namespace declaration

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
        self._warn_deprecated()
        answer = []
        if recursive:
            for decl in self.declarations:
                answer.extend(decl.i_depend_on_them())
        return answer


def get_global_namespace(decls):
    """
    Get the global namespace (::) from a declaration tree.

    Args:
        decls (list[declaration_t]): a list of declarations

    Returns:
        namespace_t: the global namespace_t object (::)

    """
    found = [
        decl for decl in scopedef.make_flatten(decls) if decl.name == '::' and
        isinstance(decl, namespace_t)]
    if len(found) == 1:
        return found[0]
    raise RuntimeError("Unable to find global namespace.")
