# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

from . import declaration
from . import typedef
from . import cpptypes
from . import type_traits
from . import class_declaration
from . import variable
from . import calldef
from . import namespace


def get_dependencies_from_decl(decl, recursive=True):
    """
    Returns the list of all types and declarations the declaration depends on.

    """
    result = []
    if isinstance(decl, typedef.typedef_t) or \
            isinstance(decl, variable.variable_t):
        return [dependency_info_t(decl, decl.decl_type)]
    if isinstance(decl, namespace.namespace_t):
        if recursive:
            for d in decl.declarations:
                result.extend(get_dependencies_from_decl(d))
        return result
    if isinstance(decl, calldef.calldef_t):
        if decl.return_type:
            result.append(
                dependency_info_t(decl, decl.return_type, hint="return type"))
        for arg in decl.arguments:
            result.append(dependency_info_t(decl, arg.decl_type))
        for exc in decl.exceptions:
            result.append(dependency_info_t(decl, exc, hint="exception"))
        return result
    if isinstance(decl, class_declaration.class_t):
        for base in decl.bases:
            result.append(
                dependency_info_t(
                    decl,
                    base.related_class,
                    base.access_type,
                    "base class"))
        if recursive:
            for access_type in class_declaration.ACCESS_TYPES.ALL:
                result.extend(
                    __find_out_member_dependencies(
                        decl.get_members(access_type), access_type))
        return result
    return result


def __find_out_member_dependencies(members, access_type):
    answer = []
    for mem in members:
        answer.extend(get_dependencies_from_decl(mem, recursive=True))
    member_ids = set([id(m) for m in members])
    for dependency in answer:
        if id(dependency.declaration) in member_ids:
            dependency.access_type = access_type
    return answer


class dependency_info_t(object):

    def __init__(self, decl, depend_on_it, access_type=None, hint=None):
        object.__init__(self)

        assert isinstance(
            depend_on_it, (class_declaration.class_t, cpptypes.type_t))
        self._decl = decl
        self._depend_on_it = depend_on_it
        self._access_type = access_type
        self._hint = hint

    @property
    def declaration(self):
        return self._decl

    @property
    def depend_on_it(self):
        return self._depend_on_it

    @property
    def access_type(self):
        return self._access_type

    @access_type.setter
    def access_type(self, access_type):
        self._access_type = access_type

    def __str__(self):
        return 'declaration "%s" depends( %s ) on "%s" ' \
               % (self.declaration, self.access_type, self.depend_on_it)

    @property
    def hint(self):
        """The declaration, that report dependency can put some additional
        inforamtion about dependency. It can be used later"""
        return self._hint

    def find_out_depend_on_it_declarations(self):
        """If declaration depends on other declaration and not on some type
        this function will return reference to it. Otherwise None will be
        returned
        """
        return impl_details.dig_declarations(self.depend_on_it)

    @staticmethod
    def i_depend_on_them(decl):
        """Returns set of declarations. every item in the returned set,
        depends on a declaration from the input"""

        to_be_included = set()
        for dependency_info in get_dependencies_from_decl(decl):
            for ddecl in dependency_info.find_out_depend_on_it_declarations():
                if ddecl:
                    to_be_included.add(ddecl)

        if isinstance(decl.parent, class_declaration.class_t):
            to_be_included.add(decl.parent)
        return to_be_included

    @staticmethod
    def we_depend_on_them(decls):
        """Returns set of declarations. every item in the returned set,
        depends on a declaration from the input"""
        to_be_included = set()
        for decl in decls:
            to_be_included.update(dependency_info_t.i_depend_on_them(decl))
        return to_be_included


class impl_details(object):

    @staticmethod
    def dig_declarations(depend_on_it):

        if isinstance(depend_on_it, declaration.declaration_t):
            return [depend_on_it]
        base_type = type_traits.base_type(
            type_traits.remove_alias(depend_on_it))
        if isinstance(base_type, cpptypes.declarated_t):
            return [base_type.declaration]
        elif isinstance(base_type, cpptypes.calldef_type_t):
            result = []
            result.extend(impl_details.dig_declarations(base_type.return_type))
            for argtype in base_type.arguments_types:
                result.extend(impl_details.dig_declarations(argtype))
            if isinstance(base_type, cpptypes.member_function_type_t):
                result.extend(
                    impl_details.dig_declarations(
                        base_type.class_inst))
            return result
        return []
