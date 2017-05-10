# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
defines classes, that describes C++ classes

This modules contains definition for next C++ declarations:
    - class definition
    - class declaration
    - small helper class for describing C++ class hierarchy
"""

import warnings
from . import scopedef
from . import declaration_utils
from . import declaration
from . import templates
from . import cpptypes
from . import byte_info
from . import elaborated_info


class ACCESS_TYPES(object):

    """class that defines "access" constants"""
    PUBLIC = "public"
    PRIVATE = "private"
    PROTECTED = "protected"
    ALL = [PUBLIC, PRIVATE, PROTECTED]


class CLASS_TYPES(object):

    """class that defines "class" type constants"""
    CLASS = "class"
    STRUCT = "struct"
    UNION = "union"
    ALL = [CLASS, STRUCT, UNION]


def get_partial_name(name):
    from . import container_traits  # prevent cyclic dependencies
    ct = container_traits.find_container_traits(name)
    if ct:
        return ct.remove_defaults(name)
    elif templates.is_instantiation(name):
        tmpl_name, args = templates.split(name)
        for i, arg_name in enumerate(args):
            args[i] = get_partial_name(arg_name.strip())
        return templates.join(tmpl_name, args)
    else:
        return name


class hierarchy_info_t(object):

    """describes class relationship"""

    def __init__(self, related_class=None, access=None, is_virtual=False):
        """creates class that contains partial information about class
        relationship"""
        if related_class:
            assert isinstance(related_class, class_t)
        self._related_class = related_class
        if access:
            assert access in ACCESS_TYPES.ALL
        self._access = access
        self._is_virtual = is_virtual
        self._declaration_path = None
        self._declaration_path_hash = None

    def __eq__(self, other):
        if not isinstance(other, hierarchy_info_t):
            return False
        return (self.declaration_path_hash ==
                other.declaration_path_hash) \
            and self._declaration_path == other._declaration_path \
            and self._access == other._access \
            and self._is_virtual == other._is_virtual

    def __hash__(self):
        return self.declaration_path_hash

    def __ne__(self, other):
        return not self.__eq__(other)

    def __lt__(self, other):
        if not isinstance(other, self.__class__):
            return self.__class__.__name__ < other.__class__.__name__
        return (self.declaration_path, self.access, self.is_virtual) < \
            (other.declaration_path, other.access, other.is_virtual)

    @property
    def related_class(self):
        """reference to base or derived :class:`class <class_t>`"""
        return self._related_class

    @related_class.setter
    def related_class(self, new_related_class):
        if new_related_class:
            assert isinstance(new_related_class, class_t)
        self._related_class = new_related_class
        self._declaration_path = None
        self._declaration_path_hash = None

    @property
    def access(self):
        return self._access

    @access.setter
    def access(self, new_access):
        assert new_access in ACCESS_TYPES.ALL
        self._access = new_access

    # TODO: Why is there an access_type / access which are the same ?
    @property
    def access_type(self):
        """describes :class:`hierarchy type <ACCESS_TYPES>`"""
        return self.access

    @access_type.setter
    def access_type(self, new_access_type):
        self.access = new_access_type

    # TODO: check whether GCC XML support this and if so parser this
    # information
    @property
    def is_virtual(self):
        """indicates whether the inheritance is virtual or not"""
        return self._is_virtual

    @is_virtual.setter
    def is_virtual(self, new_is_virtual):
        self._is_virtual = new_is_virtual

    @property
    def declaration_path(self):
        if self._declaration_path is None:
            self._declaration_path = declaration_utils.declaration_path(
                self.related_class)
        return self._declaration_path

    @property
    def declaration_path_hash(self):
        if self._declaration_path_hash is None:
            self._declaration_path_hash = hash(tuple(self.declaration_path))
        return self._declaration_path_hash


class class_declaration_t(declaration.declaration_t):

    """describes class declaration"""

    def __init__(self, name=''):
        """creates class that describes C++ class declaration
        ( and not definition )"""
        declaration.declaration_t.__init__(self, name)
        self._aliases = []

    def _get__cmp__items(self):
        """implementation details"""
        return []

    def i_depend_on_them(self, recursive=True):
        return []

    @property
    def aliases(self):
        """List of :class:`aliases <typedef_t>` to this instance"""
        return self._aliases

    @aliases.setter
    def aliases(self, new_aliases):
        self._aliases = new_aliases

    def _get_partial_name_impl(self):
        return get_partial_name(self.name)


class class_t(
        scopedef.scopedef_t,
        byte_info.byte_info,
        elaborated_info.elaborated_info):

    """describes class definition"""

    # Can be set from outside
    USE_DEMANGLED_AS_NAME = True

    def __init__(
            self,
            name='',
            class_type=CLASS_TYPES.CLASS,
            is_abstract=False):
        """creates class that describes C++ class definition"""
        scopedef.scopedef_t.__init__(self, name)
        byte_info.byte_info.__init__(self)
        elaborated_info.elaborated_info.__init__(self, class_type)
        if class_type:
            assert class_type in CLASS_TYPES.ALL
        self._class_type = class_type
        self._bases = []
        self._derived = []
        self._is_abstract = is_abstract
        self._public_members = []
        self._private_members = []
        self._protected_members = []
        self._aliases = []
        self._recursive_bases = None
        self._recursive_derived = None
        self._use_demangled_as_name = False

    @property
    def use_demangled_as_name(self):
        return class_t.USE_DEMANGLED_AS_NAME

    @use_demangled_as_name.setter
    def use_demangled_as_name(self, use_demangled_as_name):
        self._use_demangled_as_name = use_demangled_as_name

    def _get_name_impl(self):
        if not self._name:  # class with empty name
            return self._name
        elif self.use_demangled_as_name and self.demangled:

            if not self.cache.demangled_name:
                fname = declaration_utils.full_name(self.parent)
                if fname.startswith('::') and \
                        not self.demangled.startswith('::'):
                    fname = fname[2:]
                if self.demangled.startswith(fname):
                    tmp = self.demangled[len(fname):]  # demangled::name
                    if tmp.startswith('::'):
                        tmp = tmp[2:]
                    if '<' not in tmp and '<' in self._name:
                        # we have template class, but for some reason demangled
                        # name doesn't contain any template
                        # This happens for std::string class, but this breaks
                        # other cases, because this behaviour is not consistent
                        self.cache.demangled_name = self._name
                        return self.cache.demangled_name
                    else:
                        self.cache.demangled_name = tmp
                        return tmp
                else:
                    self.cache.demangled_name = self._name
                    return self._name
            else:
                return self.cache.demangled_name
        else:
            return self._name

    def __str__(self):
        name = declaration_utils.full_name(self)
        if name[:2] == "::":
            name = name[2:]
        return "%s [%s]" % (name, self.class_type)

    def _get__cmp__scope_items(self):
        """implementation details"""
        return [
            self.class_type,
            [declaration_utils.declaration_path(base.related_class) for
             base in self.bases].sort(),
            [declaration_utils.declaration_path(derive.related_class) for
             derive in self.derived].sort(),
            self.is_abstract,
            self.public_members.sort(),
            self.private_members.sort(),
            self.protected_members.sort()]

    def __eq__(self, other):
        if not scopedef.scopedef_t.__eq__(self, other):
            return False
        return self.class_type == other.class_type \
            and [declaration_utils.declaration_path(base.related_class) for
                 base in self.bases].sort() \
            == [declaration_utils.declaration_path(base.related_class) for
                base in other.bases].sort() \
            and [declaration_utils.declaration_path(derive.related_class) for
                 derive in self.derived].sort() \
            == [declaration_utils.declaration_path(derive.related_class) for
                derive in other.derived].sort() \
            and self.is_abstract == other.is_abstract \
            and self.public_members.sort() \
            == other.public_members.sort() \
            and self.private_members.sort() \
            == other.private_members.sort() \
            and self.protected_members.sort() \
            == other.protected_members.sort()

    def __hash__(self):
        return hash(self.class_type)

    @property
    def class_type(self):
        """describes class :class:`type <CLASS_TYPES>`"""
        return self._class_type

    @class_type.setter
    def class_type(self, new_class_type):
        if new_class_type:
            assert new_class_type in CLASS_TYPES.ALL
        self._class_type = new_class_type

    @property
    def bases(self):
        """list of :class:`base classes <hierarchy_info_t>`"""
        return self._bases

    @bases.setter
    def bases(self, new_bases):
        self._bases = new_bases

    @property
    def recursive_bases(self):
        """list of all :class:`base classes <hierarchy_info_t>`"""
        if self._recursive_bases is None:
            to_go = self.bases[:]
            all_bases = []
            while to_go:
                base = to_go.pop()
                if base not in all_bases:
                    all_bases.append(base)
                    to_go.extend(base.related_class.bases)
            self._recursive_bases = all_bases
        return self._recursive_bases

    @property
    def derived(self):
        """list of :class:`derived classes <hierarchy_info_t>`"""
        return self._derived

    @derived.setter
    def derived(self, new_derived):
        self._derived = new_derived

    @property
    def recursive_derived(self):
        """list of all :class:`derive classes <hierarchy_info_t>`"""
        if self._recursive_derived is None:
            to_go = self.derived[:]
            all_derived = []
            while to_go:
                derive = to_go.pop()
                if derive not in all_derived:
                    all_derived.append(derive)
                    to_go.extend(derive.related_class.derived)
            self._recursive_derived = all_derived
        return self._recursive_derived

    @property
    def is_abstract(self):
        """describes whether class abstract or not"""
        return self._is_abstract

    @is_abstract.setter
    def is_abstract(self, is_abstract):
        self._is_abstract = is_abstract

    @property
    def public_members(self):
        """list of all public :class:`members <declarationt_>`"""
        return self._public_members

    @public_members.setter
    def public_members(self, new_public_members):
        self._public_members = new_public_members

    @property
    def private_members(self):
        """list of all private :class:`members <declarationt_>`"""
        return self._private_members

    @private_members.setter
    def private_members(self, new_private_members):
        self._private_members = new_private_members

    @property
    def protected_members(self):
        """list of all protected :class:`members <declarationt_>`"""
        return self._protected_members

    @protected_members.setter
    def protected_members(self, new_protected_members):
        self._protected_members = new_protected_members

    @property
    def aliases(self):
        """List of :class:`aliases <typedef_t>` to this instance"""
        return self._aliases

    @aliases.setter
    def aliases(self, new_aliases):
        self._aliases = new_aliases

    def _get_declarations_impl(self):
        return self.get_members()

    def get_members(self, access=None):
        """
        returns list of members according to access type

        If access equals to None, then returned list will contain all members.
        You should not modify the list content, otherwise different
        optimization data will stop work and may to give you wrong results.

        :param access: describes desired members
        :type access: :class:ACCESS_TYPES

        :rtype: [ members ]
        """
        if access == ACCESS_TYPES.PUBLIC:
            return self.public_members
        elif access == ACCESS_TYPES.PROTECTED:
            return self.protected_members
        elif access == ACCESS_TYPES.PRIVATE:
            return self.private_members
        else:
            all_members = []
            all_members.extend(self.public_members)
            all_members.extend(self.protected_members)
            all_members.extend(self.private_members)
            return all_members

    def adopt_declaration(self, decl, access):
        """adds new declaration to the class

        :param decl: reference to a :class:`declaration_t`

        :param access: member access type
        :type access: :class:ACCESS_TYPES
        """
        if access == ACCESS_TYPES.PUBLIC:
            self.public_members.append(decl)
        elif access == ACCESS_TYPES.PROTECTED:
            self.protected_members.append(decl)
        elif access == ACCESS_TYPES.PRIVATE:
            self.private_members.append(decl)
        else:
            raise RuntimeError("Invalid access type: %s." % access)
        decl.parent = self
        decl.cache.reset()
        decl.cache.access_type = access

    def remove_declaration(self, decl):
        """
        removes decl from  members list

        :param decl: declaration to be removed
        :type decl: :class:`declaration_t`
        """

        access_type = self.find_out_member_access_type(decl)
        if access_type == ACCESS_TYPES.PUBLIC:
            container = self.public_members
        elif access_type == ACCESS_TYPES.PROTECTED:
            container = self.protected_members
        else:  # decl.cache.access_type == ACCESS_TYPES.PRVATE
            container = self.private_members
        del container[container.index(decl)]
        decl.cache.reset()

    def find_out_member_access_type(self, member):
        """
        returns member access type

        :param member: member of the class
        :type member: :class:`declaration_t`

        :rtype: :class:ACCESS_TYPES
        """
        assert member.parent is self
        if not member.cache.access_type:
            if member in self.public_members:
                access_type = ACCESS_TYPES.PUBLIC
            elif member in self.protected_members:
                access_type = ACCESS_TYPES.PROTECTED
            elif member in self.private_members:
                access_type = ACCESS_TYPES.PRIVATE
            else:
                raise RuntimeError(
                    "Unable to find member within internal members list.")
            member.cache.access_type = access_type
            return access_type
        else:
            return member.cache.access_type

    def __find_out_member_dependencies(self, access_type):
        members = self.get_members(access_type)
        answer = []
        for mem in members:
            answer.extend(mem.i_depend_on_them(recursive=True))
        member_ids = set([id(m) for m in members])
        for dependency in answer:
            if id(dependency.declaration) in member_ids:
                dependency.access_type = access_type
        return answer

    def i_depend_on_them(self, recursive=True):

        answer = []

        for base in self.bases:
            answer.append(
                dependency_info_t(
                    self,
                    base.related_class,
                    base.access_type,
                    "base class"))

        if recursive:
            for access_type in ACCESS_TYPES.ALL:
                answer.extend(self.__find_out_member_dependencies(access_type))

        return answer

    def _get_partial_name_impl(self):
        from . import type_traits  # prevent cyclic dependencies
        if type_traits.is_std_string(self):
            return 'string'
        elif type_traits.is_std_wstring(self):
            return 'wstring'
        else:
            return get_partial_name(self.name)

    @property
    def top_class(self):
        """reference to a parent class, which contains this class and defined
        within a namespace

        if this class is defined under a namespace, self will be returned"""
        curr = self
        parent = self.parent
        while isinstance(parent, class_t):
            curr = parent
            parent = parent.parent
        return curr


class_types = (class_t, class_declaration_t)


class impl_details(object):

    @staticmethod
    def dig_declarations(depend_on_it):

        # FIXME: prevent cyclic imports
        from . import type_traits

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


class dependency_info_t(object):

    def __init__(self, decl, depend_on_it, access_type=None, hint=None):
        object.__init__(self)

        assert isinstance(depend_on_it, (class_t, cpptypes.type_t))
        self._decl = decl
        self._depend_on_it = depend_on_it
        self._access_type = access_type
        self._hint = hint

    @property
    def declaration(self):
        return self._decl

    @property
    def decl(self):
        """
        Deprecated since 1.9.0. Will be removed in 2.0.0.

        """
        warnings.warn(
            "The decl attribute is deprecated.\n" +
            "Please use the declaration attribute instead.",
            DeprecationWarning)
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
        for dependency_info in decl.i_depend_on_them():
            for ddecl in dependency_info.find_out_depend_on_it_declarations():
                if ddecl:
                    to_be_included.add(ddecl)

        if isinstance(decl.parent, class_t):
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
