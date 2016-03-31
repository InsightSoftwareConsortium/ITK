# Copyright 2014-2015 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
defines classes, that describes C++ classes

This modules contains definition for next C++ declarations:
    - class definition
    - class declaration
    - small helper class for describing C++ class hierarchy
"""

from . import scopedef
from . import algorithm
from . import declaration
from . import dependencies
from pygccxml import utils


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
    from . import templates
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
            assert(isinstance(related_class, class_t))
        self._related_class = related_class
        if access:
            assert(access in ACCESS_TYPES.ALL)
        self._access = access
        self._is_virtual = is_virtual

    def __eq__(self, other):
        if not isinstance(other, hierarchy_info_t):
            return False
        return algorithm.declaration_path(self.related_class) == \
            algorithm.declaration_path(other.related_class) \
            and self.access == other.access \
            and self.is_virtual == other.is_virtual

    def __hash__(self):
        return hash(algorithm.declaration_path(self.related_class))

    def __ne__(self, other):
        return not self.__eq__(other)

    def __lt__(self, other):
        if not isinstance(other, self.__class__):
            return self.__class__.__name__ < other.__class__.__name__
        return (
            algorithm.declaration_path(self.related_class),
            self.access,
            self.is_virtual) < (
            algorithm.declaration_path(
                other.related_class),
            other.access,
            self.is_virtual)

    @property
    def related_class(self):
        "reference to base or derived :class:`class <class_t>`"
        return self._related_class

    @related_class.setter
    def related_class(self, new_related_class):
        if new_related_class:
            assert(isinstance(new_related_class, class_t))
        self._related_class = new_related_class

    @property
    def access(self):
        return self._access

    @access.setter
    def access(self, new_access):
        assert(new_access in ACCESS_TYPES.ALL)
        self._access = new_access

    # TODO: Why is there an access_type / access which are the same ?
    @property
    def access_type(self):
        "describes :class:`hierarchy type <ACCESS_TYPES>`"
        return self.access

    @access_type.setter
    def access_type(self, new_access_type):
        self.access = new_access_type

    # TODO: check whether GCC XML support this and if so parser this
    # information
    @property
    def is_virtual(self):
        "indicates whether the inheritance is virtual or not"
        return self._is_virtual

    @is_virtual.setter
    def is_virtual(self, new_is_virtual):
        self._is_virtual = new_is_virtual


class class_declaration_t(declaration.declaration_t):

    """describes class declaration"""

    def __init__(self, name=''):
        """creates class that describes C++ class declaration
        ( and not definition )"""
        declaration.declaration_t.__init__(self, name)
        self._aliases = []
        self._container_traits = None
        self._container_traits_set = False

    def _get__cmp__items(self):
        """implementation details"""
        return []

    def i_depend_on_them(self, recursive=True):
        return []

    @property
    def aliases(self):
        "List of :class:`aliases <typedef_t>` to this instance"
        return self._aliases

    @aliases.setter
    def aliases(self, new_aliases):
        self._aliases = new_aliases

    @property
    def container_traits(self):
        """reference to :class:`container_traits_impl_t` or None"""
        if self._container_traits_set is False:
            from . import container_traits  # prevent cyclic dependencies
            self._container_traits_set = True
            self._container_traits = container_traits.find_container_traits(
                self)
        return self._container_traits

    def _get_partial_name_impl(self):
        return get_partial_name(self.name)


class class_t(scopedef.scopedef_t):

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
        if class_type:
            assert(class_type in CLASS_TYPES.ALL)
        self._class_type = class_type
        self._bases = []
        self._derived = []
        self._is_abstract = is_abstract
        self._public_members = []
        self._private_members = []
        self._protected_members = []
        self._aliases = []
        self._byte_size = 0
        self._byte_align = 0
        self._container_traits = None
        self._container_traits_set = False
        self._recursive_bases = None
        self._recursive_derived = None
        self._use_demangled_as_name = False

    @property
    def use_demangled_as_name(self):
        if "GCC" in utils.xml_generator:
            return class_t.USE_DEMANGLED_AS_NAME
        elif "CastXML" in utils.xml_generator:
            return False

    @use_demangled_as_name.setter
    def use_demangled_as_name(self, use_demangled_as_name):
        self._use_demangled_as_name = use_demangled_as_name

    def _get_name_impl(self):
        if not self._name:  # class with empty name
            return self._name
        elif self.use_demangled_as_name and self.demangled:

            if not self.cache.demangled_name:
                fname = algorithm.full_name(self.parent)
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
        name = algorithm.full_name(self)
        if name[:2] == "::":
            name = name[2:]
        return "%s [%s]" % (name, self.class_type)

    def _get__cmp__scope_items(self):
        """implementation details"""
        return [self.class_type,
                self._sorted_list(
                    [algorithm.declaration_path(base.related_class)
                        for base in self.bases]),
                self._sorted_list(
                    [algorithm.declaration_path(derive.related_class)
                        for derive in self.derived]),
                self.is_abstract,
                self._sorted_list(self.public_members),
                self._sorted_list(self.private_members),
                self._sorted_list(self.protected_members)]

    def __eq__(self, other):
        if not scopedef.scopedef_t.__eq__(self, other):
            return False
        return self.class_type == other.class_type \
            and self._sorted_list(
                [algorithm.declaration_path(base.related_class)
                    for base in self.bases]) \
            == other._sorted_list(
                [algorithm.declaration_path(base.related_class)
                    for base in other.bases]) \
            and self._sorted_list(
                [algorithm.declaration_path(derive.related_class)
                    for derive in self.derived]) \
            == other._sorted_list(
                [algorithm.declaration_path(derive.related_class)
                    for derive in other.derived]) \
            and self.is_abstract == other.is_abstract \
            and self._sorted_list(self.public_members) \
            == other._sorted_list(other.public_members) \
            and self._sorted_list(self.private_members) \
            == other._sorted_list(other.private_members) \
            and self._sorted_list(self.protected_members) \
            == self._sorted_list(other.protected_members)

    def __hash__(self):
        return hash(self.class_type)

    @property
    def class_type(self):
        "describes class :class:`type <CLASS_TYPES>`"
        return self._class_type

    @class_type.setter
    def class_type(self, new_class_type):
        if new_class_type:
            assert(new_class_type in CLASS_TYPES.ALL)
        self._class_type = new_class_type

    @property
    def bases(self):
        "list of :class:`base classes <hierarchy_info_t>`"
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
        "list of :class:`derived classes <hierarchy_info_t>`"
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
        "describes whether class abstract or not"
        return self._is_abstract

    @is_abstract.setter
    def is_abstract(self, is_abstract):
        self._is_abstract = is_abstract

    @property
    def public_members(self):
        "list of all public :class:`members <declarationt_>`"
        return self._public_members

    @public_members.setter
    def public_members(self, new_public_members):
        self._public_members = new_public_members

    @property
    def private_members(self):
        "list of all private :class:`members <declarationt_>`"
        return self._private_members

    @private_members.setter
    def private_members(self, new_private_members):
        self._private_members = new_private_members

    @property
    def protected_members(self):
        "list of all protected :class:`members <declarationt_>`"
        return self._protected_members

    @protected_members.setter
    def protected_members(self, new_protected_members):
        self._protected_members = new_protected_members

    @property
    def aliases(self):
        "List of :class:`aliases <typedef_t>` to this instance"
        return self._aliases

    @aliases.setter
    def aliases(self, new_aliases):
        self._aliases = new_aliases

    @property
    def byte_size(self):
        "Size of this class in bytes @type: int"
        return self._byte_size

    @byte_size.setter
    def byte_size(self, new_byte_size):
        self._byte_size = new_byte_size

    @property
    def byte_align(self):
        "Alignment of this class in bytes @type: int"
        return self._byte_align

    @byte_align.setter
    def byte_align(self, new_byte_align):
        self._byte_align = new_byte_align

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
                dependencies.dependency_info_t(
                    self,
                    base.related_class,
                    base.access_type,
                    "base class"))

        if recursive:
            for access_type in ACCESS_TYPES.ALL:
                answer.extend(self.__find_out_member_dependencies(access_type))

        return answer

    @property
    def container_traits(self):
        """reference to :class:`container_traits_impl_t` or None"""
        if self._container_traits_set is False:
            from . import container_traits  # prevent cyclic dependencies
            self._container_traits_set = True
            self._container_traits = container_traits.find_container_traits(
                self)
        return self._container_traits

    def find_copy_constructor(self):
        copy_ = self.constructors(
            lambda x: x.is_copy_constructor,
            recursive=False,
            allow_empty=True)
        if copy_:
            return copy_[0]
        else:
            return None

    def find_trivial_constructor(self):
        trivial = self.constructors(
            lambda x: x.is_trivial_constructor,
            recursive=False,
            allow_empty=True)
        if trivial:
            return trivial[0]
        else:
            return None

    def _get_partial_name_impl(self):
        from . import type_traits  # prevent cyclic dependencies
        if type_traits.is_std_string(self):
            return 'string'
        elif type_traits.is_std_wstring(self):
            return 'wstring'
        else:
            return get_partial_name(self.name)

    def find_noncopyable_vars(self):
        """returns list of all `noncopyable` variables"""

        from . import type_traits as tt  # prevent cyclic dependencies

        logger = utils.loggers.cxx_parser
        mvars = self.variables(
            lambda v: not v.type_qualifiers.has_static,
            recursive=False,
            allow_empty=True)
        noncopyable_vars = []

        message = (
            "__contains_noncopyable_mem_var - %s - TRUE - " +
            "containes const member variable")

        for mvar in mvars:

            type_ = tt.remove_reference(mvar.type)

            if tt.is_const(type_):
                no_const = tt.remove_const(type_)
                if tt.is_fundamental(no_const) or tt.is_enum(no_const):
                    logger.debug((
                        message + "- fundamental or enum")
                        % self.decl_string)
                    noncopyable_vars.append(mvar)
                if tt.is_class(no_const):
                    logger.debug((message + " - class") % self.decl_string)
                    noncopyable_vars.append(mvar)
                if tt.is_array(no_const):
                    logger.debug((message + " - array") % self.decl_string)
                    noncopyable_vars.append(mvar)

            if tt.class_traits.is_my_case(type_):

                cls = tt.class_traits.get_declaration(type_)
                if tt.is_noncopyable(cls):
                    logger.debug((
                        message + " - class that is not copyable")
                        % self.decl_string)
                    noncopyable_vars.append(mvar)

        logger.debug((
            "__contains_noncopyable_mem_var - %s - FALSE - doesn't " +
            "contain noncopyable members") % self.decl_string)

        return noncopyable_vars

    @property
    def has_vtable(self):
        """True, if class has virtual table, False otherwise"""
        from . import calldef
        return bool(
            self.calldefs(
                lambda f: isinstance(f, calldef.member_function_t) and
                f.virtuality != calldef.VIRTUALITY_TYPES.NOT_VIRTUAL,
                recursive=False,
                allow_empty=True))

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
