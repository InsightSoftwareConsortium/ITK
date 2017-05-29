# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Defines :class:`pygccxml.declarations.declaration_t` class - all declarations
base class.

"""

import warnings

from . import declaration_utils
from . import algorithms_cache


class declaration_t(object):
    """
    Base class for all classes that represent a C++ declaration.

    """

    def __init__(
            self,
            name='',
            location=None,
            is_artificial=False,
            mangled=None,
            demangled=None,
            attributes=None):
        self._name = name
        self._location = location
        self._is_artificial = is_artificial
        self._mangled = mangled
        self._demangled = demangled
        self._attributes = attributes
        self._parent = None
        self._cache = algorithms_cache.declaration_algs_cache_t()
        self._partial_name = None
        self._decorated_name = None

    def __str__(self):
        """
        Default __str__ method.

        This version just returns the decl_string and the class.
        Derived classes may override this method to provide more detailed
        information.

        A __str__ method for a declaration should always provide enough
        information so that it uniquely identifies the declaration and
        the user is able to find the declaration in his source code.

        """

        name = self.decl_string
        if name[:2] == "::":
            name = name[2:]
        # Append the declaration class
        cls = self.__class__.__name__
        if cls[-2:] == "_t":
            cls = cls[:-2]
        cls = cls.replace('_', ' ')

        return "%s [%s]" % (name, cls)

    def _get__cmp__items(self):
        """
        Implementation detail.

        """
        # Every derived class should implement this method. This method should
        # return a list of items, that should be compared.

        print(
            '_get__cmp__items not implemented for class ',
            self.__class__.__name__)

        raise NotImplementedError()

    def _get__cmp__data(self):
        """
        Implementation detail.

        """
        if self._cache.cmp_data is None:
            cmp_data = [
                declaration_utils.declaration_path(self.parent),
                self.name,
                self.location]
            cmp_data.extend(self._get__cmp__items())
            self._cache.cmp_data = cmp_data

        return self._cache.cmp_data

    def __eq__(self, other):
        """
        This function will return true, if both declarations refers to the same
        object.
        This function could be implemented in terms of _get__cmp__data, but in
        this case it will downgrade performance. self.mangled property is not
        compared, because it could be changed from one compilation time to an
        other.

        """

        if not isinstance(other, self.__class__):
            return False
        return self.name == other.name \
            and self.location == other.location \
            and declaration_utils.declaration_path(self.parent) \
            == declaration_utils.declaration_path(other.parent)

    def __hash__(self):
        return (hash(self.__class__) ^
                hash(self.name) ^
                hash(self.location))

    def __ne__(self, other):
        """
        Return not self.__eq__( other ).

        """

        return not self.__eq__(other)

    def __lt__(self, other):
        """
        .. code-block:: python

           if not isinstance( other, self.__class__ ):
               return self.__class__.__name__ < other.__class__.__name__
           return self._get__cmp__data() < other._get__cmp__data()

        """

        if not isinstance(other, self.__class__):
            return self.__class__.__name__ < other.__class__.__name__
        return self._get__cmp__data() < other._get__cmp__data()

    def _get_name_impl(self):
        return self._name

    def _on_rename(self):
        """
        Placeholder method, is redefined in child class.

        """

        pass

    @property
    def name(self):
        """
        Declaration name
           @type: str

        """

        return self._get_name_impl()

    @name.setter
    def name(self, new_name):
        previous_name = self._name
        self._name = new_name
        self._partial_name = None
        self.cache.reset_name_based()
        if previous_name:
            # There was a reset of the name
            self._on_rename()

    def _get_partial_name_impl(self):
        return self.name

    @property
    def partial_name(self):
        """
        Declaration name, without template default arguments.

        Right now std containers is the only classes that support this
        functionality.

        """

        if None is self._partial_name:
            self._partial_name = self._get_partial_name_impl()

        return self._partial_name

    @property
    def parent(self):
        """
        Reference to parent declaration.

           @type: declaration_t

        """

        return self._parent

    @parent.setter
    def parent(self, new_parent):
        if new_parent:
            assert isinstance(new_parent, declaration_t)

        self._parent = new_parent

    @property
    def top_parent(self):
        """
        Reference to top parent declaration.

           @type: declaration_t

        """
        parent = self.parent
        while parent is not None:
            if parent.parent is None:
                return parent
            else:
                parent = parent.parent
        return self

    @property
    def location(self):
        """
        Location of the declaration within source file

           @type: :class:`location_t`

        """

        return self._location

    @location.setter
    def location(self, new_location):
        self._location = new_location

    @property
    def is_artificial(self):
        """
        Describes whether declaration is compiler generated or not

           @type: bool

        """

        return self._is_artificial

    @is_artificial.setter
    def is_artificial(self, new_artificial):
        self._is_artificial = bool(new_artificial)

    def get_mangled_name(self):
        return self._mangled

    @property
    def mangled(self):
        """
        Unique declaration name generated by the compiler.

        For GCCXML, you can get the mangled name for all the declarations.
        When using CastXML, calling mangled is only allowed on functions
        and variables. For other declarations it will raise an exception.

        :return: the mangled name
        :rtype: str

        """

        return self._mangled

    @mangled.setter
    def mangled(self, mangled):
        self._mangled = mangled

    @property
    def demangled(self):
        """
        Declaration name, reconstructed from GCCXML generated unique name.

           @type: str

        """
        return self._demangled

    @demangled.setter
    def demangled(self, demangled):
        self._demangled = demangled

    @property
    def decorated_name(self):
        """
        Unique declaration name extracted from a binary file
        ( .map, .dll, .so, etc ).

           @type: str

        """
        warnings.warn(
            "The decorated_name attribute is deprecated. See the changelog.",
            DeprecationWarning)
        # Deprecated since 1.9.0, will be removed in 2.0.0
        return self._decorated_name

    @decorated_name.setter
    def decorated_name(self, decorated_name):
        warnings.warn(
            "The decorated_name attribute is deprecated. See the changelog.",
            DeprecationWarning)
        # Deprecated since 1.9.0, will be removed in 2.0.0
        self._decorated_name = decorated_name

    @property
    def attributes(self):
        """
        GCCXML attributes, set using __attribute__((gccxml("...")))

           @type: str

        """

        return self._attributes

    @attributes.setter
    def attributes(self, attributes):
        self._attributes = attributes

    def create_decl_string(self, with_defaults=True):
        return declaration_utils.full_name(self, with_defaults)

    @property
    def decl_string(self):
        """
        Declaration full name.

        """

        return self.create_decl_string()

    @property
    def partial_decl_string(self):
        """
        Declaration full name.

        """

        return self.create_decl_string(with_defaults=False)

    @property
    def cache(self):
        """
        Implementation detail.

        Reference to instance of :class:`algorithms_cache_t` class.

        """

        return self._cache

    def i_depend_on_them(self, recursive=True):
        """
        Return list of all types and declarations the declaration depends on

        """
        raise NotImplementedError()
