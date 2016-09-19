# Copyright 2014-2016 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
defines class that describes C++ typedef declaration
"""

import warnings
from . import declaration
from . import class_declaration


class typedef_t(declaration.declaration_t):

    """describes C++ typedef declaration"""

    def __init__(self, name='', type=None, decl_type=None):
        """creates class that describes C++ typedef"""

        if type is not None:
            warnings.warn(
                "The type argument is deprecated. \n" +
                "Please use the decl_type argument instead.",
                DeprecationWarning)
            if decl_type is not None:
                raise (
                    "Please use only either the type or " +
                    "decl_type argument.")
            # Still allow to use the old type for the moment.
            decl_type = type

        declaration.declaration_t.__init__(self, name)
        self._decl_type = decl_type

    def _get__cmp__items(self):
        """implementation details"""
        return [self.decl_type]

    def __eq__(self, other):
        if not declaration.declaration_t.__eq__(self, other):
            return False
        return self.decl_type == other.decl_type

    def __hash__(self):
        return super.__hash__(self)

    @property
    def type(self):
        """
        Deprecated since v1.8.0. Will be removed in v1.9.0

        """

        warnings.warn(
            "typedef_t.type is deprecated.\n" +
            "Please use typedef_t.decl_type instead.", DeprecationWarning)
        return self._decl_type

    @type.setter
    def type(self, _decl_type):
        """
        Deprecated since v1.8.0. Will be removed in v1.9.0

        """

        warnings.warn(
            "typedef_t.type is deprecated.\n" +
            "Please use typedef_t.decl_type instead.", DeprecationWarning)
        self._decl_type = _decl_type

    @property
    def decl_type(self):
        """reference to the original :class:`decl_type <type_t>`"""
        return self._decl_type

    @decl_type.setter
    def decl_type(self, decl_type):
        self._decl_type = decl_type

    def i_depend_on_them(self, recursive=True):
        return [class_declaration.dependency_info_t(self, self.decl_type)]

    @property
    def byte_size(self):
        """Size of this type in bytes @type: int"""
        return self._decl_type.byte_size

    @property
    def byte_align(self):
        """alignment of this type in bytes @type: int"""
        return self._decl_type.byte_align
