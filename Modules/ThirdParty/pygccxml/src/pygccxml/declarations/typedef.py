# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
defines class that describes C++ typedef declaration
"""

from . import declaration
from . import byte_info


class typedef_t(declaration.declaration_t, byte_info.byte_info):

    """describes C++ typedef declaration"""

    def __init__(self, name='', decl_type=None):
        """creates class that describes C++ typedef"""
        declaration.declaration_t.__init__(self, name)
        byte_info.byte_info.__init__(self)
        self._decl_type = decl_type
        if not isinstance(decl_type, str) and decl_type is not None:
            self.byte_size = decl_type.byte_size
            self.byte_align = decl_type.byte_align

    def _get__cmp__items(self):
        """implementation details"""
        return [self.decl_type]

    def __eq__(self, other):
        if not declaration.declaration_t.__eq__(self, other):
            return False
        return self.decl_type == other.decl_type

    __hash__ = declaration.declaration_t.__hash__

    @property
    def decl_type(self):
        """reference to the original :class:`decl_type <type_t>`"""
        return self._decl_type

    @decl_type.setter
    def decl_type(self, decl_type):
        self._decl_type = decl_type
        self.byte_size = decl_type.byte_size
        self.byte_align = decl_type.byte_align

    def i_depend_on_them(self, recursive=True):
        self._warn_deprecated()
        # Deprecated method. The cyclic import will be removed with the method
        # in the next release, so we can disable the cyclic import check here.
        from . import dependencies  # pylint: disable=R0401
        return [dependencies.dependency_info_t(self, self.decl_type)]
