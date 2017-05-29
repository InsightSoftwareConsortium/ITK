# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Defines classes that will keep results of different calculations.

"""


class declaration_algs_cache_t(object):

    def __init__(self):
        object.__init__(self)
        self._enabled = True
        self._full_name = None
        self._full_partial_name = None
        self._access_type = None
        self._demangled_name = None
        self._declaration_path = None
        self._partial_declaration_path = None
        self._container_key_type = None
        self._container_element_type = None
        self._cmp_data = None
        self._normalized_name = None
        self._normalized_partial_name = None
        self._normalized_full_name_true = None
        self._normalized_full_name_false = None
        self._container_traits = None

    def disable(self):
        self._enabled = False

    def enable(self):
        self._enabled = True

    @property
    def enabled(self):
        return self._enabled

    @property
    def full_name(self):
        return self._full_name

    @full_name.setter
    def full_name(self, fname):
        if not self.enabled:
            fname = None
        self._full_name = fname

    @property
    def container_traits(self):
        return self._container_traits

    @container_traits.setter
    def container_traits(self, container_traits):
        if not self.enabled:
            container_traits = None
        self._container_traits = container_traits

    @property
    def full_partial_name(self):
        return self._full_partial_name

    @full_partial_name.setter
    def full_partial_name(self, fname):
        if not self.enabled:
            fname = None
        self._full_partial_name = fname

    @property
    def access_type(self):
        return self._access_type

    @access_type.setter
    def access_type(self, access_type):
        if not self.enabled:
            access_type = None
        self._access_type = access_type

    @property
    def demangled_name(self):
        return self._demangled_name

    @demangled_name.setter
    def demangled_name(self, demangled_name):
        if not self.enabled:
            demangled_name = None
        self._demangled_name = demangled_name

    @property
    def declaration_path(self):
        return self._declaration_path

    @declaration_path.setter
    def declaration_path(self, declaration_path):
        if not self.enabled:
            declaration_path = None
        self._declaration_path = declaration_path

    @property
    def partial_declaration_path(self):
        return self._partial_declaration_path

    @partial_declaration_path.setter
    def partial_declaration_path(self, partial_declaration_path):
        if not self.enabled:
            partial_declaration_path = None
        self._partial_declaration_path = partial_declaration_path

    @property
    def container_element_type(self):
        return self._container_element_type

    @container_element_type.setter
    def container_element_type(self, etype):
        if not self.enabled:
            etype = None
        self._container_element_type = etype

    @property
    def container_key_type(self):
        return self._container_key_type

    @container_key_type.setter
    def container_key_type(self, ktype):
        if not self.enabled:
            ktype = None
        self._container_key_type = ktype

    @property
    def normalized_name(self):
        return self._normalized_name

    @normalized_name.setter
    def normalized_name(self, normalized_name):
        if not self.enabled:
            normalized_name = None
        self._normalized_name = normalized_name

    @property
    def normalized_partial_name(self):
        return self._normalized_partial_name

    @normalized_partial_name.setter
    def normalized_partial_name(self, normalized_partial_name):
        if not self.enabled:
            normalized_partial_name = None
        self._normalized_partial_name = normalized_partial_name

    @property
    def normalized_full_name_true(self):
        return self._normalized_full_name_true

    @normalized_full_name_true.setter
    def normalized_full_name_true(self, normalized_full_name_true):
        if not self.enabled:
            normalized_full_name_true = None
        self._normalized_full_name_true = normalized_full_name_true

    @property
    def normalized_full_name_false(self):
        return self._normalized_full_name_false

    @normalized_full_name_false.setter
    def normalized_full_name_false(self, normalized_full_name_false):
        if not self.enabled:
            normalized_full_name_false = None
        self._normalized_full_name_false = normalized_full_name_false

    @property
    def cmp_data(self):
        """Data used for comparison between declarations."""
        return self._cmp_data

    @cmp_data.setter
    def cmp_data(self, cmp_data):
        """Data used for comparison between declarations."""
        if not self.enabled:
            cmp_data = None
        self._cmp_data = cmp_data

    def reset(self):
        self.full_name = None
        self.full_partial_name = None
        self.access_type = None
        self.demangled_name = None
        self.declaration_path = None
        self.partial_declaration_path = None
        self.container_key_type = None
        self.container_element_type = None
        self.cmp_data = None
        self.normalized_name = None
        self.normalized_partial_name = None
        self.normalized_full_name_true = None
        self.normalized_full_name_false = None
        self.container_traits = None

    def reset_name_based(self):
        self.full_name = None
        self.full_partial_name = None
        self.demangled_name = None
        self.declaration_path = None
        self.partial_declaration_path = None
        self.container_key_type = None
        self.container_element_type = None
        self.cmp_data = None
        self.normalized_name = None
        self.normalized_partial_name = None
        self.normalized_full_name_true = None
        self.normalized_full_name_false = None
        self.container_traits = None

    def reset_access_type(self):
        self.access_type = None


class type_algs_cache_t(object):
    enabled = True

    @staticmethod
    def disable():
        type_algs_cache_t.enabled = False

    @staticmethod
    def enable():
        type_algs_cache_t.enabled = True

    def __init__(self):
        object.__init__(self)
        self._remove_alias = None
        self._decl_string = None
        self._partial_decl_string = None

    @property
    def remove_alias(self):
        return self._remove_alias

    @remove_alias.setter
    def remove_alias(self, remove_alias):
        if not type_algs_cache_t.enabled:
            remove_alias = None
        self._remove_alias = remove_alias

    @property
    def decl_string(self):
        return self._decl_string

    @decl_string.setter
    def decl_string(self, decl_string):
        if not type_algs_cache_t.enabled:
            decl_string = None
        self._decl_string = decl_string

    @property
    def partial_decl_string(self):
        return self._partial_decl_string

    @partial_decl_string.setter
    def partial_decl_string(self, partial_decl_string):
        if not type_algs_cache_t.enabled:
            partial_decl_string = None
        self._partial_decl_string = partial_decl_string

    def reset(self):
        self.remove_alias = None
        self.decl_string = None
        self.partial_decl_string = None
