# Copyright 2014-2015 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Defines classes that will keep results of different calculations.

"""

from .. import utils


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
        if "GCC" in utils.xml_generator:
            return self._demangled_name
        elif "CastXML" in utils.xml_generator:
            raise Exception("Demangled name is not available with CastXML.")

    @demangled_name.setter
    def demangled_name(self, demangled_name):
        if not self.enabled:
            demangled_name = None
        if "GCC" in utils.xml_generator:
            self._demangled_name = demangled_name
        elif "CastXML" in utils.xml_generator:
            raise Exception("Demangled name is not available with CastXML.")

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

    def reset(self):
        self.full_name = None
        self.full_partial_name = None
        self.access_type = None
        if "GCCXML" in utils.xml_generator:
            self.demangled_name = None
        self.declaration_path = None
        self.partial_declaration_path = None
        self.container_key_type = None
        self.container_element_type = None

    def reset_name_based(self):
        self.full_name = None
        self.full_partial_name = None
        if "GCCXML" in utils.xml_generator:
            self.demangled_name = None
        self.declaration_path = None
        self.partial_declaration_path = None
        self.container_key_type = None
        self.container_element_type = None

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

    @property
    def remove_alias(self):
        return self._remove_alias

    @remove_alias.setter
    def remove_alias(self, remove_alias):
        if not type_algs_cache_t.enabled:
            remove_alias = None
        self._remove_alias = remove_alias

    def reset(self):
        self.remove_alias = None
