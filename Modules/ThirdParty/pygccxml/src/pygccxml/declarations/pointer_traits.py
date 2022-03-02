# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

from . import templates
from . import type_traits
from . import class_declaration
from . import class_declaration_traits
from . import class_traits
from . import traits_impl_details
from . import runtime_errors


class internal_type_traits:

    """small convenience class, which provides access to internal types"""
    # TODO: add exists function
    @staticmethod
    def get_by_name(type_, name):
        if class_traits.is_my_case(type_):
            cls = class_traits.get_declaration(type_)
            return type_traits.remove_declarated(
                cls.typedef(name, recursive=False).decl_type)
        elif class_declaration_traits.is_my_case(type_):
            cls = class_declaration_traits.get_declaration(type_)
            value_type_str = templates.args(cls.name)[0]
            ref = traits_impl_details.impl_details.find_value_type(
                cls.top_parent, value_type_str)
            if ref:
                return ref
            else:
                raise RuntimeError((
                    "Unable to find reference to internal " +
                    "type '%s' in type '%s'.") % (name, cls.decl_string))
        else:
            raise RuntimeError(
                ("Unable to find reference to internal type '%s'"
                 "in type '%s'.") % (name, type_.decl_string))


class smart_pointer_traits:

    """implements functionality, needed for convenient work with
    smart pointers"""

    @staticmethod
    def is_smart_pointer(type_):
        """returns True, if type represents instantiation of
        `boost::shared_ptr` or `std::shared_ptr`, False otherwise"""
        type_ = type_traits.remove_alias(type_)
        type_ = type_traits.remove_cv(type_)
        type_ = type_traits.remove_declarated(type_)
        if not isinstance(type_,
                          (class_declaration.class_declaration_t,
                           class_declaration.class_t)):
            return False
        if not (
                traits_impl_details.impl_details.is_defined_in_xxx(
                    'boost', type_) or
                traits_impl_details.impl_details.is_defined_in_xxx(
                    'std', type_)):
            return False
        return type_.decl_string.startswith('::boost::shared_ptr<') or \
            type_.decl_string.startswith('::std::shared_ptr<')

    @staticmethod
    def value_type(type_):
        """returns reference to `boost::shared_ptr` \
        or `std::shared_ptr` value type"""
        if not smart_pointer_traits.is_smart_pointer(type_):
            raise TypeError(
                'Type "%s" is not an instantiation of \
                boost::shared_ptr or std::shared_ptr' %
                type_.decl_string)
        try:
            return internal_type_traits.get_by_name(type_, "element_type")
        except runtime_errors.declaration_not_found_t:
            return _search_in_bases(type_)


class auto_ptr_traits:

    """implements functionality, needed for convenient work with
    `std::auto_ptr` pointers"""

    @staticmethod
    def is_smart_pointer(type_):
        """returns True, if type represents instantiation of
        `boost::shared_ptr`, False otherwise"""
        type_ = type_traits.remove_alias(type_)
        type_ = type_traits.remove_cv(type_)
        type_ = type_traits.remove_declarated(type_)
        if not isinstance(type_,
                          (class_declaration.class_declaration_t,
                           class_declaration.class_t)):
            return False
        if not traits_impl_details.impl_details.is_defined_in_xxx(
                'std', type_):
            return False
        return type_.decl_string.startswith('::std::auto_ptr<')

    @staticmethod
    def value_type(type_):
        """returns reference to `boost::shared_ptr` value type"""
        if not auto_ptr_traits.is_smart_pointer(type_):
            raise TypeError(
                'Type "%s" is not instantiation of std::auto_ptr' %
                type_.decl_string)
        try:
            return internal_type_traits.get_by_name(type_, "element_type")
        except runtime_errors.declaration_not_found_t:
            return _search_in_bases(type_)


def _search_in_bases(type_):
    """Implementation detail."""
    found = False
    for base_type in type_.declaration.bases:
        try:
            found = internal_type_traits.get_by_name(
                base_type.related_class, "element_type")
        except runtime_errors.declaration_not_found_t:
            pass
        if found:
            return found
    raise RuntimeError(
        ("Unable to find 'element_type' declaration '%s'"
         "in type '%s'.") % type_.decl_string)
