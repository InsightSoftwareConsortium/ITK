# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

from . import namespace
from . import calldef
from . import cpptypes
from . import type_traits
from . import class_declaration


class impl_details(object):

    """implementation details"""
    @staticmethod
    def is_defined_in_xxx(xxx, cls):
        """
        Small helper method that checks whether the class `cls` is defined
        under `::xxx` namespace
        """
        if not cls.parent:
            return False

        if not isinstance(cls.parent, namespace.namespace_t):
            return False

        if xxx != cls.parent.name:
            return False

        xxx_ns = cls.parent
        if not xxx_ns.parent:
            return False

        if not isinstance(xxx_ns.parent, namespace.namespace_t):
            return False

        if xxx_ns.parent.name != '::':
            return False

        global_ns = xxx_ns.parent
        return None is global_ns.parent

    @staticmethod
    def find_value_type(global_ns, value_type_str):
        """implementation details"""
        if not value_type_str.startswith('::'):
            value_type_str = '::' + value_type_str
        found = global_ns.decls(
            name=value_type_str,
            function=lambda decl: not isinstance(decl, calldef.calldef_t),
            allow_empty=True)
        if not found:
            no_global_ns_value_type_str = value_type_str[2:]
            if no_global_ns_value_type_str in cpptypes.FUNDAMENTAL_TYPES:
                return cpptypes.FUNDAMENTAL_TYPES[no_global_ns_value_type_str]
            elif type_traits.is_std_string(value_type_str):
                string_ = global_ns.typedef('::std::string')
                return type_traits.remove_declarated(string_)
            elif type_traits.is_std_wstring(value_type_str):
                string_ = global_ns.typedef('::std::wstring')
                return type_traits.remove_declarated(string_)
            else:
                value_type_str = no_global_ns_value_type_str
                has_const = value_type_str.startswith('const ')
                if has_const:
                    value_type_str = value_type_str[len('const '):]
                has_pointer = value_type_str.endswith('*')
                if has_pointer:
                    value_type_str = value_type_str[:-1]
                found = None
                if has_const or has_pointer:
                    found = impl_details.find_value_type(
                        global_ns,
                        value_type_str)
                if not found:
                    return None
                else:
                    if isinstance(found, class_declaration.class_types):
                        return cpptypes.declarated_t(found)
                    if has_const:
                        return cpptypes.const_t(found)
                    if has_pointer:
                        return cpptypes.pointer_t(found)
        if len(found) == 1:
            return found[0]
        else:
            return None
