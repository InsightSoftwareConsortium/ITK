# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

import re
from .. import utils
from .. import declarations
from ..declarations import type_traits


class default_argument_patcher_t(object):

    def __init__(self, enums, cxx_std):
        object.__init__(self)
        self.__enums = enums
        self.__cxx_std = cxx_std

    def __call__(self, decl):
        for arg in decl.arguments:
            if not arg.default_value:
                continue
            fixer = self.__find_fixer(decl, arg)
            if fixer:
                arg.default_value = fixer(decl, arg)

    def __find_fixer(self, func, arg):
        if self.__is_unqualified_enum(func, arg):
            return self.__fix_unqualified_enum
        elif self.__is_double_call(func, arg):
            return self.__fix_double_call
        elif self.__is_invalid_integral(func, arg):
            return self.__fix_invalid_integral
        elif self.__is_constructor_call(func, arg):
            return self.__fix_constructor_call
        else:
            return None

    @staticmethod
    def __join_names(prefix, suffix):
        if prefix == '::':
            return '::' + suffix
        else:
            return prefix + '::' + suffix

    @staticmethod
    def __is_unqualified_enum(func, arg):
        type_ = declarations.remove_reference(
            declarations.remove_cv(arg.decl_type))
        if not declarations.is_enum(type_):
            return False
        enum_type = declarations.enum_declaration(type_)
        # GCCXML does not qualify an enum value in the default argument
        # but CastXML does. Split the default value and use only the
        # enum value for fixing it.
        return enum_type.has_value_name(
            arg.default_value.split('::')[-1])

    def __fix_unqualified_enum(self, func, arg):
        type_ = declarations.remove_reference(
            declarations.remove_cv(arg.decl_type))
        enum_type = declarations.enum_declaration(type_)
        if self.__cxx_std.is_cxx11_or_greater:
            qualifier_decl_string = enum_type.decl_string
        else:
            qualifier_decl_string = enum_type.parent.decl_string
        return self.__join_names(
            qualifier_decl_string,
            arg.default_value.split('::')[-1])

    @staticmethod
    def __is_invalid_integral(func, arg):
        type_ = declarations.remove_reference(
            declarations.remove_cv(arg.decl_type))
        if not declarations.is_integral(type_):
            return False
        try:
            int(arg.default_value)
            return False
        except ValueError:
            # The arg.default_value string could not be converted to int
            return True

    def __fix_invalid_integral(self, func, arg):
        try:
            int(arg.default_value)
            return arg.default_value
        except ValueError:
            # The arg.default_value string could not be converted to int
            pass

        try:
            int(arg.default_value, 16)
            if utils.get_architecture() == 64:
                # on 64 bit architecture, gccxml reports 0fffff, which is
                # valid number the problem is that in this case it is so
                # buggy so pygccxml can not fix it users will have to fix the
                # default value manually
                return arg.default_value
            default_value = arg.default_value.lower()
            found_hex = [ch for ch in default_value if ch in 'abcdef']
            if found_hex and not default_value.startswith('0x'):
                int('0x' + default_value, 16)
                return '0x' + default_value
        except ValueError:
            # The arg.default_value string could not be converted to int
            pass

        # may be we deal with enum
        # CastXML qualifies the enum value with enum type, so split the
        # argument and use only the enum value
        enum_value = arg.default_value.split('::')[-1]
        parent = func.parent
        while parent:
            found = self.__find_enum(parent, enum_value)
            if found:
                if declarations.is_fundamental(arg.decl_type) and ' ' in \
                        arg.decl_type.decl_string:
                    template = '(%s)(%s)'
                else:
                    template = '%s(%s)'
                if self.__cxx_std.is_cxx11_or_greater:
                    qualifier_decl_string = found.decl_string
                else:
                    qualifier_decl_string = found.parent.decl_string
                return template % (arg.decl_type.decl_string,
                                   self.__join_names(qualifier_decl_string,
                                                     enum_value))
            else:
                parent = parent.parent

        # check if we have an unqualified integral constant
        # only do patching in cases where we have a bare variable name
        c_var = re.compile("[a-z_][a-z0-9_]*", re.IGNORECASE)
        m = c_var.match(arg.default_value)
        if m:
            parent = func.parent
            while parent:
                try:
                    found = parent.variable(
                        arg.default_value, recursive=False)
                except declarations.declaration_not_found_t:
                    # ignore exceptions if a match is not found
                    found = None
                if found and declarations.is_fundamental(arg.decl_type):
                    return "%s" % self.__join_names(
                        found.parent.decl_string, arg.default_value)
                parent = parent.parent

        return arg.default_value

    def __find_enum(self, scope, default_value):
        # this algorithm could be improved: it could take into account
        # 1. unnamed namespace
        # 2. location within files

        for enum in self.__enums:
            if enum.parent is scope and enum.has_value_name(default_value):
                return enum
        return None

    @staticmethod
    def __is_double_call(func, arg):
        call_invocation = declarations.call_invocation
        dv = arg.default_value
        found1 = call_invocation.find_args(dv)
        if found1 == call_invocation.NOT_FOUND:
            return False
        found2 = call_invocation.find_args(dv, found1[1] + 1)
        if found2 == call_invocation.NOT_FOUND:
            return False
        args1 = call_invocation.args(dv[found1[0]: found1[1] + 1])
        args2 = call_invocation.args(dv[found2[0]: found2[1] + 1])
        return len(args1) == len(args2)

    @staticmethod
    def __fix_double_call(func, arg):
        call_invocation = declarations.call_invocation
        dv = arg.default_value
        found1 = call_invocation.find_args(dv)
        found2 = call_invocation.find_args(dv, found1[1] + 1)
        # args1 = call_invocation.args(dv[ found1[0] : found1[1] + 1 ])
        args2 = call_invocation.args(dv[found2[0]: found2[1] + 1])
        return call_invocation.join(dv[:found1[0]], args2)

    @staticmethod
    def __is_constructor_call(func, arg):
        call_invocation = declarations.call_invocation
        dv = arg.default_value
        if not call_invocation.is_call_invocation(dv):
            return False
        name = call_invocation.name(dv)
        base_type = type_traits.base_type(arg.decl_type)
        if not isinstance(base_type, declarations.declarated_t):
            return False
        decl = base_type.declaration
        return (
            decl.name == name or
            (isinstance(decl, declarations.class_t) and
             default_argument_patcher_t.__is_decl_in_aliases(decl, name)))

    @staticmethod
    def __is_decl_in_aliases(declaration, name):
        return name in [typedef.name for typedef in declaration.aliases]

    def __fix_constructor_call(self, func, arg):
        call_invocation = declarations.call_invocation
        dv = arg.default_value
        if not call_invocation.is_call_invocation(dv):
            return False
        base_type = type_traits.base_type(arg.decl_type)
        decl = base_type.declaration
        name, args = call_invocation.split(dv)
        if decl.name != name:
            # we have some alias to the class
            relevant_typedefs = [
                typedef for typedef in decl.aliases if typedef.name == name]
            if len(relevant_typedefs) == 1:
                f_q_name = self.__join_names(
                    declarations.full_name(
                        relevant_typedefs[0].parent),
                    name)
            else:  # in this case we can not say which typedef user uses:
                f_q_name = self.__join_names(
                    declarations.full_name(
                        decl.parent),
                    decl.name)
        else:
            f_q_name = self.__join_names(
                declarations.full_name(
                    decl.parent),
                name)

        return call_invocation.join(f_q_name, args)


class casting_operator_patcher_t(object):

    def __init__(self):
        object.__init__(self)

    def __call__(self, decl):
        decl.name = 'operator ' + decl.return_type.decl_string


_casting_oper_patcher_ = casting_operator_patcher_t()


def fix_calldef_decls(decls, enums, cxx_std):
    """
    some times gccxml report typedefs defined in no namespace
    it happens for example in next situation
    template< typename X>
    void ddd(){ typedef typename X::Y YY;}
    if I will fail on this bug next time, the right way to fix it may be
    different
    """
    default_arg_patcher = default_argument_patcher_t(enums, cxx_std)
    # decls should be flat list of all declarations, you want to apply patch on
    for decl in decls:
        default_arg_patcher(decl)
        if isinstance(decl, declarations.casting_operator_t):
            _casting_oper_patcher_(decl)


def update_unnamed_class(decls):
    """
    Adds name to class_t declarations.

    If CastXML is being used, the type definitions with an unnamed
    class/struct are split across two nodes in the XML tree. For example,

        typedef struct {} cls;

    produces

        <Struct id="_7" name="" context="_1" .../>
        <Typedef id="_8" name="cls" type="_7" context="_1" .../>

    For each typedef, we look at which class it refers to, and update the name
    accordingly. This helps the matcher classes finding these declarations.
    This was the behaviour with gccxml too, so this is important for
    backward compatibility.

    If the castxml epic version 1 is used, there is even an elaborated type
    declaration between the typedef and the struct/class, that also needs to be
    taken care of.

    Args:
        decls (list[declaration_t]): a list of declarations to be patched.
    Returns:
        None
    """

    for decl in decls:
        if isinstance(decl, declarations.typedef_t):
            referent = decl.decl_type
            if isinstance(referent, declarations.elaborated_t):
                referent = referent.base
            if not isinstance(referent, declarations.declarated_t):
                continue
            referent = referent.declaration
            if referent.name or not isinstance(referent, declarations.class_t):
                continue
            referent.name = decl.name
