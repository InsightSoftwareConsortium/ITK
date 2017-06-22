# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
defines classes, that describe C++ types
"""

from . import algorithms_cache
from . import byte_info


class type_t(byte_info.byte_info):

    """base class for all types"""

    def __init__(self):
        byte_info.byte_info.__init__(self)
        self.cache = algorithms_cache.type_algs_cache_t()

    def __str__(self):
        res = self.decl_string
        if res[:2] == "::":
            res = res[2:]
        return res

    def __eq__(self, other):
        if not isinstance(other, type_t):
            return False
        return self.decl_string == other.decl_string

    def __hash__(self):
        return hash(self.decl_string)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __lt__(self, other):
        if not isinstance(other, self.__class__):
            return self.__class__.__name__ < other.__class__.__name__
        return self.decl_string < other.decl_string

    def build_decl_string(self, with_defaults=True):
        raise NotImplementedError()

    @property
    def decl_string(self):
        if self.cache.decl_string is None:
            self.cache.decl_string = self.build_decl_string()
        return self.cache.decl_string

    @property
    def partial_decl_string(self):
        if self.cache.partial_decl_string is None:
            self.cache.partial_decl_string = self.build_decl_string(False)
        return self.cache.partial_decl_string

    def _clone_impl(self):
        raise NotImplementedError()

    def clone(self):
        """returns new instance of the type"""
        answer = self._clone_impl()
        return answer


# There are cases when GCC-XML reports something like this
# <Unimplemented id="_9482" tree_code="188" \
# tree_code_name="template_type_parm" node="0xcc4d5b0"/>
# In this case I will use this as type


class dummy_type_t(type_t):

    """provides :class:`type_t` interface for a string, that defines C++ type.

    This class could be very useful in the code generator.
    """

    def __init__(self, decl_string):
        type_t.__init__(self)
        self._decl_string = decl_string

    def build_decl_string(self, with_defaults=True):
        return self._decl_string

    def _clone_impl(self):
        return dummy_type_t(self._decl_string)


class unknown_t(type_t):
    """
    type, that represents all C++ types,
    that could not be parsed  by GCC-XML

    """

    def __init__(self):
        type_t.__init__(self)

    def build_decl_string(self, with_defaults=True):
        return '?unknown?'

    def _clone_impl(self):
        return self


class ellipsis_t(type_t):

    """type, that represents "..." in function definition"""

    def __init__(self):
        type_t.__init__(self)

    def build_decl_string(self, with_defaults=True):
        return '...'

    def _clone_impl(self):
        return self

##########################################################################
# Fundamental types:


class fundamental_t(type_t):

    """base class for all fundamental, build-in types"""

    def __init__(self, name):
        type_t.__init__(self)
        self._name = name

    def build_decl_string(self, with_defaults=True):
        return self._name

    def _clone_impl(self):
        return self


class java_fundamental_t(fundamental_t):

    """base class for all JNI defined fundamental types"""

    def __init__(self, name):
        fundamental_t.__init__(self, name)


class void_t(fundamental_t):

    """represents void type"""
    CPPNAME = 'void'

    def __init__(self):
        fundamental_t.__init__(self, void_t.CPPNAME)


class char_t(fundamental_t):

    """represents char type"""
    CPPNAME = 'char'

    def __init__(self):
        fundamental_t.__init__(self, char_t.CPPNAME)


class signed_char_t(fundamental_t):

    """represents signed char type"""
    CPPNAME = 'signed char'

    def __init__(self):
        fundamental_t.__init__(self, signed_char_t.CPPNAME)


class unsigned_char_t(fundamental_t):

    """represents unsigned char type"""
    CPPNAME = 'unsigned char'

    def __init__(self):
        fundamental_t.__init__(self, unsigned_char_t.CPPNAME)


class wchar_t(fundamental_t):

    """represents wchar_t type"""
    CPPNAME = 'wchar_t'

    def __init__(self):
        fundamental_t.__init__(self, wchar_t.CPPNAME)


class short_int_t(fundamental_t):

    """represents short int type"""
    CPPNAME = 'short int'

    def __init__(self):
        fundamental_t.__init__(self, short_int_t.CPPNAME)


class short_unsigned_int_t(fundamental_t):

    """represents short unsigned int type"""
    CPPNAME = 'short unsigned int'

    def __init__(self):
        fundamental_t.__init__(self, short_unsigned_int_t.CPPNAME)


class bool_t(fundamental_t):

    """represents bool type"""
    CPPNAME = 'bool'

    def __init__(self):
        fundamental_t.__init__(self, bool_t.CPPNAME)


class int_t(fundamental_t):

    """represents int type"""
    CPPNAME = 'int'

    def __init__(self):
        fundamental_t.__init__(self, int_t.CPPNAME)


class unsigned_int_t(fundamental_t):

    """represents unsigned int type"""
    CPPNAME = 'unsigned int'

    def __init__(self):
        fundamental_t.__init__(self, unsigned_int_t.CPPNAME)


class long_int_t(fundamental_t):

    """represents long int type"""
    CPPNAME = 'long int'

    def __init__(self):
        fundamental_t.__init__(self, long_int_t.CPPNAME)


class long_unsigned_int_t(fundamental_t):

    """represents long unsigned int type"""
    CPPNAME = 'long unsigned int'

    def __init__(self):
        fundamental_t.__init__(self, long_unsigned_int_t.CPPNAME)


class long_long_int_t(fundamental_t):

    """represents long long int type"""
    CPPNAME = 'long long int'

    def __init__(self):
        fundamental_t.__init__(self, long_long_int_t.CPPNAME)


class long_long_unsigned_int_t(fundamental_t):

    """represents long long unsigned int type"""
    CPPNAME = 'long long unsigned int'

    def __init__(self):
        fundamental_t.__init__(self, long_long_unsigned_int_t.CPPNAME)


class float_t(fundamental_t):

    """represents float type"""
    CPPNAME = 'float'

    def __init__(self):
        fundamental_t.__init__(self, float_t.CPPNAME)


class double_t(fundamental_t):

    """represents double type"""
    CPPNAME = 'double'

    def __init__(self):
        fundamental_t.__init__(self, double_t.CPPNAME)


class long_double_t(fundamental_t):

    """represents long double type"""
    CPPNAME = 'long double'

    def __init__(self):
        fundamental_t.__init__(self, long_double_t.CPPNAME)


class complex_double_t(fundamental_t):

    """represents complex double type"""
    CPPNAME = 'complex double'

    def __init__(self):
        fundamental_t.__init__(self, complex_double_t.CPPNAME)


class complex_long_double_t(fundamental_t):

    """represents complex long double type"""
    CPPNAME = 'complex long double'

    def __init__(self):
        fundamental_t.__init__(self, complex_long_double_t.CPPNAME)


class complex_float_t(fundamental_t):

    """represents complex float type"""
    CPPNAME = 'complex float'

    def __init__(self):
        fundamental_t.__init__(self, complex_float_t.CPPNAME)


class jbyte_t(java_fundamental_t):

    """represents jbyte type"""
    JNAME = 'jbyte'

    def __init__(self):
        java_fundamental_t.__init__(self, jbyte_t.JNAME)


class jshort_t(java_fundamental_t):

    """represents jshort type"""
    JNAME = 'jshort'

    def __init__(self):
        java_fundamental_t.__init__(self, jshort_t.JNAME)


class jint_t(java_fundamental_t):

    """represents jint type"""
    JNAME = 'jint'

    def __init__(self):
        java_fundamental_t.__init__(self, jint_t.JNAME)


class jlong_t(java_fundamental_t):

    """represents jlong type"""
    JNAME = 'jlong'

    def __init__(self):
        java_fundamental_t.__init__(self, jlong_t.JNAME)


class jfloat_t(java_fundamental_t):

    """represents jfloat type"""
    JNAME = 'jfloat'

    def __init__(self):
        java_fundamental_t.__init__(self, jfloat_t.JNAME)


class jdouble_t(java_fundamental_t):

    """represents jdouble type"""
    JNAME = 'jdouble'

    def __init__(self):
        java_fundamental_t.__init__(self, jdouble_t.JNAME)


class jchar_t(java_fundamental_t):

    """represents jchar type"""
    JNAME = 'jchar'

    def __init__(self):
        java_fundamental_t.__init__(self, jchar_t.JNAME)


class jboolean_t(java_fundamental_t):

    """represents jboolean type"""
    JNAME = 'jboolean'

    def __init__(self):
        java_fundamental_t.__init__(self, jboolean_t.JNAME)


class int128_t(fundamental_t):

    """represents __int128_t type"""
    CPPNAME = '__int128_t'

    def __init__(self):
        fundamental_t.__init__(self, int128_t.CPPNAME)


class uint128_t(fundamental_t):

    """represents __uint128_t type"""
    CPPNAME = '__uint128_t'

    def __init__(self):
        fundamental_t.__init__(self, uint128_t.CPPNAME)


FUNDAMENTAL_TYPES = {
    # adding java types
    void_t.CPPNAME: void_t(),
    char_t.CPPNAME: char_t(),
    signed_char_t.CPPNAME: signed_char_t(),
    unsigned_char_t.CPPNAME: unsigned_char_t(),
    wchar_t.CPPNAME: wchar_t(),
    short_int_t.CPPNAME: short_int_t(),
    'signed ' + short_int_t.CPPNAME: short_int_t(),
    short_unsigned_int_t.CPPNAME: short_unsigned_int_t(),
    bool_t.CPPNAME: bool_t(),
    int_t.CPPNAME: int_t(),
    'signed ' + int_t.CPPNAME: int_t(),
    unsigned_int_t.CPPNAME: unsigned_int_t(),
    long_int_t.CPPNAME: long_int_t(),
    long_unsigned_int_t.CPPNAME: long_unsigned_int_t(),
    long_long_int_t.CPPNAME: long_long_int_t(),
    long_long_unsigned_int_t.CPPNAME: long_long_unsigned_int_t(),
    int128_t.CPPNAME: int128_t(),
    uint128_t.CPPNAME: uint128_t(),
    float_t.CPPNAME: float_t(),
    double_t.CPPNAME: double_t(),
    long_double_t.CPPNAME: long_double_t(),
    complex_long_double_t.CPPNAME: complex_long_double_t(),
    complex_double_t.CPPNAME: complex_double_t(),
    complex_float_t.CPPNAME: complex_float_t(),
    jbyte_t.JNAME: jbyte_t(),
    jshort_t.JNAME: jshort_t(),
    jint_t.JNAME: jint_t(),
    jlong_t.JNAME: jlong_t(),
    jfloat_t.JNAME: jfloat_t(),
    jdouble_t.JNAME: jdouble_t(),
    jchar_t.JNAME: jchar_t(),
    jboolean_t.JNAME: jboolean_t(),
    '__java_byte': jbyte_t(),
    '__java_short': jshort_t(),
    '__java_int': jint_t(),
    '__java_long': jlong_t(),
    '__java_float': jfloat_t(),
    '__java_double': jdouble_t(),
    '__java_char': jchar_t(),
    '__java_boolean': jboolean_t()
}
"""
defines a mapping between fundamental type name and its synonym to the instance
of class that describes the type
"""


def _f(x, with_defaults):
    """
    A small helper function.

    """

    return x.build_decl_string(with_defaults)

##########################################################################
# Compound types:


class compound_t(type_t):

    """class that allows to represent compound types like `const int*`"""

    def __init__(self, base):
        type_t.__init__(self)
        self._base = base

    @property
    def base(self):
        """reference to internal/base class"""
        return self._base

    @base.setter
    def base(self, new_base):
        self._base = new_base

    def build_decl_string(self, with_defaults=True):
        raise NotImplementedError()

    def _clone_impl(self):
        raise NotImplementedError()


class volatile_t(compound_t):

    """represents `volatile whatever` type"""

    def __init__(self, base):
        compound_t.__init__(self, base)

    def build_decl_string(self, with_defaults=True):
        return self.base.build_decl_string(with_defaults) + ' volatile'

    def _clone_impl(self):
        return volatile_t(self.base.clone())


class restrict_t(compound_t):

    """represents `restrict whatever` type"""

    # The restrict keyword can be considered an extension to the strict
    # aliasing rule. It allows the programmer to declare that pointers which
    # share the same type (or were otherwise validly created) do not alias
    # eachother. By using restrict the programmer can declare that any loads
    # and stores through the qualified pointer (or through another pointer
    # copied either directly or indirectly from the restricted pointer) are
    # the only loads and stores to the same address during the lifetime of
    # the pointer. In other words, the pointer is not aliased by any pointers
    # other than its own copies.

    def __init__(self, base):
        compound_t.__init__(self, base)

    def build_decl_string(self, with_defaults=True):
        return '__restrict__ ' + self.base.build_decl_string(with_defaults)

    def _clone_impl(self):
        return restrict_t(self.base.clone())


class const_t(compound_t):

    """represents `whatever const` type"""

    def __init__(self, base):
        compound_t.__init__(self, base)

    def build_decl_string(self, with_defaults=True):
        return self.base.build_decl_string(with_defaults) + ' const'

    def _clone_impl(self):
        return const_t(self.base.clone())


class pointer_t(compound_t):

    """represents `whatever*` type"""

    def __init__(self, base):
        compound_t.__init__(self, base)

    def build_decl_string(self, with_defaults=True):
        decl_string = self.base.build_decl_string(with_defaults)
        if isinstance(self.base, calldef_type_t):
            # This is a function pointer. Do not add supplementary *
            return decl_string
        else:
            return decl_string + " *"

    def _clone_impl(self):
        return pointer_t(self.base.clone())


class reference_t(compound_t):

    """represents `whatever&` type"""

    def __init__(self, base):
        compound_t.__init__(self, base)

    def build_decl_string(self, with_defaults=True):
        return self.base.build_decl_string(with_defaults) + ' &'

    def _clone_impl(self):
        return reference_t(self.base.clone())


class elaborated_t(compound_t):

    """represents `elaborated` type"""

    def __init__(self, base):
        compound_t.__init__(self, base)

    def build_decl_string(self, with_defaults=True):
        if hasattr(self.base.declaration, "elaborated_type_specifier"):
            prefix = self.base.declaration.elaborated_type_specifier + " "
        else:
            prefix = ""
        return prefix + self.base.build_decl_string(with_defaults)

    def _clone_impl(self):
        return elaborated_t(self.base.clone())


class array_t(compound_t):

    """represents C++ array type"""
    SIZE_UNKNOWN = -1

    def __init__(self, base, size):
        compound_t.__init__(self, base)
        self._size = size

    @property
    def size(self):
        """returns array size"""
        return self._size

    @size.setter
    def size(self, size):
        """sometimes there is a need to update the size of the array"""
        self.cache.reset()
        self._size = size

    def build_decl_string(self, with_defaults=True):
        return self.__bds_for_multi_dim_arrays(None, with_defaults)

    def __bds_for_multi_dim_arrays(self, parent_dims=None, with_defaults=True):
        if parent_dims:
            parent_dims.append(self.size)
        else:
            parent_dims = [self.size]

        if isinstance(self.base, array_t):
            return self.base.__bds_for_multi_dim_arrays(
                parent_dims,
                with_defaults)
        else:
            tmp = []
            for s in parent_dims:
                tmp.append('[%d]' % s)
            return \
                self.base.build_decl_string(with_defaults) + " " + "".join(tmp)

    def _clone_impl(self):
        return array_t(self.base.clone(), self.size)


class calldef_type_t(object):

    """base class for all types that describes "callable" declaration"""

    def __init__(self, return_type=None, arguments_types=None):
        object.__init__(self)
        self._return_type = return_type
        if arguments_types is None:
            arguments_types = []
        self._arguments_types = arguments_types

    @property
    def return_type(self):
        """reference to :class:`return type <type_t>`"""
        return self._return_type

    @return_type.setter
    def return_type(self, new_return_type):
        self._return_type = new_return_type

    @property
    def arguments_types(self):
        """list of argument :class:`types <type_t>`"""
        return self._arguments_types

    @arguments_types.setter
    def arguments_types(self, new_arguments_types):
        self._arguments_types = new_arguments_types

    @property
    def has_ellipsis(self):
        return self.arguments_types and isinstance(
            self.arguments_types[-1], ellipsis_t)


class free_function_type_t(type_t, calldef_type_t):

    """describes free function type"""
    NAME_TEMPLATE = '%(return_type)s (*)( %(arguments)s )'
    TYPEDEF_NAME_TEMPLATE = (
        '%(return_type)s ( *%(typedef_name)s )( %(arguments)s )')

    def __init__(self, return_type=None, arguments_types=None):
        type_t.__init__(self)
        calldef_type_t.__init__(self, return_type, arguments_types)

    @staticmethod
    def create_decl_string(
            return_type, arguments_types, with_defaults=True):
        """
        Returns free function type

        :param return_type: function return type
        :type return_type: :class:`type_t`
        :param arguments_types: list of argument :class:`type <type_t>`
        :rtype: :class:`free_function_type_t`

        """

        return free_function_type_t.NAME_TEMPLATE % {
            'return_type': return_type.build_decl_string(with_defaults),
            'arguments': ','.join(
                [_f(x, with_defaults) for x in arguments_types])}

    def build_decl_string(self, with_defaults=True):
        return self.create_decl_string(
            self.return_type,
            self.arguments_types,
            with_defaults)

    def _clone_impl(self):
        rt_clone = None
        if self.return_type:
            rt_clone = self.return_type.clone()
        return free_function_type_t(
            rt_clone, [
                arg.clone() for arg in self.arguments_types])

    # TODO: create real typedef
    def create_typedef(self, typedef_name, unused=None, with_defaults=True):
        """returns string, that contains valid C++ code, that defines typedef
        to function type

        :param name: the desired name of typedef
        """

        return free_function_type_t.TYPEDEF_NAME_TEMPLATE % {
            'typedef_name': typedef_name,
            'return_type': self.return_type.build_decl_string(with_defaults),
            'arguments': ','.join(
                [_f(x, with_defaults) for x in self.arguments_types])}


class member_function_type_t(type_t, calldef_type_t):

    """describes member function type"""
    NAME_TEMPLATE = (
        '%(return_type)s ( %(class)s::* )( %(arguments)s )%(has_const)s')
    TYPEDEF_NAME_TEMPLATE = (
        '%(return_type)s ( %(class)s::*%(typedef_name)s' +
        ')( %(arguments)s ) %(has_const)s')

    def __init__(
            self,
            class_inst=None,
            return_type=None,
            arguments_types=None,
            has_const=False):
        type_t.__init__(self)
        calldef_type_t.__init__(self, return_type, arguments_types)
        self._has_const = has_const
        self._class_inst = class_inst

    @property
    def has_const(self):
        """describes, whether function has const modifier"""
        return self._has_const

    @has_const.setter
    def has_const(self, has_const):
        self._has_const = has_const

    @property
    def class_inst(self):
        """reference to parent :class:`class <declaration_t>`"""
        return self._class_inst

    @class_inst.setter
    def class_inst(self, class_inst):
        self._class_inst = class_inst

    # TODO: create real typedef
    def create_typedef(
            self,
            typedef_name,
            class_alias=None,
            with_defaults=True):
        """creates typedef to the function type

        :param typedef_name: desired type name
        :rtype: string
        """
        has_const_str = ''
        if self.has_const:
            has_const_str = 'const'
        if None is class_alias:
            if with_defaults:
                class_alias = self.class_inst.decl_string
            else:
                class_alias = self.class_inst.partial_decl_string

        return member_function_type_t.TYPEDEF_NAME_TEMPLATE % {
            'typedef_name': typedef_name,
            'return_type': self.return_type.build_decl_string(with_defaults),
            'class': class_alias,
            'arguments': ','.join(
                [_f(x, with_defaults) for x in self.arguments_types]),
            'has_const': has_const_str}

    @staticmethod
    def create_decl_string(
            return_type,
            class_decl_string,
            arguments_types,
            has_const,
            with_defaults=True):
        has_const_str = ''
        if has_const:
            has_const_str = 'const'
        return_type_decl_string = ''
        if return_type:
            return_type_decl_string = return_type.build_decl_string(
                with_defaults)

        return member_function_type_t.NAME_TEMPLATE % {
            'return_type': return_type_decl_string,
            'class': class_decl_string,
            'arguments': ','.join(
                [_f(x, with_defaults) for x in arguments_types]),
            'has_const': has_const_str}

    def build_decl_string(self, with_defaults=True):
        return self.create_decl_string(
            self.return_type,
            self.class_inst.decl_string,
            self.arguments_types,
            self.has_const,
            with_defaults)

    def _clone_impl(self):
        rt_clone = None
        if self.return_type:
            rt_clone = self.return_type.clone()

        return member_function_type_t(
            self.class_inst, rt_clone, [
                arg.clone() for arg in self.arguments_types], self.has_const)


class member_variable_type_t(compound_t):

    """describes member variable type"""
    NAME_TEMPLATE = '%(type)s ( %(class)s::* )'

    def __init__(self, class_inst=None, variable_type=None):
        compound_t.__init__(self, class_inst)
        self._mv_type = variable_type

    @property
    def variable_type(self):
        """describes member variable :class:`type <type_t>`"""
        return self._mv_type

    @variable_type.setter
    def variable_type(self, new_type):
        self._mv_type = new_type

    def build_decl_string(self, with_defaults=True):
        return self.NAME_TEMPLATE % {
            'type': self.variable_type.build_decl_string(with_defaults),
            'class': self.base.build_decl_string(with_defaults)}

    def _clone_impl(self):
        return member_variable_type_t(
            class_inst=self.base,
            variable_type=self.variable_type.clone())


##########################################################################
# declarated types:

class declarated_t(type_t, byte_info.byte_info):

    """class that binds between to hierarchies: :class:`type_t`
    and :class:`declaration_t`"""

    def __init__(self, declaration):
        type_t.__init__(self)
        byte_info.byte_info.__init__(self)
        self._declaration = declaration
        self.byte_size = self._declaration.byte_size
        self.byte_align = self._declaration.byte_align

    @property
    def declaration(self):
        """reference to :class:`declaration_t`"""
        return self._declaration

    @declaration.setter
    def declaration(self, new_declaration):
        self._declaration = new_declaration
        self.byte_size = self._declaration.byte_size
        self.byte_align = self._declaration.byte_align

    def build_decl_string(self, with_defaults=True):
        if with_defaults:
            return self._declaration.decl_string
        else:
            return self._declaration.partial_decl_string

    def _clone_impl(self):
        return declarated_t(self._declaration)


class type_qualifiers_t(object):

    """contains additional information about type: mutable, static, extern"""

    def __init__(self, has_static=False, has_mutable=False, has_extern=False):
        self._has_static = has_static
        self._has_extern = has_extern
        self._has_mutable = has_mutable

    def __eq__(self, other):
        if not isinstance(other, type_qualifiers_t):
            return False
        return self.has_static == other.has_static \
            and self.has_extern == other.has_extern \
            and self.has_mutable == other.has_mutable

    def __hash__(self):
        return super(type_qualifiers_t, self).__hash__()

    def __ne__(self, other):
        return not self.__eq__(other)

    def __lt__(self, other):
        if not isinstance(other, type_qualifiers_t):
            return object.__lt__(self, other)
        return self.has_static < other.has_static \
            and self.has_extern < other.has_extern \
            and self.has_mutable < other.has_mutable

    @property
    def has_static(self):
        return self._has_static

    @has_static.setter
    def has_static(self, has_static):
        self._has_static = has_static

    @property
    def has_extern(self):
        return self._has_extern

    @has_extern.setter
    def has_extern(self, has_extern):
        self._has_extern = has_extern

    @property
    def has_mutable(self):
        return self._has_mutable

    @has_mutable.setter
    def has_mutable(self, has_mutable):
        self._has_mutable = has_mutable
