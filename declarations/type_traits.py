# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
defines few algorithms, that deals with different C++ type properties

Are you aware of `boost::type_traits <http://www.boost.org/doc/libs/1_37_0/
libs/type_traits/doc/html/boost_typetraits/intro.html>`_
library? pygccxml implements the same functionality.

This module contains a set of very specific traits functions\\classes, each of
which encapsulate a single trait from the C++ type system. For example:
* is a type a pointer or a reference type ?
* does a type have a trivial constructor ?
* does a type have a const-qualifier ?

"""

from . import cpptypes
from . import typedef
from .. import utils


def __remove_alias(type_):
    """
    Implementation detail.

    Args:
        type_ (type_t): type

    Returns:
        type_t: the type associated to the inputted type
    """
    if isinstance(type_, cpptypes.declarated_t) and \
            isinstance(type_.declaration, typedef.typedef_t):
        return __remove_alias(type_.declaration.decl_type)
    if isinstance(type_, cpptypes.compound_t):
        type_.base = __remove_alias(type_.base)
        return type_
    return type_


def remove_alias(type_):
    """
    Returns `type_t` without typedef

    Args:
        type_ (type_t | declaration_t): type or declaration

    Returns:
        type_t: the type associated to the inputted declaration
    """
    if isinstance(type_, cpptypes.type_t):
        type_ref = type_
    elif isinstance(type_, typedef.typedef_t):
        type_ref = type_.decl_type
    else:
        # Not a valid input, just return it
        return type_
    if type_ref.cache.remove_alias:
        return type_ref.cache.remove_alias
    no_alias = __remove_alias(type_ref.clone())
    type_ref.cache.remove_alias = no_alias
    return no_alias


def decompose_type(tp):
    """
    Implementation detail
    """
    if isinstance(tp, cpptypes.compound_t):
        return [tp] + decompose_type(tp.base)
    elif isinstance(tp, typedef.typedef_t):
        return decompose_type(tp.decl_type)
    elif isinstance(tp, cpptypes.declarated_t) and \
            isinstance(tp.declaration, typedef.typedef_t):
        return decompose_type(tp.declaration.decl_type)
    else:
        return [tp]


def decompose_class(type_):
    """implementation details"""
    types = decompose_type(type_)
    return [tp.__class__ for tp in types]


def base_type(type_):
    """returns base type.

    For `const int` will return `int`
    """
    types = decompose_type(type_)
    return types[-1]


def _create_cv_types(base):
    """
    Implementation detail.

    """
    return (
        [base,
         cpptypes.const_t(base),
         cpptypes.volatile_t(base),
         cpptypes.volatile_t(cpptypes.const_t(base))]
    )


# Some tuples containing combinations of different types
# These are created once the module is loaded, so that when they are used
# they do not need to be re-created.
_void_def = _create_cv_types(cpptypes.void_t())
_bool_def = _create_cv_types(cpptypes.bool_t())
_float_def = (
    _create_cv_types(cpptypes.float_t()) +
    _create_cv_types(cpptypes.double_t()) +
    _create_cv_types(cpptypes.long_double_t()))
_integral_def = (
    _create_cv_types(cpptypes.char_t()) +
    _create_cv_types(cpptypes.unsigned_char_t()) +
    _create_cv_types(cpptypes.signed_char_t()) +
    _create_cv_types(cpptypes.wchar_t()) +
    _create_cv_types(cpptypes.short_int_t()) +
    _create_cv_types(cpptypes.short_unsigned_int_t()) +
    _create_cv_types(cpptypes.bool_t()) +
    _create_cv_types(cpptypes.int_t()) +
    _create_cv_types(cpptypes.unsigned_int_t()) +
    _create_cv_types(cpptypes.long_int_t()) +
    _create_cv_types(cpptypes.long_unsigned_int_t()) +
    _create_cv_types(cpptypes.long_long_int_t()) +
    _create_cv_types(cpptypes.long_long_unsigned_int_t()) +
    _create_cv_types(cpptypes.int128_t()) +
    _create_cv_types(cpptypes.uint128_t()))


def does_match_definition(given, main, secondary):
    """implementation details"""
    assert isinstance(secondary, tuple)
    assert len(secondary) == 2  # general solution could be provided
    types = decompose_type(given)

    if isinstance(types[0], main):
        return True

    if len(types) >= 2:
        cond1 = isinstance(types[0], main)
        cond2 = isinstance(types[1], secondary)
        cond3 = isinstance(types[1], main)
        cond4 = isinstance(types[0], secondary)
        if (cond1 and cond2) or (cond3 and cond4):
            return True

        if len(types) >= 3:
            classes = set([tp.__class__ for tp in types[:3]])
            desired = set([main] + list(secondary))
            diff = classes.symmetric_difference(desired)
            if not diff:
                return True
            if len(diff) == 2:
                items = list(diff)
                return (
                    issubclass(
                        items[0], items[1]) or issubclass(items[1], items[0]))
            else:
                return False
    else:
        return False


def is_bool(type_):
    """
    Check if type is of boolean type.

    Args:
        type_ (type_t): The type to be checked

    Returns:
        bool: True if type is a boolean, False otherwise.
    """
    return remove_alias(type_) in _bool_def


def is_void(type_):
    """
    Check if type is of void type.

    Args:
        type_ (type_t): The type to be checked

    Returns:
        bool: True if type is void, False otherwise.
    """
    return remove_alias(type_) in _void_def


def is_void_pointer(type_):
    """returns True, if type represents `void*`, False otherwise"""
    return is_same(type_, cpptypes.pointer_t(cpptypes.void_t()))


def is_integral(type_):
    """
    Check if type is a C++ integral type

    Args:
        type_ (type_t): The type to be checked

    Returns:
        bool: True if type is a C++ integral type, False otherwise.
    """
    return remove_alias(type_) in _integral_def


def is_floating_point(type_):
    """returns True, if type represents C++ floating point type,
    False otherwise"""

    return remove_alias(type_) in _float_def


def is_arithmetic(type_):
    """returns True, if type represents C++ integral or floating point type,
    False otherwise"""
    return is_integral(type_) or is_floating_point(type_)


def is_pointer(type_):
    """returns True, if type represents C++ pointer type, False otherwise"""
    return does_match_definition(type_,
                                 cpptypes.pointer_t,
                                 (cpptypes.const_t, cpptypes.volatile_t)) \
        or does_match_definition(type_,
                                 cpptypes.pointer_t,
                                 (cpptypes.volatile_t, cpptypes.const_t))


def is_calldef_pointer(type_):
    """returns True, if type represents pointer to free/member function,
    False otherwise"""
    if not is_pointer(type_):
        return False
    nake_type = remove_alias(type_)
    nake_type = remove_cv(nake_type)
    return isinstance(nake_type, cpptypes.compound_t) \
        and isinstance(nake_type.base, cpptypes.calldef_type_t)


def remove_pointer(type_):
    """removes pointer from the type definition

    If type is not pointer type, it will be returned as is.
    """
    nake_type = remove_alias(type_)
    if not is_pointer(nake_type):
        return type_
    elif isinstance(nake_type, cpptypes.volatile_t) and \
            isinstance(nake_type.base, cpptypes.pointer_t):
        return cpptypes.volatile_t(nake_type.base.base)
    elif isinstance(nake_type, cpptypes.const_t) and \
            isinstance(nake_type.base, cpptypes.pointer_t):
        return cpptypes.const_t(nake_type.base.base)
    elif isinstance(nake_type, cpptypes.volatile_t) \
            and isinstance(nake_type.base, cpptypes.const_t) \
            and isinstance(nake_type.base.base, cpptypes.pointer_t):
        return (
            cpptypes.volatile_t(cpptypes.const_t(nake_type.base.base.base))
        )
    else:
        return nake_type.base


def is_reference(type_):
    """returns True, if type represents C++ reference type, False otherwise"""
    nake_type = remove_alias(type_)
    return isinstance(nake_type, cpptypes.reference_t)


def is_array(type_):
    """returns True, if type represents C++ array type, False otherwise"""
    nake_type = remove_alias(type_)
    nake_type = remove_reference(nake_type)
    nake_type = remove_cv(nake_type)
    return isinstance(nake_type, cpptypes.array_t)


def array_size(type_):
    """returns array size"""
    nake_type = remove_alias(type_)
    nake_type = remove_reference(nake_type)
    nake_type = remove_cv(nake_type)
    assert isinstance(nake_type, cpptypes.array_t)
    return nake_type.size


def array_item_type(type_):
    """returns array item type"""
    if is_array(type_):
        type_ = remove_alias(type_)
        type_ = remove_cv(type_)
        return type_.base
    elif is_pointer(type_):
        return remove_pointer(type_)
    else:
        raise RuntimeError(
            "array_item_type functions takes as argument array or pointer " +
            "types")


def remove_reference(type_):
    """removes reference from the type definition

    If type is not reference type, it will be returned as is.
    """
    nake_type = remove_alias(type_)
    if not is_reference(nake_type):
        return type_
    else:
        return nake_type.base


def is_const(type_):
    """returns True, if type represents C++ const type, False otherwise"""
    nake_type = remove_alias(type_)
    if isinstance(nake_type, cpptypes.const_t):
        return True
    elif isinstance(nake_type, cpptypes.volatile_t):
        return is_const(nake_type.base)
    elif isinstance(nake_type, cpptypes.array_t):
        return is_const(nake_type.base)
    return False


def remove_const(type_):
    """removes const from the type definition

    If type is not const type, it will be returned as is
    """

    nake_type = remove_alias(type_)
    if not is_const(nake_type):
        return type_
    else:
        # Handling for const and volatile qualified types. There is a
        # difference in behavior between GCCXML and CastXML for cv-qual arrays.
        # GCCXML produces the following nesting of types:
        #       ->  volatile_t(const_t(array_t))
        # while CastXML produces the following nesting:
        #       ->  array_t(volatile_t(const_t))
        # For both cases, we must unwrap the types, remove const_t, and add
        # back the outer layers
        if isinstance(nake_type, cpptypes.array_t):
            is_v = is_volatile(nake_type)
            if is_v:
                result_type = nake_type.base.base.base
            else:
                result_type = nake_type.base.base
            if is_v:
                result_type = cpptypes.volatile_t(result_type)
            return cpptypes.array_t(result_type, nake_type.size)

        elif isinstance(nake_type, cpptypes.volatile_t):
            return cpptypes.volatile_t(nake_type.base.base)

        return nake_type.base


def remove_declarated(type_):
    """removes type-declaration class-binder :class:`declarated_t` from
    the `type_`

    If `type_` is not :class:`declarated_t`, it will be returned as is
    """
    type_ = remove_alias(type_)
    if isinstance(type_, cpptypes.elaborated_t):
        type_ = type_.base
    if isinstance(type_, cpptypes.declarated_t):
        type_ = type_.declaration
    return type_


def is_same(type1, type2):
    """returns True, if type1 and type2 are same types"""
    nake_type1 = remove_declarated(type1)
    nake_type2 = remove_declarated(type2)
    return nake_type1 == nake_type2


def is_elaborated(type_):
    """returns True, if type represents C++ elaborated type, False otherwise"""
    nake_type = remove_alias(type_)
    if isinstance(nake_type, cpptypes.elaborated_t):
        return True
    elif isinstance(nake_type, cpptypes.reference_t):
        return is_elaborated(nake_type.base)
    elif isinstance(nake_type, cpptypes.pointer_t):
        return is_elaborated(nake_type.base)
    elif isinstance(nake_type, cpptypes.volatile_t):
        return is_elaborated(nake_type.base)
    elif isinstance(nake_type, cpptypes.const_t):
        return is_elaborated(nake_type.base)
    return False


def remove_elaborated(type_):
    """removes type-declaration class-binder :class:`elaborated_t` from
    the `type_`

    If `type_` is not :class:`elaborated_t`, it will be returned as is
    """
    nake_type = remove_alias(type_)
    if not is_elaborated(nake_type):
        return type_
    else:
        if isinstance(type_, cpptypes.elaborated_t):
            type_ = type_.base
    return type_


def is_volatile(type_):
    """returns True, if type represents C++ volatile type, False otherwise"""
    nake_type = remove_alias(type_)
    if isinstance(nake_type, cpptypes.volatile_t):
        return True
    elif isinstance(nake_type, cpptypes.const_t):
        return is_volatile(nake_type.base)
    elif isinstance(nake_type, cpptypes.array_t):
        return is_volatile(nake_type.base)
    return False


def remove_volatile(type_):
    """removes volatile from the type definition

    If type is not volatile type, it will be returned as is
    """
    nake_type = remove_alias(type_)
    if not is_volatile(nake_type):
        return type_
    else:
        if isinstance(nake_type, cpptypes.array_t):
            is_c = is_const(nake_type)
            if is_c:
                base_type_ = nake_type.base.base.base
            else:
                base_type_ = nake_type.base.base
            result_type = base_type_
            if is_c:
                result_type = cpptypes.const_t(result_type)
            return cpptypes.array_t(result_type, nake_type.size)
        return nake_type.base


def remove_cv(type_):
    """removes const and volatile from the type definition"""

    nake_type = remove_alias(type_)
    if not is_const(nake_type) and not is_volatile(nake_type):
        return type_
    result = nake_type
    if is_const(result):
        result = remove_const(result)
    if is_volatile(result):
        result = remove_volatile(result)
    if is_const(result):
        result = remove_const(result)
    return result


def is_fundamental(type_):
    """returns True, if type represents C++ fundamental type"""
    return does_match_definition(
        type_,
        cpptypes.fundamental_t,
        (cpptypes.const_t, cpptypes.volatile_t)) \
        or does_match_definition(
            type_,
            cpptypes.fundamental_t,
            (cpptypes.volatile_t, cpptypes.const_t))


string_equivalences = [
    (
        '::std::basic_string<char,std::char_traits<char>,'
        'std::allocator<char> >'),
    (
        '::std::basic_string<char, std::char_traits<char>, '
        'std::allocator<char> >'),
    '::std::basic_string<char>', '::std::string']

wstring_equivalences = [
    (
        '::std::basic_string<wchar_t,std::char_traits<wchar_t>,' +
        'std::allocator<wchar_t> >'),
    (
        '::std::basic_string<wchar_t, std::char_traits<wchar_t>, ' +
        'std::allocator<wchar_t> >'),
    '::std::basic_string<wchar_t>', '::std::wstring']

ostream_equivalences = [
    '::std::basic_ostream<char, std::char_traits<char> >',
    '::std::basic_ostream<char,std::char_traits<char> >',
    '::std::basic_ostream<char>', '::std::ostream']

wostream_equivalences = [
    '::std::basic_ostream<wchar_t, std::char_traits<wchar_t> >',
    '::std::basic_ostream<wchar_t,std::char_traits<wchar_t> >',
    '::std::basic_ostream<wchar_t>', '::std::wostream']


def is_std_string(type_):
    """
    Returns True, if type represents C++ `std::string`, False otherwise.

    """

    if utils.is_str(type_):
        return type_ in string_equivalences
    else:
        type_ = remove_alias(type_)
        type_ = remove_reference(type_)
        type_ = remove_cv(type_)
        return type_.decl_string in string_equivalences


def is_std_wstring(type_):
    """
    Returns True, if type represents C++ `std::wstring`, False otherwise.

    """

    if utils.is_str(type_):
        return type_ in wstring_equivalences
    else:
        type_ = remove_alias(type_)
        type_ = remove_reference(type_)
        type_ = remove_cv(type_)
        return type_.decl_string in wstring_equivalences


def is_std_ostream(type_):
    """
    Returns True, if type represents C++ std::ostream, False otherwise.

    """

    if utils.is_str(type_):
        return type_ in ostream_equivalences
    else:
        type_ = remove_alias(type_)
        type_ = remove_reference(type_)
        type_ = remove_cv(type_)
        return type_.decl_string in ostream_equivalences


def is_std_wostream(type_):
    """
    Returns True, if type represents C++ std::wostream, False otherwise.

    """

    if utils.is_str(type_):
        return type_ in wostream_equivalences
    else:
        type_ = remove_alias(type_)
        type_ = remove_reference(type_)
        type_ = remove_cv(type_)
        return type_.decl_string in wostream_equivalences
