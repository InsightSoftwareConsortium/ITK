# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

import os

from . import class_declaration
from . import type_traits
from . import enumeration
from . import calldef_members
from . import calldef_types
from . import scopedef
from . import cpptypes
from .. import utils


def is_union(declaration):
    """
    Returns True if declaration represents a C++ union

    Args:
        declaration (declaration_t): the declaration to be checked.

    Returns:
        bool: True if declaration represents a C++ union
    """
    if not is_class(declaration):
        return False
    decl = class_traits.get_declaration(declaration)
    return decl.class_type == class_declaration.CLASS_TYPES.UNION


def is_struct(declaration):
    """
    Returns True if declaration represents a C++ struct

    Args:
        declaration (declaration_t): the declaration to be checked.

    Returns:
        bool: True if declaration represents a C++ struct
    """
    if not is_class(declaration):
        return False
    decl = class_traits.get_declaration(declaration)
    return decl.class_type == class_declaration.CLASS_TYPES.STRUCT


class declaration_xxx_traits(object):
    """this class implements the functionality needed for convenient work with
    declaration classes

    Implemented functionality:
        - find out whether a declaration is a desired one
        - get reference to the declaration
    """

    def __init__(self, declaration_class):
        self.declaration_class = declaration_class

    @staticmethod
    def __apply_sequence(type_):
        return \
            type_traits.remove_declarated(
                type_traits.remove_elaborated(
                    type_traits.remove_cv(
                        type_traits.remove_alias(
                            type_traits.remove_pointer(type_)))))

    def is_my_case(self, type_):
        """returns True, if type represents the desired declaration,
        False otherwise"""
        return (
            isinstance(self.__apply_sequence(type_), self.declaration_class)
        )

    def get_declaration(self, type_):
        """returns reference to the declaration

        Precondition: self.is_my_case( type ) == True
        """
        return self.__apply_sequence(type_)


enum_traits = declaration_xxx_traits(enumeration.enumeration_t)
"""implements functionality, needed for convenient work with C++ enums"""

is_enum = enum_traits.is_my_case
"""returns True, if type represents C++ enumeration declaration,
False otherwise"""

enum_declaration = enum_traits.get_declaration
"""returns reference to enum declaration"""

class_traits = declaration_xxx_traits(class_declaration.class_t)
"""implements functionality, needed for convenient work with C++ classes"""

is_class = class_traits.is_my_case
"""returns True, if type represents C++ class definition, False otherwise"""

class_declaration_traits = declaration_xxx_traits(
    class_declaration.class_declaration_t)
"""implements functionality, needed for convenient work with C++ class
declarations"""

is_class_declaration = class_declaration_traits.is_my_case
"""returns True, if type represents C++ class declaration, False otherwise"""


def find_trivial_constructor(type_):
    """
    Returns reference to trivial constructor.

    Args:
        type_ (declarations.class_t): the class to be searched.

    Returns:
        declarations.constructor_t: the trivial constructor

    """
    assert isinstance(type_, class_declaration.class_t)

    trivial = type_.constructors(
        lambda x: is_trivial_constructor(x),
        recursive=False,
        allow_empty=True)
    if trivial:
        return trivial[0]

    return None


def find_copy_constructor(type_):
    """
    Returns reference to copy constructor.

    Args:
        type_ (declarations.class_t): the class to be searched.

    Returns:
        declarations.constructor_t: the copy constructor

    """
    copy_ = type_.constructors(
        lambda x: is_copy_constructor(x),
        recursive=False,
        allow_empty=True)
    if copy_:
        return copy_[0]

    return None


def find_noncopyable_vars(class_type, already_visited_cls_vars=None):
    """
    Returns list of all `noncopyable` variables.

    If an already_visited_cls_vars list is provided as argument, the returned
    list will not contain these variables. This list will be extended with
    whatever variables pointing to classes have been found.

    Args:
        class_type (declarations.class_t): the class to be searched.
        already_visited_cls_vars (list): optional list of vars that should not
            be checked a second time, to prevent infinite recursions.

    Returns:
        list: list of all `noncopyable` variables.

    """

    assert isinstance(class_type, class_declaration.class_t)

    logger = utils.loggers.cxx_parser
    mvars = class_type.variables(
        lambda v: not v.type_qualifiers.has_static,
        recursive=False,
        allow_empty=True)
    noncopyable_vars = []

    if already_visited_cls_vars is None:
        already_visited_cls_vars = []

    message = (
        "__contains_noncopyable_mem_var - %s - TRUE - " +
        "contains const member variable")

    for mvar in mvars:

        var_type = type_traits.remove_reference(mvar.decl_type)

        if type_traits.is_const(var_type):
            no_const = type_traits.remove_const(var_type)
            if type_traits.is_fundamental(no_const) or is_enum(no_const):
                logger.debug(
                    (message + "- fundamental or enum"),
                    var_type.decl_string)
                noncopyable_vars.append(mvar)
            if is_class(no_const):
                logger.debug((message + " - class"), var_type.decl_string)
                noncopyable_vars.append(mvar)
            if type_traits.is_array(no_const):
                logger.debug((message + " - array"), var_type.decl_string)
                noncopyable_vars.append(mvar)

        if type_traits.is_pointer(var_type):
            continue

        if class_traits.is_my_case(var_type):
            cls = class_traits.get_declaration(var_type)

            # Exclude classes that have already been visited.
            if cls in already_visited_cls_vars:
                continue
            already_visited_cls_vars.append(cls)

            if is_noncopyable(cls, already_visited_cls_vars):
                logger.debug(
                    (message + " - class that is not copyable"),
                    var_type.decl_string)
                noncopyable_vars.append(mvar)

    logger.debug((
        "__contains_noncopyable_mem_var - %s - FALSE - doesn't " +
        "contain noncopyable members"), class_type.decl_string)

    return noncopyable_vars


def has_trivial_constructor(class_):
    """if class has public trivial constructor, this function will return
    reference to it, None otherwise"""
    class_ = class_traits.get_declaration(class_)
    trivial = find_trivial_constructor(class_)
    if trivial and trivial.access_type == 'public':
        return trivial


def has_copy_constructor(class_):
    """if class has public copy constructor, this function will return
    reference to it, None otherwise"""
    class_ = class_traits.get_declaration(class_)
    copy_constructor = find_copy_constructor(class_)
    if copy_constructor and copy_constructor.access_type == 'public':
        return copy_constructor


def has_destructor(class_):
    """if class has destructor, this function will return reference to it,
    None otherwise"""
    class_ = class_traits.get_declaration(class_)
    destructor = class_.decls(
        decl_type=calldef_members.destructor_t,
        recursive=False,
        allow_empty=True)
    if destructor:
        return destructor[0]


def has_public_constructor(class_):
    """if class has any public constructor, this function will return list of
    them, otherwise None"""
    class_ = class_traits.get_declaration(class_)
    decls = class_.constructors(
        lambda c: not is_copy_constructor(c) and c.access_type == 'public',
        recursive=False,
        allow_empty=True)
    if decls:
        return decls


def has_public_assign(class_):
    """returns True, if class has public assign operator, False otherwise"""
    class_ = class_traits.get_declaration(class_)
    decls = class_.member_operators(
        lambda o: o.symbol == '=' and o.access_type == 'public',
        recursive=False,
        allow_empty=True)
    return bool(decls)


def has_public_destructor(decl_type):
    """returns True, if class has public destructor, False otherwise"""
    d = has_destructor(decl_type)
    return d and d.access_type == 'public'


def has_vtable(decl_type):
    """True, if class has virtual table, False otherwise"""
    assert isinstance(decl_type, class_declaration.class_t)
    return bool(
        decl_type.calldefs(
            lambda f: isinstance(f, calldef_members.member_function_t) and
            f.virtuality != calldef_types.VIRTUALITY_TYPES.NOT_VIRTUAL,
            recursive=False,
            allow_empty=True))


def is_base_and_derived(based, derived):
    """returns True, if there is "base and derived" relationship between
    classes, False otherwise"""
    assert isinstance(based, class_declaration.class_t)
    assert isinstance(derived, (class_declaration.class_t, tuple))

    if isinstance(derived, class_declaration.class_t):
        all_derived = ([derived])
    else:  # tuple
        all_derived = derived

    for derived_cls in all_derived:
        for base_desc in derived_cls.recursive_bases:
            if base_desc.related_class == based:
                return True
    return False


def has_any_non_copyconstructor(decl_type):
    """if class has any public constructor, which is not copy constructor,
    this function will return list of them, otherwise None"""
    class_ = class_traits.get_declaration(decl_type)
    decls = class_.constructors(
        lambda c: not is_copy_constructor(c) and c.access_type == 'public',
        recursive=False,
        allow_empty=True)
    if decls:
        return decls


class __is_convertible_t(object):

    """implementation details"""

    def __init__(self, source, target):
        self.__source = self.__normalize(source)
        self.__target = self.__normalize(target)

    @staticmethod
    def __find_class_by_class_declaration(class_decl):
        found = scopedef.find_declaration(
            class_decl.parent.declarations,
            name=class_decl.name,
            decl_type=class_declaration.class_t)
        return found

    def __normalize(self, type_):
        type_ = type_traits.remove_alias(type_)
        bt_of_type = type_traits.base_type(type_)
        if isinstance(bt_of_type, cpptypes.declarated_t) \
           and isinstance(bt_of_type.declaration,
                          class_declaration.class_declaration_t):
            type_ = type_.clone()
            bt_of_type = type_traits.base_type(type_)
            bt_of_type.declaration = self.__find_class_by_class_declaration(
                bt_of_type.declaration)
        return type_

    @staticmethod
    def __test_trivial(src, target):
        if not (src and target):
            return False
        if type_traits.is_same(src, target):
            return True  # X => X
        if type_traits.is_const(target) and type_traits.is_same(
                src, target.base):
            return True  # X => const X
        if type_traits.is_reference(target) and type_traits.is_same(
                src, target.base):
            return True  # X => X&
        if type_traits.is_reference(target) and type_traits.is_const(
                target.base) and type_traits.is_same(src, target.base.base):
            return True  # X => const X&
        if type_traits.is_same(target, cpptypes.pointer_t(cpptypes.void_t())):
            if type_traits.is_integral(src) or is_enum(src):
                return False
            return True  # X => void*
        if type_traits.is_pointer(src) and \
            type_traits.is_pointer(target) and \
            type_traits.is_const(target.base) and \
                type_traits.is_same(src.base, target.base.base):
            return True  # X* => const X*
        if type_traits.is_reference(src) and \
            type_traits.is_reference(target) and \
            type_traits.is_const(target.base) and \
                type_traits.is_same(src.base, target.base.base):
            return True  # X& => const X&
        if not type_traits.is_const(src) and \
            type_traits.is_array(src) and \
            type_traits.is_pointer(target) and \
                type_traits.is_same(type_traits.base_type(src), target.base):
            return True  # X[2] => X*
        if type_traits.is_array(src) and \
            type_traits.is_pointer(target) and \
            type_traits.is_const(target.base) and \
            type_traits.is_same(
                    type_traits.base_type(src), target.base.base):
            return True

    @staticmethod
    def __test_pointer_to_func_or_mv__to__func_or_mv(source, target):
        if type_traits.is_pointer(source) \
           and type_traits.is_reference(target) \
           and isinstance(target.base,
                          (cpptypes.free_function_type_t,
                           cpptypes.member_function_type_t,
                           cpptypes.member_variable_type_t)) \
           and type_traits.is_same(source.base, target.base):
            return True

        if type_traits.is_pointer(source) \
           and isinstance(target,
                          (cpptypes.free_function_type_t,
                           cpptypes.member_function_type_t,
                           cpptypes.member_variable_type_t)) \
           and type_traits.is_same(source.base, target):
            return True

        if type_traits.is_pointer(target) \
           and type_traits.is_reference(source) \
           and isinstance(source.base,
                          (cpptypes.free_function_type_t,
                           cpptypes.member_function_type_t,
                           cpptypes.member_variable_type_t)) \
           and type_traits.is_same(source.base, target.base):
            return True

        if type_traits.is_pointer(target) \
           and isinstance(source,
                          (cpptypes.free_function_type_t,
                           cpptypes.member_function_type_t,
                           cpptypes.member_variable_type_t)) \
           and type_traits.is_same(target.base, source):
            return True

    @staticmethod
    def __test_const_x_ref__to__x(source, target):
        if not type_traits.is_reference(source) \
           or not type_traits.is_const(source.base) \
           or not type_traits.is_same(source.base.base, target):
            return False
        if type_traits.is_fundamental(target):
            return True
        if is_enum(target):
            return True
        if isinstance(target, cpptypes.declarated_t):
            assert isinstance(target.declaration, class_declaration.class_t)
            if has_copy_constructor(target.declaration):
                return True  # we have copy constructor
        return False

    @staticmethod
    def __test_const_ref_x__to__y(source, target):
        if not type_traits.is_reference(source) or not \
                type_traits.is_const(source.base):
            return False
        if type_traits.is_fundamental(source.base.base) and \
                type_traits.is_fundamental(target):
            return True
        if is_convertible(source.base.base, cpptypes.int_t()) and \
                is_enum(target):
            return True
        if isinstance(target, cpptypes.declarated_t):
            assert isinstance(target.declaration, class_declaration.class_t)
            if has_copy_constructor(target.declaration):
                return True  # we have copy constructor
        return False

    @staticmethod
    def __test_ref_x__to__x(source, target):
        if not type_traits.is_reference(source) or not \
                type_traits.is_same(source.base, target):
            return False
        if type_traits.is_fundamental(target):
            return True
        if is_enum(target):
            return True
        if isinstance(target, cpptypes.declarated_t):
            assert isinstance(target.declaration, class_declaration.class_t)
            if has_copy_constructor(target.declaration):
                return True  # we have copy constructor
        return False

    @staticmethod
    def __test_ref_x__to__y(source, target):
        if not type_traits.is_reference(source):
            return False
        if type_traits.is_fundamental(source.base) and \
                type_traits.is_fundamental(target):
            return True
        if is_convertible(source.base, cpptypes.int_t()) and is_enum(target):
            return True
        if isinstance(target, cpptypes.declarated_t):
            assert isinstance(target.declaration, class_declaration.class_t)
            if has_copy_constructor(target.declaration):
                return True  # we have copy constructor
        return False

    @staticmethod
    def __test_fundamental__to__fundamental(source, target):
        if not type_traits.is_fundamental(
                type_traits.base_type(source)) or not \
                type_traits.is_fundamental(
                    type_traits.base_type(target)):
            return False
        if type_traits.is_void(type_traits.base_type(source)) or \
                type_traits.is_void(type_traits.base_type(target)):
            return False
        if type_traits.is_fundamental(source) and \
                type_traits.is_fundamental(target):
            return True
        if not type_traits.is_pointer(source) and \
                type_traits.is_fundamental(target):
            return True
        if not type_traits.is_pointer(source) and \
                type_traits.is_const(target) and \
                type_traits.is_fundamental(target.base):
            return True
        if type_traits.is_fundamental(source) \
           and type_traits.is_reference(target) \
           and type_traits.is_const(target.base) \
           and type_traits.is_fundamental(target.base.base):
            return True  # X => const Y&
        return False

    @staticmethod
    def _is_both_declarated(x, y):
        return (
            isinstance(x, cpptypes.declarated_t) and
            isinstance(y, cpptypes.declarated_t))

    def __test_derived_to_based(self, source, target):
        derived = type_traits.base_type(source)
        base = type_traits.base_type(target)
        if not (
                isinstance(derived, cpptypes.declarated_t) and
                isinstance(derived.declaration, class_declaration.class_t)):
            return False
        if not (isinstance(base, cpptypes.declarated_t) and
                isinstance(base.declaration, class_declaration.class_t)):
            return False
        base = base.declaration
        derived = derived.declaration
        if not is_base_and_derived(base, derived):
            return False
        for b in derived.recursive_bases:
            if ((b.related_class is base) and b.access_type !=
                    class_declaration.ACCESS_TYPES.PRIVATE):
                break
        else:
            return False

        base = target
        derived = source
        # d => b
        if self._is_both_declarated(base, derived):
            return True
        # d* => b*
        if type_traits.is_pointer(derived) and \
                type_traits.is_pointer(base) and \
                self._is_both_declarated(base.base, derived.base):
            return True
        # const d* => const b*
        if type_traits.is_pointer(derived) and \
                type_traits.is_pointer(base) and \
                type_traits.is_const(derived.base) and \
                type_traits.is_const(base.base) \
           and self._is_both_declarated(base.base.base, derived.base.base):
            return True
        # d* => const b*
        if type_traits.is_pointer(derived) and type_traits.is_pointer(base) \
           and type_traits.is_const(derived.base)\
           and self._is_both_declarated(base.base.base, derived.base):
            return True

        # d& => b&
        if type_traits.is_reference(derived) and \
                type_traits.is_reference(base) and \
                self._is_both_declarated(base.base, derived.base):
            return True
        # const d& => const b&
        if type_traits.is_reference(derived) and \
                type_traits.is_reference(base) and \
                type_traits.is_const(derived.base) and \
                type_traits.is_const(base.base) \
           and self._is_both_declarated(base.base.base, derived.base.base):
            return True
        # d& => const b&
        if type_traits.is_reference(derived) and \
                type_traits.is_reference(base) and \
                type_traits.is_const(derived.base) \
           and self._is_both_declarated(base.base.base, derived.base):
            return True
        return False

    def is_convertible(self):
        source = self.__source
        target = self.__target

        if self.__test_trivial(source, target):
            return True
        if type_traits.is_array(source) or type_traits.is_array(target):
            return False
        if self.__test_const_x_ref__to__x(source, target):
            return True
        if self.__test_const_ref_x__to__y(source, target):
            return True
        if self.__test_ref_x__to__x(source, target):
            return True
        if self.__test_ref_x__to__y(source, target):
            return True
        if self.__test_fundamental__to__fundamental(source, target):
            return True
        if self.__test_pointer_to_func_or_mv__to__func_or_mv(source, target):
            return True
        if self.__test_derived_to_based(source, target):
            return True

        if isinstance(source, cpptypes.declarated_t):
            if isinstance(source.declaration, enumeration.enumeration_t) \
               and type_traits.is_fundamental(target) \
               and not type_traits.is_void(target):
                return True  # enum could be converted to any integral type

            if isinstance(source.declaration, class_declaration.class_t):
                source_inst = source.declaration
                # class instance could be convertible to something else if it
                # has operator
                casting_operators = scopedef.find_all_declarations(
                    source_inst.declarations,
                    decl_type=calldef_members.casting_operator_t,
                    recursive=False)
                if casting_operators:
                    for operator in casting_operators:
                        if is_convertible(operator.return_type, target):
                            return True

        # may be target is class too, so in this case we should check whether
        # is has constructor from source
        if isinstance(target, cpptypes.declarated_t) and \
                isinstance(target.declaration, class_declaration.class_t):
            constructors = scopedef.find_all_declarations(
                target.declaration.declarations,
                decl_type=calldef_members.constructor_t,
                recursive=False)
            if constructors:
                for constructor in constructors:
                    if len(constructor.arguments) != 1:
                        continue
                    # TODO: add test to check explicitness
                    if is_convertible(source,
                                      constructor.arguments[0].decl_type):
                        return True

        return False


def is_convertible(source, target):
    """returns True, if source could be converted to target, otherwise False"""
    return __is_convertible_t(source, target).is_convertible()


def __is_noncopyable_single(class_, already_visited_cls_vars=None):
    """
    Implementation detail.

    Checks if the class is non copyable, without considering the base classes.

    Args:
        class_ (declarations.class_t): the class to be checked
        already_visited_cls_vars (list): optional list of vars that should not
            be checked a second time, to prevent infinite recursions.

    Returns:
        bool: if the class is non copyable
    """
    # It is not enough to check base classes, we should also to check
    # member variables.
    logger = utils.loggers.cxx_parser

    if has_copy_constructor(class_) \
       and has_public_constructor(class_) \
       and has_public_assign(class_) \
       and has_public_destructor(class_):
        msg = os.linesep.join([
            "__is_noncopyable_single - %s - COPYABLE:" % class_.decl_string,
            "    trivial copy constructor: yes",
            "    public constructor: yes",
            "    public assign: yes",
            "    public destructor: yes"])
        logger.debug(msg)
        return False

    if already_visited_cls_vars is None:
        already_visited_cls_vars = []

    if find_noncopyable_vars(class_, already_visited_cls_vars):
        logger.debug(
            ("__is_noncopyable_single(TRUE) - %s - contains noncopyable " +
             "members"), class_.decl_string)
        return True

    logger.debug((
        "__is_noncopyable_single(FALSE) - %s - COPYABLE, because is " +
        "doesn't contains noncopyable members"), class_.decl_string)
    return False


def is_noncopyable(class_, already_visited_cls_vars=None):
    """
    Checks if class is non copyable

    Args:
        class_ (declarations.class_t): the class to be checked
        already_visited_cls_vars (list): optional list of vars that should not
            be checked a second time, to prevent infinite recursions.
            In general you can ignore this argument, it is mainly used during
            recursive calls of is_noncopyable() done by pygccxml.

    Returns:
        bool: if the class is non copyable
    """
    logger = utils.loggers.cxx_parser

    class_decl = class_traits.get_declaration(class_)

    true_header = "is_noncopyable(TRUE) - %s - " % class_.decl_string

    if is_union(class_):
        return False

    if class_decl.is_abstract:
        logger.debug(true_header + "abstract client")
        return True

    # if class has public, user defined copy constructor, than this class is
    # copyable
    copy_ = find_copy_constructor(class_decl)
    if copy_ and copy_.access_type == 'public' and not copy_.is_artificial:
        return False

    if already_visited_cls_vars is None:
        already_visited_cls_vars = []

    for base_desc in class_decl.recursive_bases:
        assert isinstance(base_desc, class_declaration.hierarchy_info_t)

        if base_desc.related_class.decl_string in \
                ('::boost::noncopyable', '::boost::noncopyable_::noncopyable'):
            logger.debug(true_header + "derives from boost::noncopyable")
            return True

        if not has_copy_constructor(base_desc.related_class):

            base_copy_ = find_copy_constructor(base_desc.related_class)

            if base_copy_ and base_copy_.access_type == 'private':
                logger.debug(
                    true_header +
                    "there is private copy constructor")
                return True
            elif __is_noncopyable_single(
                    base_desc.related_class, already_visited_cls_vars):
                logger.debug(
                    true_header +
                    "__is_noncopyable_single returned True")
                return True

        if __is_noncopyable_single(
                base_desc.related_class, already_visited_cls_vars):
            logger.debug(
                true_header +
                "__is_noncopyable_single returned True")
            return True

    if not has_copy_constructor(class_decl):
        logger.debug(true_header + "does not have trivial copy constructor")
        return True
    elif not has_public_constructor(class_decl):
        logger.debug(true_header + "does not have a public constructor")
        return True
    elif has_destructor(class_decl) and not has_public_destructor(class_decl):
        logger.debug(true_header + "has private destructor")
        return True

    return __is_noncopyable_single(class_decl, already_visited_cls_vars)


def is_unary_operator(oper):
    """returns True, if operator is unary operator, otherwise False"""
    # definition:
    # member in class
    # ret-type operator symbol()
    # ret-type operator [++ --](int)
    # globally
    # ret-type operator symbol( arg )
    # ret-type operator [++ --](X&, int)
    symbols = ['!', '&', '~', '*', '+', '++', '-', '--']
    if not isinstance(oper, calldef_members.operator_t):
        return False
    if oper.symbol not in symbols:
        return False
    if isinstance(oper, calldef_members.member_operator_t):
        if len(oper.arguments) == 0:
            return True
        elif oper.symbol in ['++', '--'] and \
                isinstance(oper.arguments[0].decl_type, cpptypes.int_t):
            return True
        return False

    if len(oper.arguments) == 1:
        return True
    elif oper.symbol in ['++', '--'] \
            and len(oper.arguments) == 2 \
            and isinstance(oper.arguments[1].decl_type, cpptypes.int_t):
        # may be I need to add additional check whether first argument is
        # reference or not?
        return True
    return False


def is_binary_operator(oper):
    """returns True, if operator is binary operator, otherwise False"""
    # definition:
    # member in class
    # ret-type operator symbol(arg)
    # globally
    # ret-type operator symbol( arg1, arg2 )
    symbols = [
        ',', '()', '[]', '!=', '%', '%=', '&', '&&', '&=', '*', '*=', '+',
        '+=', '-', '-=', '->', '->*', '/', '/=', '<', '<<', '<<=', '<=', '=',
        '==', '>', '>=', '>>', '>>=', '^', '^=', '|', '|=', '||']
    if not isinstance(oper, calldef_members.operator_t):
        return False
    if oper.symbol not in symbols:
        return False

    if isinstance(oper, calldef_members.member_operator_t):
        if len(oper.arguments) == 1:
            return True
        return False

    if len(oper.arguments) == 2:
        return True
    return False


def is_copy_constructor(constructor):
    """
    Check if the declaration is a copy constructor,

    Args:
        constructor (declarations.constructor_t): the constructor
            to be checked.

    Returns:
        bool: True if this is a copy constructor, False instead.

    """
    assert isinstance(constructor, calldef_members.constructor_t)
    args = constructor.arguments
    parent = constructor.parent

    # A copy constructor has only one argument
    if len(args) != 1:
        return False

    # We have only one argument, get it
    arg = args[0]

    if not isinstance(arg.decl_type, cpptypes.compound_t):
        # An argument of type declarated_t (a typedef) could be passed to
        # the constructor; and it could be a reference.
        # But in c++ you can NOT write :
        #    "typedef class MyClass { MyClass(const MyClass & arg) {} }"
        # If the argument is a typedef, this is not a copy constructor.
        # See the hierarchy of declarated_t and coumpound_t. They both
        # inherit from type_t but are not related so we can discriminate
        # between them.
        return False

    # The argument needs to be passed by reference in a copy constructor
    if not type_traits.is_reference(arg.decl_type):
        return False

    # The argument needs to be const for a copy constructor
    if not type_traits.is_const(arg.decl_type.base):
        return False

    un_aliased = type_traits.remove_alias(arg.decl_type.base)
    # un_aliased now refers to const_t instance
    if not isinstance(un_aliased.base, cpptypes.declarated_t):
        # We are looking for a declaration
        # If "class MyClass { MyClass(const int & arg) {} }" is used,
        # this is not copy constructor, so we return False here.
        # -> un_aliased.base == cpptypes.int_t (!= cpptypes.declarated_t)
        return False

    # Final check: compare the parent (the class declaration for example)
    # with the declaration of the type passed as argument.
    return id(un_aliased.base.declaration) == id(parent)


def is_trivial_constructor(constructor):
    """
    Check if the declaration is a trivial constructor.

    Args:
        constructor (declarations.constructor_t): the constructor
            to be checked.

    Returns:
        bool: True if this is a trivial constructor, False instead.

    """
    assert isinstance(constructor, calldef_members.constructor_t)
    return not bool(constructor.arguments)
