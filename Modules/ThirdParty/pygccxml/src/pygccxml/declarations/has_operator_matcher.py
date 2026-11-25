from . import type_traits
from . import class_declaration
from . import matchers
from . import cpptypes


def has_public_binary_operator(type_, operator_symbol):
    """returns True, if `type_` has public binary operator, otherwise False"""
    type_ = type_traits.remove_alias(type_)
    type_ = type_traits.remove_cv(type_)
    type_ = type_traits.remove_declarated(type_)
    assert isinstance(type_, class_declaration.class_t)

    if type_traits.is_std_string(type_) or type_traits.is_std_wstring(type_):
        # In some case compare operators of std::basic_string are not
        # instantiated
        return True

    operators = type_.member_operators(
        function=matchers.custom_matcher_t(
            lambda decl: not decl.is_artificial) &
        matchers.access_type_matcher_t('public'),
        symbol=operator_symbol, allow_empty=True, recursive=False)
    if operators:
        return True

    declarated = cpptypes.declarated_t(type_)
    const = cpptypes.const_t(declarated)
    reference = cpptypes.reference_t(const)
    operators = type_.top_parent.operators(
        function=lambda decl: not decl.is_artificial,
        arg_types=[reference, None],
        symbol=operator_symbol,
        allow_empty=True,
        recursive=True)
    if operators:
        return True
    for bi in type_.recursive_bases:
        assert isinstance(bi, class_declaration.hierarchy_info_t)
        if bi.access_type != class_declaration.ACCESS_TYPES.PUBLIC:
            continue
        operators = bi.related_class.member_operators(
            function=matchers.custom_matcher_t(
                lambda decl: not decl.is_artificial) &
            matchers.access_type_matcher_t('public'),
            symbol=operator_symbol, allow_empty=True, recursive=False)
        if operators:
            return True
    return False


def has_public_equal(decl_type):
    """returns True, if class has public operator==, otherwise False"""
    return has_public_binary_operator(decl_type, '==')


def has_public_less(decl_type):
    """returns True, if class has public operator<, otherwise False"""
    return has_public_binary_operator(decl_type, '<')
