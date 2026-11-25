# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
template instantiation parser

This module provides functionality necessary to

* :func:`parse <pygccxml.declarations.templates.parse>`
* :func:`split <pygccxml.declarations.templates.split>`
* :func:`join <pygccxml.declarations.templates.join>`
* :func:`normalize <pygccxml.declarations.templates.normalize>`

C++ template instantiations
"""

from . import pattern_parser
from . import declaration_utils

__THE_PARSER = pattern_parser.parser_t('<', '>', ',')


def is_instantiation(decl_string):
    """
    returns True if `decl_string` is template instantiation and False otherwise

    :param decl_string: string that should be checked for pattern presence
    :type decl_string: str

    :rtype: bool
    """
    return __THE_PARSER.has_pattern(decl_string)


def name(decl_string):
    """
    returns name of instantiated template

    :type decl_string: str
    :rtype: str
    """
    return __THE_PARSER.name(decl_string)


def args(decl_string):
    """
    returns list of template arguments

    :type decl_string: `str`
    :rtype: [`str`]
    """
    return __THE_PARSER.args(decl_string)


def split(decl_string):
    """returns (name, [arguments] )"""
    return __THE_PARSER.split(decl_string)


def split_recursive(decl_string):
    """returns [(name, [arguments])]"""
    return __THE_PARSER.split_recursive(decl_string)


def join(name_, args_):
    """returns name< argument_1, argument_2, ..., argument_n >"""
    return __THE_PARSER.join(name_, args_)


def normalize(decl_string):
    """returns `decl_string`, which contains "normalized" spaces

    this functionality allows to implement comparison of 2 different string
    which are actually same: x::y< z > and x::y<z>
    """
    return __THE_PARSER.normalize(decl_string)


def normalize_name(decl):
    """
    Cached variant of normalize

    Args:
        decl (declaration.declaration_t): the declaration

    Returns:
        str: normalized name
    """
    if decl.cache.normalized_name is None:
        decl.cache.normalized_name = normalize(decl.name)
    return decl.cache.normalized_name


def normalize_partial_name(decl):
    """
    Cached variant of normalize

    Args:
        decl (declaration.declaration_t): the declaration

    Returns:
        str: normalized name
    """
    if decl.cache.normalized_partial_name is None:
        decl.cache.normalized_partial_name = normalize(decl.partial_name)
    return decl.cache.normalized_partial_name


def normalize_full_name_true(decl):
    """
    Cached variant of normalize

    Args:
        decl (declaration.declaration_t): the declaration

    Returns:
        str: normalized name
    """
    if decl.cache.normalized_full_name_true is None:
        decl.cache.normalized_full_name_true = normalize(
            declaration_utils.full_name(decl, with_defaults=True))
    return decl.cache.normalized_full_name_true


def normalize_full_name_false(decl):
    """
    Cached variant of normalize

    Args:
        decl (declaration.declaration_t): the declaration

    Returns:
        str: normalized name
    """
    if decl.cache.normalized_full_name_false is None:
        decl.cache.normalized_full_name_false = normalize(
            declaration_utils.full_name(decl, with_defaults=False))
    return decl.cache.normalized_full_name_false
