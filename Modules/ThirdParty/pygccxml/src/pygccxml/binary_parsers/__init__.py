# Copyright 2014-2016 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
contains classes that allows to extract different information from binary files
( .map, .dll, .so ) and integrate it with existing declarations tree

The main function of this package is
:func:`pygccxml.binary_parsers.parsers.merge_information`.
"""

import warnings
from .undname import undname_creator_t
from .parsers import merge_information


def undecorate_blob(blob):
    """Returns undecorated/unmangled string, created from
    blob(exported symbol name)
    """
    warnings.warn(
        "undecorate_blob is deprecated.\n" +
        "Please have a look at the changelog for an explanation (since 1.8.0)",
        DeprecationWarning)

    return undname_creator_t().undecorate_blob(blob)


def format_decl(decl, hint=None):
    """
    returns string, that represents formatted declaration, according to some
    rules
    :param hint: valid values are: "msvc" and "nm"
    """
    warnings.warn(
        "format_decl is deprecated.\n" +
        "Please have a look at the changelog for an explanation (since 1.8.0)",
        DeprecationWarning)

    return undname_creator_t().format_decl(decl, hint=hint)
