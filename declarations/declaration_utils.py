# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

import warnings


def declaration_path(decl, with_defaults=None):
    """
    Returns a list of parent declarations names.

    Args:
        decl (declaration_t): declaration for which declaration path
                              should be calculated.

    Returns:
        list[(str | basestring)]: list of names, where first item is the top
                                  parent name and last item the inputted
                                  declaration name.
    """

    if with_defaults is not None:
        # Deprecated since 1.9.0, will be removed in 2.0.0
        warnings.warn(
            "The with_defaults parameter is deprecated.\n", DeprecationWarning)

    if not decl:
        return []
    if not decl.cache.declaration_path:
        result = [decl.name]
        parent = decl.parent
        while parent:
            if parent.cache.declaration_path:
                result.reverse()
                decl.cache.declaration_path = parent.cache.declaration_path + \
                    result
                return decl.cache.declaration_path
            else:
                result.append(parent.name)
                parent = parent.parent
        result.reverse()
        decl.cache.declaration_path = result
        return result
    else:
        return decl.cache.declaration_path


def partial_declaration_path(decl):
    """
    Returns a list of parent declarations names without template arguments that
    have default value.

    Args:
        decl (declaration_t): declaration for which the partial declaration
                              path should be calculated.

    Returns:
        list[(str | basestring)]: list of names, where first item is the top
                                  parent name and last item the inputted
                                  declaration name.
    """

    # TODO:
    # If parent declaration cache already has declaration_path, reuse it for
    # calculation.
    if not decl:
        return []
    if not decl.cache.partial_declaration_path:
        result = [decl.partial_name]
        parent = decl.parent
        while parent:
            if parent.cache.partial_declaration_path:
                result.reverse()
                decl.cache.partial_declaration_path \
                    = parent.cache.partial_declaration_path + result
                return decl.cache.partial_declaration_path
            else:
                result.append(parent.partial_name)
                parent = parent.parent
        result.reverse()
        decl.cache.partial_declaration_path = result
        return result
    else:
        return decl.cache.partial_declaration_path


def full_name_from_declaration_path(dpath):
    # Here I have lack of knowledge:
    # TODO: "What is the full name of declaration declared in unnamed
    # namespace?"
    result = [_f for _f in dpath if _f]
    result = result[0] + '::'.join(result[1:])
    return result


def full_name(decl, with_defaults=True):
    """
    Returns declaration full qualified name.

    If `decl` belongs to anonymous namespace or class, the function will return
    C++ illegal qualified name.

    Args:
        decl (declaration_t): declaration for which the full qualified name
                              should be calculated.

    Returns:
        list[(str | basestring)]: full name of the declaration.

    """

    if None is decl:
        raise RuntimeError("Unable to generate full name for None object!")
    if with_defaults:
        if not decl.cache.full_name:
            path = declaration_path(decl)
            if path == [""]:
                # Declarations without names are allowed (for examples class
                # or struct instances). In this case set an empty name..
                decl.cache.full_name = ""
            else:
                decl.cache.full_name = full_name_from_declaration_path(path)
        return decl.cache.full_name
    else:
        if not decl.cache.full_partial_name:
            path = partial_declaration_path(decl)
            if path == [""]:
                # Declarations without names are allowed (for examples class
                # or struct instances). In this case set an empty name.
                decl.cache.full_partial_name = ""
            else:
                decl.cache.full_partial_name = \
                    full_name_from_declaration_path(path)
        return decl.cache.full_partial_name


def get_named_parent(decl):
    """
    Returns a reference to a named parent declaration.

    Args:
        decl (declaration_t): the child declaration

    Returns:
        declaration_t: the declaration or None if not found.

    """

    if not decl:
        return None

    parent = decl.parent
    while parent and (not parent.name or parent.name == '::'):
        parent = parent.parent
    return parent
