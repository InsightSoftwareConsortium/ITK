# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Free function invocation parser

The parser is able to extract function name and list of arguments from a
function invocation statement. For example, for the following code

.. code-block:: c++

  do_something( x1, x2, x3 )

the parser will extract
- function name - `do_something`
- argument names - `[ x1, x2, x3 ]`

"""

from . import pattern_parser

__THE_PARSER = pattern_parser.parser_t('(', ')', ',')


def is_call_invocation(declaration_string):
    """
    Returns True if `declaration_string` is a function invocation.

    :param declaration_string: string that should be checked for pattern.
    :type declaration_string: str

    :rtype: bool

    """
    return __THE_PARSER.has_pattern(declaration_string)


def name(declaration_string):
    """
    Returns the name of a function.

    :type declaration_string: str
    :rtype: str

    """
    return __THE_PARSER.name(declaration_string)


def args(declaration_string):
    """
    Returns list of function arguments

    :type declaration_string: str
    :rtype: [str]

    """
    return __THE_PARSER.args(declaration_string)


NOT_FOUND = __THE_PARSER.NOT_FOUND


def find_args(text, start=None):
    """
    Finds arguments within function invocation.

    :type text: str
    :rtype: [ arguments ] or :data:NOT_FOUND if arguments could not be found.

    """
    return __THE_PARSER.find_args(text, start)


def split(declaration_string):
    """
    Returns (name, [arguments] )

    """
    return __THE_PARSER.split(declaration_string)


def split_recursive(declaration_string):
    """
    Returns [(name, [arguments])].

    """
    return __THE_PARSER.split_recursive(declaration_string)


def join(name_, args_, arg_separator=None):
    """
    Returns name( argument_1, argument_2, ..., argument_n ).

    """
    return __THE_PARSER.join(name_, args_, arg_separator)
