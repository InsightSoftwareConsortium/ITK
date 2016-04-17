# Copyright 2014-2015 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""Implements a few "find" algorithms on declarations tree"""

from . import algorithm


class matcher(object):

    """Class-namespace, contains implementation of a few "find" algorithms
    and definition of the related exception classes"""

    class declaration_not_found_t(RuntimeError):

        """Exception raised when the declaration could not be found"""

        def __init__(self, matcher):
            RuntimeError.__init__(self)
            self.matcher = matcher

        def __str__(self):
            return (
                "Unable to find declaration. Matcher: [%s]" % str(
                    self.matcher)
            )

    class multiple_declarations_found_t(RuntimeError):

        """Exception raised when more than one declaration was found"""

        def __init__(self, matcher):
            RuntimeError.__init__(self)
            self.matcher = matcher

        def __str__(self):
            return (
                "Multiple declarations have been found. Matcher: [%s]" % str(
                    self.matcher)
            )

    @staticmethod
    def find(decl_matcher, decls, recursive=True):
        """
        Returns a list of declarations that match `decl_matcher` defined
        criteria or None

        :param decl_matcher: Python callable object, that takes one argument -
            reference to a declaration
        :param decls: the search scope, :class:declaration_t object or
            :class:declaration_t objects list t
        :param recursive: boolean, if True, the method will run `decl_matcher`
            on the internal declarations too
        """

        where = []
        if isinstance(decls, list):
            where.extend(decls)
        else:
            where.append(decls)
        if recursive:
            where = algorithm.make_flatten(where)
        return list(filter(decl_matcher, where))

    @staticmethod
    def find_single(decl_matcher, decls, recursive=True):
        """
        Returns a reference to the declaration, that match `decl_matcher`
        defined criteria.

        if a unique declaration could not be found the method will return None.

        :param decl_matcher: Python callable object, that takes one argument -
            reference to a declaration
        :param decls: the search scope, :class:declaration_t object or
            :class:declaration_t objects list t
        :param recursive: boolean, if True, the method will run `decl_matcher`
            on the internal declarations too
        """
        answer = matcher.find(decl_matcher, decls, recursive)
        if len(answer) == 1:
            return answer[0]

    @staticmethod
    def get_single(decl_matcher, decls, recursive=True):
        """
        Returns a reference to declaration, that match `decl_matcher` defined
        criteria.

        If a unique declaration could not be found, an appropriate exception
        will be raised.

        :param decl_matcher: Python callable object, that takes one argument -
            reference to a declaration
        :param decls: the search scope, :class:declaration_t object or
            :class:declaration_t objects list t
        :param recursive: boolean, if True, the method will run `decl_matcher`
            on the internal declarations too
        """
        answer = matcher.find(decl_matcher, decls, recursive)
        if len(answer) == 1:
            return answer[0]
        elif not answer:
            raise matcher.declaration_not_found_t(decl_matcher)
        else:
            raise matcher.multiple_declarations_found_t(decl_matcher)
