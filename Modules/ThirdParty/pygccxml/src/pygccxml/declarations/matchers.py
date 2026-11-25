# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
defines all "built-in" classes that implement declarations compare
functionality according to some criteria
"""

import re
from . import class_declaration


class matcher_base_t(object):

    """matcher_base_t class defines interface for classes that will implement
       compare functionality according to some criteria.
    """

    def __init__(self):
        object.__init__(self)

    def __call__(self, decl):
        raise NotImplementedError(
            "matcher must always implement the __call__() method.")

    def __invert__(self):
        """not-operator (~)"""
        return not_matcher_t(self)

    def __and__(self, other):
        """and-operator (&)"""
        return and_matcher_t([self, other])

    def __or__(self, other):
        """or-operator (|)"""
        return or_matcher_t([self, other])

    def __str__(self):
        return "base class for all matchers"


class and_matcher_t(matcher_base_t):

    """
    Combine several other matchers with "&" (and) operator.

    For example: find all private functions with name XXX

    .. code-block:: python

       matcher = access_type_matcher_t( 'private' ) & \
           calldef_matcher_t( name='XXX' )
    """

    def __init__(self, matchers):
        matcher_base_t.__init__(self)
        self.matchers = matchers

    def __call__(self, decl):
        for matcher in self.matchers:
            if not matcher(decl):
                return False
        return True

    def __str__(self):
        return " & ".join(["(%s)" % str(x) for x in self.matchers])


class or_matcher_t(matcher_base_t):

    """Combine several other matchers with "|" (or) operator.

    For example: find all functions and variables with name 'XXX'

    .. code-block:: python

       matcher = variable_matcher_t( name='XXX' ) | \
           calldef_matcher_t( name='XXX' )

    """

    def __init__(self, matchers):
        matcher_base_t.__init__(self)
        self.matchers = matchers

    def __call__(self, decl):
        for matcher in self.matchers:
            if matcher(decl):
                return True
        return False

    def __str__(self):
        return " | ".join(["(%s)" % str(x) for x in self.matchers])


class not_matcher_t(matcher_base_t):

    """
    return the inverse result of a matcher

    For example: find all public and protected declarations

    .. code-block:: python

       matcher = ~access_type_matcher_t( 'private' )
    """

    def __init__(self, matcher):
        matcher_base_t.__init__(self)
        self.matcher = matcher

    def __call__(self, decl):
        return not self.matcher(decl)

    def __str__(self):
        return "~(%s)" % str(self.matcher)


class regex_matcher_t(matcher_base_t):

    """
    Instance of this class will match declaration using regular expression.
    User should supply a function that will extract from declaration desired
    information as string. Later, this matcher will match that string using
    user regular expression.
    """

    def __init__(self, regex, function=None):
        """
        :param regex: regular expression
        :type regex: string, an instance of this class will compile it for you

        :param function: function that will be called to get an information
                         from declaration as string. As input this function
                         takes single argument - reference to a declaration.
                         Return value should be string. If function is None,
                         then the matcher will use declaration name.

        """
        matcher_base_t.__init__(self)
        self.regex = re.compile(regex)
        self.function = function
        if self.function is None:
            self.function = lambda decl: decl.name

    def __call__(self, decl):
        text = self.function(decl)
        return bool(self.regex.match(text))

    def __str__(self):
        return '(regex=%s)' % self.regex


class custom_matcher_t(matcher_base_t):

    """
    Instance of this class will match declaration by user custom criteria.
    """

    def __init__(self, function):
        """
        :param function: callable, that takes single argument -
                         declaration instance should return True or False
        """
        matcher_base_t.__init__(self)
        self.function = function

    def __call__(self, decl):
        return bool(self.function(decl))

    def __str__(self):
        return '(user criteria)'


class access_type_matcher_t(matcher_base_t):

    """
    Instance of this class will match declaration by its access type: public,
    private or protected. If declarations does not have access type, for
    example free function, then `False` will be returned.
    """

    def __init__(self, access_type):
        """
        :param access_type: declaration access type, could be "public",
        "private", "protected"
        :type access_type: :class: `str`
        """
        matcher_base_t.__init__(self)
        self.access_type = access_type

    def __call__(self, decl):
        if not isinstance(decl.parent, class_declaration.class_t):
            return False
        return (
            self.access_type == decl.parent.find_out_member_access_type(decl)
        )

    def __str__(self):
        return '(access type=%s)' % self.access_type


class virtuality_type_matcher_t(matcher_base_t):

    """
    Instance of this class will match declaration by its virtual type: not
    virtual, virtual or pure virtual. If declarations does not have "virtual"
    property, for example free function, then `False` will be returned.
    """

    def __init__(self, virtuality_type):
        """
        :param access_type: declaration access type
        :type access_type: :class:VIRTUALITY_TYPES defines few constants for
        your convenience.
        """
        matcher_base_t.__init__(self)
        self.virtuality_type = virtuality_type

    def __call__(self, decl):
        if not isinstance(decl.parent, class_declaration.class_t):
            return False
        return self.virtuality_type == decl.virtuality

    def __str__(self):
        return '(virtuality type=%s)' % self.virtuality_type
