# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt


class declaration_not_found_t(RuntimeError):
    """Exception raised when the declaration could not be found"""

    def __init__(self, decl_matcher):
        RuntimeError.__init__(self)
        self._decl_matcher = decl_matcher

    def __str__(self):
        return (
            "Unable to find declaration. Matcher: [%s]" % str(
                self._decl_matcher)
        )


class multiple_declarations_found_t(RuntimeError):
    """Exception raised when more than one declaration was found"""

    def __init__(self, decl_matcher):
        RuntimeError.__init__(self)
        self._decl_matcher = decl_matcher

    def __str__(self):
        return (
            "Multiple declarations have been found. Matcher: [%s]" % str(
                self._decl_matcher)
        )


class visit_function_has_not_been_found_t(RuntimeError):
    """
    Exception that is raised, from :func:`apply_visitor`, when a visitor could
    not be applied.

    """

    def __init__(self, visitor, decl_inst):
        RuntimeError.__init__(self)
        self.__msg = (
            "Unable to find visit function. Visitor class: %s. " +
            "Declaration instance class: %s'") \
            % (visitor.__class__.__name__, decl_inst.__class__.__name__)

    def __str__(self):
        return self.__msg
