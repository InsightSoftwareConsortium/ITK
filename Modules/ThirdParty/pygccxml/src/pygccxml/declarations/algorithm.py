# Copyright 2014-2016 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Define few unrelated algorithms that work on declarations.

"""

import warnings
from . import declaration_utils


class match_declaration_t(object):
    """
    Helper class for different search algorithms.

    This class will help developer to match declaration by:
        - declaration type, for example :class:`class_t` or
            :class:`operator_t`.
        - declaration name
        - declaration full name
        - reference to parent declaration

    """

    def __init__(
            self, type=None, decl_type=None,
            name=None, fullname=None, parent=None):

        if type is not None:
            # Deprecated since 1.8.0. Will be removed in 1.9.0
            warnings.warn(
                "The type argument is deprecated. \n" +
                "Please use the decl_type argument instead.",
                DeprecationWarning)
            if decl_type is not None:
                raise (
                    "Please use only either the type or " +
                    "decl_type argument.")
            # Still allow to use the old type for the moment.
            decl_type = type

        self._decl_type = decl_type
        self.name = name
        self.fullname = fullname
        self.parent = parent

    def does_match_exist(self, inst):
        """
        Returns True if inst does match one of specified criteria.

        :param inst: declaration instance
        :type inst: :class:`declaration_t`

        :rtype: bool

        """

        answer = True
        if self._decl_type is not None:
            answer &= isinstance(inst, self._decl_type)
        if self.name is not None:
            answer &= inst.name == self.name
        if self.parent is not None:
            answer &= self.parent is inst.parent
        if self.fullname is not None:
            if inst.name:
                answer &= self.fullname == declaration_utils.full_name(inst)
            else:
                answer = False
        return answer

    def __call__(self, inst):
        """
        .. code-block:: python

           return self.does_match_exist(inst)

        """

        return self.does_match_exist(inst)


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


def apply_visitor(visitor, decl_inst):
    """
    Applies a visitor on declaration instance.

    :param visitor: instance
    :type visitor: :class:`type_visitor_t` or :class:`decl_visitor_t`

    """

    fname = 'visit_' + \
        decl_inst.__class__.__name__[:-2]  # removing '_t' from class name
    if not hasattr(visitor, fname):
        raise visit_function_has_not_been_found_t(visitor, decl_inst)
    return getattr(visitor, fname)()
