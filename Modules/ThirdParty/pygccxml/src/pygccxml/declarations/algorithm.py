# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Define few unrelated algorithms that work on declarations.

"""

from . import declaration_utils
from . import runtime_errors


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
            self, decl_type=None,
            name=None, fullname=None, parent=None):

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


def apply_visitor(visitor, decl_inst):
    """
    Applies a visitor on declaration instance.

    :param visitor: instance
    :type visitor: :class:`type_visitor_t` or :class:`decl_visitor_t`

    """

    fname = 'visit_' + \
        decl_inst.__class__.__name__[:-2]  # removing '_t' from class name
    if not hasattr(visitor, fname):
        raise runtime_errors.visit_function_has_not_been_found_t(
            visitor, decl_inst)
    return getattr(visitor, fname)()
