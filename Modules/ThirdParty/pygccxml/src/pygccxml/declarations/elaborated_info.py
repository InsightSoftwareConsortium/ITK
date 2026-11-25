# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt


class elaborated_info(object):
    """
    This class stores the name of the elaborated type specifier.

    """

    def __init__(self, elaborated_type_specifier):
        self._elaborated_type_specifier = elaborated_type_specifier

    @property
    def elaborated_type_specifier(self):
        """
        Elaborated specifier (can be: struct, union, class or enum).

        Returns:
            str: elaborated specifier
        """
        return self._elaborated_type_specifier
