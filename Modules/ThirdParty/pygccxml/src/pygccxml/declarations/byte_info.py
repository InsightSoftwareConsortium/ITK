# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt


class byte_info(object):

    """
    This class stores information about the byte size and byte align
    values from a declaration/type.

    """

    def __init__(self):
        self._byte_size = 0
        self._byte_align = 0

    @property
    def byte_size(self):
        """
        Size of this declaration/type in bytes

        Returns:
            int: Size of this declaration/type in bytes
        """
        return self._byte_size

    @byte_size.setter
    def byte_size(self, new_byte_size):
        """
        Set size of this declaration/type in bytes

        Args:
            new_byte_size (int): Size of this declaration/type in bytes
        """
        self._byte_size = new_byte_size

    @property
    def byte_align(self):
        """
        Alignment of this declaration/type in bytes

        Returns:
            int: Alignment of this declaration/type in bytes
        """
        return self._byte_align

    @byte_align.setter
    def byte_align(self, new_byte_align):
        """
        Set size of alignment of this declaration/type in bytes

        Args:
            new_byte_align (int): Alignment of this declaration/type in bytes
        """
        self._byte_align = new_byte_align
