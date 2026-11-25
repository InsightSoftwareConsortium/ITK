# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt


class location_t(object):
    """
    Provides information about the location of the declaration within the
    source file.

    """

    def __init__(self, file_name='', line=-1):
        self._file_name = file_name
        self._line = line

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.line == other.line \
            and self.file_name == other.file_name

    def __hash__(self):
        return hash(self.__class__) ^ hash(self.line) ^ hash(self.file_name)

    def __ne__(self, other):
        return not self.__eq__(other)

    def __lt__(self, other):
        if not isinstance(other, location_t):
            return self.__class__.__name__ < other.__class__.__name__
        return (self.file_name, self.line) < (other.file_name, other.line)

    @property
    def file_name(self):
        """
        Absolute source file name, type string.

        """

        return self._file_name

    @file_name.setter
    def file_name(self, new_file_name):
        self._file_name = new_file_name

    @property
    def line(self):
        """
        Line number, type int.

        """

        return self._line

    @line.setter
    def line(self, new_line):
        self._line = new_line

    def as_tuple(self):
        """
        Return tuple(self.file_name, self.line)

        """

        return self.file_name, self.line
