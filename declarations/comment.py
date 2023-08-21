# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Describe a C++ comment declaration.

"""

from . import location as pygccxml_location


class comment_t(object):

    def __init__(self, name='', declarations=None):
        """
        Creates an object that describes a C++ comment declaration.

        Args:

        """
        self._location = {}
        self._begin_line = 0
        self._begin_column = 0
        self._begin_offset = 0
        self._end_line = 0
        self._end_column = 0
        self._end_offset = 0
        self._text = ""

    @property
    def location(self):
        """An instance of the location_t class
        which contains the file where the
        comment can be found.
        @type: location_t """
        return self._location

    @location.setter
    def location(self, location):
        if not isinstance(location, pygccxml_location.location_t):
            raise ValueError(
                "'location' must be a location_t (got a %s instead)" %
                type(location).__name__)
        self._location = location

    @property
    def begin_line(self):
        """An integer value which corresponds to the
        line of the file where the comment begins
        @type: int """
        return self._begin_line

    @begin_line.setter
    def begin_line(self, begin_line):
        self._begin_line = int(begin_line)

    @property
    def begin_offset(self):
        """An integer value representing the
        number of bytes from the beginning of the
        file to the start of the comment
        @type: int """
        return self._begin_offset

    @begin_offset.setter
    def begin_offset(self, begin_offset):
        self._begin_offset = int(begin_offset)

    @property
    def begin_column(self):
        """An integer value which corresponds to the
        column of the file where the comment begins
        @type: int """
        return self._begin_column

    @begin_column.setter
    def begin_column(self, begin_column):
        self._begin_column = int(begin_column)

    @property
    def end_line(self):
        """An integer value which corresponds to the
        line of the file where the comment ends
        @type: int """
        return self._end_line

    @end_line.setter
    def end_line(self, end_line):
        self._end_line = int(end_line)

    @property
    def end_offset(self):
        """An integer value representing the
        number of bytes from the beginning of the
        file to the end of the comment
        @type: int """
        return self._end_offset

    @end_offset.setter
    def end_offset(self, end_offset):
        self._end_offset = int(end_offset)

    @property
    def end_column(self):
        """An integer value which corresponds to the
        column of character in a line of the file
        where the comment ends
        @type: int """
        return self._end_column

    @end_column.setter
    def end_column(self, end_column):
        self._end_column = int(end_column)

    @property
    def text(self):
        """A list of strings where each entry in the list
        is one line of the comment. These comments will not
        end in a newline
        @type: list """
        return self._text

    @text.setter
    def text(self, text):
        if not isinstance(text, list):
            raise ValueError(
                "'text' must be a list (got a %s instead)" %
                type(text).__name__)
        self._text = text
