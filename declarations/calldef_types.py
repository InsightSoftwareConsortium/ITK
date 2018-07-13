# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

import re


class VIRTUALITY_TYPES(object):

    """class that defines "virtuality" constants"""
    NOT_VIRTUAL = 'not virtual'
    VIRTUAL = 'virtual'
    PURE_VIRTUAL = 'pure virtual'
    ALL = [NOT_VIRTUAL, VIRTUAL, PURE_VIRTUAL]


# preserving backward compatebility
FUNCTION_VIRTUALITY_TYPES = VIRTUALITY_TYPES


class CALLING_CONVENTION_TYPES(object):

    """class that defines "calling convention" constants"""
    UNKNOWN = ''
    CDECL = 'cdecl'
    STDCALL = 'stdcall'
    THISCALL = 'thiscall'
    FASTCALL = 'fastcall'
    SYSTEM_DEFAULT = '<<<system default>>>'

    all = (UNKNOWN, CDECL, STDCALL, THISCALL, FASTCALL, SYSTEM_DEFAULT)

    pattern = re.compile(
        r'.*(?:^|\s)(?:__)?(?P<cc>cdecl|stdcall|thiscall|fastcall)(?:__)?.*')

    @staticmethod
    def extract(text, default=UNKNOWN):
        """extracts calling convention from the text. If the calling convention
        could not be found, the "default"is used"""
        if not text:
            return default
        found = CALLING_CONVENTION_TYPES.pattern.match(text)
        if found:
            return found.group('cc')

        return default
