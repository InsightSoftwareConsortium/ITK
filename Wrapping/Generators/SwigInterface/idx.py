#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Import unicode literals so that StringIO works on both Python 2 and 3
from __future__ import unicode_literals
from __future__ import print_function

import sys
import os

try:
    # Python 3
    from io import StringIO
except ImportError:
    # Python 2
    from cStringIO import StringIO


def getType(v):
    if hasattr(v, "type"):
        return getType(v.type)
    if hasattr(v, "declaration"):
        return getType(v.declaration)
    return v


class IdxGenerator(object):
    """Generates a the .idx file for an ITK wrapping submodule (which usually
    corresponds to a class)."""

    # the output file
    outputFile = StringIO()

    def __init__(self, moduleName):
        self.moduleName = moduleName

    def create_idxfile(self, idxFilePath, wrappers_ns):
        # iterate over all the typedefs in the _wrapping_::wrappers namespace
        for typedef in wrappers_ns.typedefs():
            n = typedef.name
            s = getType(typedef).decl_string
            # drop the :: prefix - it make swig produce invalid code
            if s.startswith("::"):
                s = s[2:]
            self.outputFile.write("{%s} {%s} {%s}\n" % (s, n, self.moduleName))

        content = self.outputFile.getvalue()

        with open(idxFilePath, "w") as f:
            f.write(content)

if __name__ == '__main__':
    pygccxmlPath = sys.argv[1]
    castxmlPath = sys.argv[2]
    xmlFilePath = sys.argv[3]
    idxFilePath = sys.argv[4]

    sys.path.append(pygccxmlPath)
    import pygccxml
    import logging
    pygccxml.utils.loggers.cxx_parser.setLevel(logging.CRITICAL)

    # init the pygccxml stuff
    pygccxml.declarations.scopedef_t.RECURSIVE_DEFAULT = False
    pygccxml.declarations.scopedef_t.ALLOW_EMPTY_MDECL_WRAPPER = True
    pygccxml_config = pygccxml.parser.config.gccxml_configuration_t(
        gccxml_path=castxmlPath)
    pygccxml_reader = pygccxml.parser.source_reader.source_reader_t(
        pygccxml_config)
    # and read a xml file
    res = pygccxml_reader.read_xml_file(xmlFilePath)

    global_ns = pygccxml.declarations.get_global_namespace(res)
    cable_ns = global_ns.namespace('_wrapping_')
    wrappers_ns = cable_ns.namespace('wrappers')

    module = os.path.splitext(os.path.basename(xmlFilePath))[0]

    idx_generator = IdxGenerator(module)
    idx_generator.create_idxfile(idxFilePath, wrappers_ns)
