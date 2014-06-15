#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Import unicode literals so that StringIO works on both Python 2 and 3
from __future__ import unicode_literals
from __future__ import print_function

import sys, os

try:
    # Python 3
    from io import StringIO
except ImportError:
    # Python 2
    from cStringIO import StringIO

pygccxmlPath = sys.argv[1]
gccxmlPath = sys.argv[2]
xmlFilePath = sys.argv[3]
idxFilePath = sys.argv[4]

sys.path.append(pygccxmlPath)
import warnings
# Temporarly hide pygccxml warnings
# Import normally once pygccxml 1.6.0 is out
warnings.filterwarnings("ignore", category=UserWarning)
warnings.filterwarnings("ignore", category=DeprecationWarning)
import pygccxml

# the output file
outputFile = StringIO()
# init the pygccxml stuff
pygccxml.declarations.scopedef_t.RECURSIVE_DEFAULT = False
pygccxml.declarations.scopedef_t.ALLOW_EMPTY_MDECL_WRAPPER = True
pygccxml_config = pygccxml.parser.config.gccxml_configuration_t(gccxml_path=gccxmlPath)
pygccxml_reader = pygccxml.parser.source_reader.source_reader_t(pygccxml_config)
# and read a xml file
res = pygccxml_reader.read_xml_file(xmlFilePath)

global_ns = pygccxml.declarations.get_global_namespace( res )
cable_ns = global_ns.namespace('_cable_')
wrappers_ns = cable_ns.namespace('wrappers')

module = os.path.splitext(os.path.basename(xmlFilePath))[0]

# iterate over all the typedefs in the _cable_::wrappers namespace
for typedef in wrappers_ns.typedefs():
  n = typedef.name
  s = typedef.type.decl_string
  # drop the :: prefix - it make swig produce invalid code
  if s.startswith("::"):
    s = s[2:]
  outputFile.write("{%s} {%s} {%s}\n" % (s, n, module))

content = outputFile.getvalue()

if idxFilePath != '-':
  f = file( idxFilePath, "w" )
  f.write( content )
  f.close()
else:
  sys.stdout.write( content )
