#!/usr/bin/env python
# -*- coding: utf-8 -*-

from __future__ import print_function

import sys, os
sys.path.append(sys.path[0]+os.sep+'pygccxml-1.0.0')

import pygccxml, sys, cStringIO

# the output file
outputFile = cStringIO.StringIO()
# init the pygccxml stuff
pygccxml.declarations.scopedef_t.RECURSIVE_DEFAULT = False
pygccxml.declarations.scopedef_t.ALLOW_EMPTY_MDECL_WRAPPER = True
pygccxml_config = pygccxml.parser.config.config_t()
pygccxml_reader = pygccxml.parser.source_reader.source_reader_t(pygccxml_config)
# and read a xml file
res = pygccxml_reader.read_xml_file(sys.argv[1])

global_ns = pygccxml.declarations.get_global_namespace( res )
cable_ns = global_ns.namespace('_cable_')
wrappers_ns = cable_ns.namespace('wrappers')

module = os.path.splitext(os.path.basename(sys.argv[1]))[0]

# iterate over all the typedefs in the _cable_::wrappers namespace
for typedef in wrappers_ns.typedefs():
  n = typedef.name
  s = typedef.type.decl_string
  # drop the :: prefix - it make swig produce invalid code
  if s.startswith("::"):
    s = s[2:]
  print(outputFile, " {%s} {%s} {%s}" % (s, n, module))

content = outputFile.getvalue()

if sys.argv[2] != '-':
  f = file( sys.argv[2], "w" )
  f.write( content )
  f.close()
else:
  sys.stdout.write( content )
