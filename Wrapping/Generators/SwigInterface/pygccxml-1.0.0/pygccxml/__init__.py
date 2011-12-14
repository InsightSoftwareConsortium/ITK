# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0. (See
# accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

"""Python GCC-XML front end.

This package provides functionality to extract and inspect
declarations from C/C++ header files. This is accomplished
by invoking the external tool U{gccxml<http://www.gccxml.org/>}
which parses a header file and dumps the declarations as a
XML file. This XML file is then read by pygccxml and the contents
are made available as appropriate Python objects.

To parse a set of C/C++ header files you use the
L{parse()<parser.parse>} function in the L{parser} sub package which
returns a tree that contains all declarations found in the header
files. The root of the tree represents the main namespace C{::} and
the children nodes represent the namespace contents such as other
namespaces, classes, functions, etc. Each node in the tree is an
object of a type derived from the
L{declaration_t<declarations.declaration_t>} base class. An inner
node is always either a namespace (L{namespace_t<declarations.namespace_t>})
or a class (L{class_t<declarations.class_t>}) which are both derived
from L{scopedef_t<declarations.scopedef_t>}. Everything else (free functions,
member functions, enumerations, variables, etc.) is always a leaf.
You will find all those declaration classes in the L{declarations}
sub-package.

"""

import pygccxml.declarations as declarations
import pygccxml.parser as parser
import pygccxml.utils as utils

#TODO:
#  1. Write documentation for filtering functionality.
#  2. Add "explicit" property for constructors

__version__ = '1.0.0'

__revision__ = 1080
