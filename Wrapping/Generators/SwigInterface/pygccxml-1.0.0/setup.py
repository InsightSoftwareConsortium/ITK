#!/usr/bin/env python
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0. (See
# accompanying file LICENSE_1_0.txt or copy at
# http://www.boost.org/LICENSE_1_0.txt)

from __future__ import print_function

import sys, os, os.path
from distutils import sysconfig
from distutils.core import setup
from distutils.cmd import Command

def generate_doc():
    """Generate the epydoc reference manual.
    """
    print("Generating epydoc files...")

    from epydoc.docbuilder import build_doc_index
    from epydoc.docwriter.html import HTMLWriter

    docindex = build_doc_index(['pygccxml'])
    html_writer = HTMLWriter( docindex
                              , prj_name='pygccxml'
                              , prj_url='http://www.language-binding.net'
                              , show_private=False
                              , show_frames=False)

    html_writer.write( os.path.join('docs', 'apidocs') )

class doc_cmd(Command):
    """This is a new distutils command 'doc' to build the epydoc manual.
    """

    description = 'build the API reference using epydoc'
    user_options = [('no-doc', None, "don't run epydoc")]
    boolean_options = ['no-doc']

    def initialize_options (self):
        self.no_doc = 0

    def finalize_options (self):
        pass

    def run(self):
        if self.no_doc:
            return
        generate_doc()


# Generate the doc when a source distribution is created
if sys.argv[-1]=="sdist":
    generate_doc()


setup( name = "pygccxml",
       version = "1.0.0",
       description = "GCC-XML generated file reader",
       author = "Roman Yakovenko",
       author_email = "roman.yakovenko@gmail.com",
       url = 'http://www.language-binding.net/pygccxml/pygccxml.html',
       packages = [ 'pygccxml',
                    'pygccxml.declarations',
                    'pygccxml.parser',
                    'pygccxml.msvc',
                    'pygccxml.msvc.bsc',
                    'pygccxml.msvc.pdb',
                    'pygccxml.utils' ],
       cmdclass = {"doc" : doc_cmd}
)
