#!/usr/bin/env python
"""Doxygen XML to SWIG docstring converter for ITK classes.

Usage:

  itk_doxy2swig.py [input directory name] [output swing interface file name]

"""

from __future__ import print_function

import sys
import os
import glob
from doxy2swig import *
if sys.version_info >= (3,0):
    # Python 3
    from io import StringIO
else:
    # Python 2
    from cStringIO import StringIO

class itkDoxy2SWIG(Doxy2SWIG):
    def __init__(self, src, cpp_name="", swig_name=""):
        Doxy2SWIG.__init__(self, src)
        self.cpp_name = cpp_name
        self.swig_name = swig_name

    def write_to_file(self, file):
        if self.multi:
            file.write("".join(self.pieces))
        else:
            file.write("".join(self.clean_pieces(self.pieces)))

    def cpp_to_swig_name(self, cpp_name):
        if self.cpp_name == cpp_name:
          return self.swig_name
        return cpp_name

    def do_compoundname(self, node):
        self.add_text('\n\n')
        data = self.cpp_to_swig_name(node.firstChild.data)
        # print("=================", data)
        self.add_text('%%feature("docstring") %s "\n'%data)

    def do_memberdef(self, node):
        prot = node.attributes['prot'].value
        id = node.attributes['id'].value
        kind = node.attributes['kind'].value
        tmp = node.parentNode.parentNode.parentNode
        compdef = tmp.getElementsByTagName('compounddef')[0]
        cdef_kind = compdef.attributes['kind'].value

        if prot == 'public':
            first = self.get_specific_nodes(node, ('definition', 'name'))
            name = first['name'].firstChild.data
            if name[:8] == 'operator': # Don't handle operators yet.
                return

            # store self.pieces to be able to restore it if the docstring is empty.
            pieces_backup = list(self.pieces)

            defn = first['definition'].firstChild.data
            self.add_text('\n')
            self.add_text('%feature("docstring") ')

            anc = node.parentNode.parentNode
            if cdef_kind in ('file', 'namespace'):
                ns_node = anc.getElementsByTagName('innernamespace')
                if not ns_node and cdef_kind == 'namespace':
                    ns_node = anc.getElementsByTagName('compoundname')
                if ns_node:
                    ns = ns_node[0].firstChild.data
                    self.add_text(' %s::%s "\n%s'%(ns, name, defn))
                else:
                    self.add_text(' %s "\n%s'%(name, defn))
            elif cdef_kind in ('class', 'struct'):
                # Get the full function name.
                anc_node = anc.getElementsByTagName('compoundname')
                cname = self.cpp_to_swig_name(anc_node[0].firstChild.data)
                # self.add_text(' %s::%s "\n%s'%(cname, name, defn))
                self.add_text(' %s::%s "'%(cname, name))
                # print("***", name, defn)

            # make sure that the docstring won't be empty before writing any text
            current_length = len(self.pieces)

            for n in node.childNodes:
                # if n not in first.values():
                if n not in first.values() and (n.__class__.__name__ != "Element" or "description" in n.tagName):
                    self.parse(n)

            # make sure that the docstring won't be empty before writing any text
            if current_length == len(self.pieces):
                # restore the old self.pieces
                self.pieces = pieces_backup
            else:
                self.add_text(['";', '\n'])


def d2s_dir(in_dir_name, out_swig_i):
  with open(in_dir_name) as conffile:
    output = StringIO()
    for l in conffile:
      l = l.strip()
      if l != "":
        ls = l.split("\t")
        xfn = ls[0]
        if os.path.isfile(xfn): # make sure the assumed file exists
            cpp_name = ls[1]
#            print("-- Doxygen to SWIG: " + cpp_name)
            output2 = StringIO()
            d2s = itkDoxy2SWIG(xfn, cpp_name, "@[{(]})@")
            d2s.generate()
            d2s.write_to_file(output2)
            tpl = output2.getvalue()
            output2.close()
            for swig_name in ls[2:]:
              output.write(tpl.replace("@[{(]})@", swig_name))
        else:
          print("Warning: %s does not exist. Ignore it." % xfn, file=sys.stderr)
  with open(out_swig_i, 'w') as f:
    f.write(output.getvalue())
    f.close()

def main(in_dir_name, out_swig_i):
        d2s_dir(in_dir_name, out_swig_i)

if __name__ == '__main__':
        if len(sys.argv) != 3:
                print(__doc__)
                sys.exit(1)
        main(sys.argv[1], sys.argv[2])
