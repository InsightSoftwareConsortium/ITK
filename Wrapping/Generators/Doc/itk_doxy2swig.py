#!/usr/bin/env python3
"""Doxygen XML to SWIG docstring converter for ITK classes.

Usage:

  itk_doxy2swig.py [input directory name] [output swing interface file name]

"""

import sys
import os
import glob
from doxy2swig import *
from io import StringIO
from xml.dom import minidom

CITE_REFID_PREFIX = "citelist_1CITEREF_"
_bibliographies = {}


def _plain_text(node):
    if node.nodeType == node.TEXT_NODE:
        return node.data
    if node.nodeType != node.ELEMENT_NODE:
        return ""
    if node.tagName == "nonbreakablespace":
        return " "
    if node.tagName in ("ndash", "mdash"):
        return "-"
    text = "".join(_plain_text(n) for n in node.childNodes)
    if node.tagName == "ulink":
        text += f" ({node.getAttribute('url')})"
    return text


def _bibliography(xml_dir):
    """Map citation refids to the entries of Doxygen's citelist.xml."""
    if xml_dir not in _bibliographies:
        entries = {}
        citelist = os.path.join(xml_dir, "citelist.xml")
        if os.path.isfile(citelist):
            doc = minidom.parse(citelist)
            for variablelist in doc.getElementsByTagName("variablelist"):
                refid = None
                for child in variablelist.childNodes:
                    if child.nodeType != child.ELEMENT_NODE:
                        continue
                    if child.tagName == "varlistentry":
                        anchors = child.getElementsByTagName("anchor")
                        refid = anchors[0].getAttribute("id") if anchors else None
                    elif child.tagName == "listitem" and refid:
                        entries[refid] = " ".join(_plain_text(child).split())
                        refid = None
        _bibliographies[xml_dir] = entries
    return _bibliographies[xml_dir]


class itkDoxy2SWIG(Doxy2SWIG):
    def __init__(self, src, cpp_name="", swig_name=""):
        Doxy2SWIG.__init__(self, src)
        self.cpp_name = cpp_name
        self.swig_name = swig_name
        self.citations = []

    def do_ref(self, node):
        refid = node.getAttribute("refid")
        if not refid.startswith(CITE_REFID_PREFIX):
            Doxy2SWIG.do_ref(self, node)
            return
        # Doxygen's [N] labels only index its own bibliography page, and
        # without bibtex Doxygen writes the key instead.
        self.add_text(" " + refid[len(CITE_REFID_PREFIX) :])
        if refid not in self.citations:
            self.citations.append(refid)

    def end_docstring(self):
        bibliography = _bibliography(self.my_dir)
        references = [r for r in self.citations if r in bibliography]
        if references:
            self.add_text(["\n", "\n", "References:", "\n"])
            for refid in references:
                entry = f"{refid[len(CITE_REFID_PREFIX) :]}: {bibliography[refid]}"
                entry = entry.replace("\\", r"\\\\").replace('"', r"\"")
                self.add_text(["\n", entry, "\n"])
        self.citations = []
        Doxy2SWIG.end_docstring(self)

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
        self.add_text("\n\n")
        data = self.cpp_to_swig_name(node.firstChild.data)
        # print("=================", data)
        self.add_text('%%feature("docstring") %s "\n' % data)

    def do_memberdef(self, node):
        prot = node.attributes["prot"].value
        id = node.attributes["id"].value
        kind = node.attributes["kind"].value
        tmp = node.parentNode.parentNode.parentNode
        compdef = tmp.getElementsByTagName("compounddef")[0]
        cdef_kind = compdef.attributes["kind"].value

        if prot == "public":
            first = self.get_specific_nodes(node, ("definition", "name"))
            name = first["name"].firstChild.data
            if name[:8] == "operator":  # Don't handle operators yet.
                return
            if first["definition"].firstChild is None:
                return

            # store self.pieces to be able to restore it if the docstring is empty.
            pieces_backup = list(self.pieces)

            defn = first["definition"].firstChild.data
            self.add_text("\n")
            self.add_text('%feature("docstring") ')

            anc = node.parentNode.parentNode
            if cdef_kind in ("file", "namespace"):
                ns_node = anc.getElementsByTagName("innernamespace")
                if not ns_node and cdef_kind == "namespace":
                    ns_node = anc.getElementsByTagName("compoundname")
                if ns_node:
                    ns = ns_node[0].firstChild.data
                    self.add_text(f' {ns}::{name} "\n{defn}')
                else:
                    self.add_text(f' {name} "\n{defn}')
            elif cdef_kind in ("class", "struct"):
                # Get the full function name.
                anc_node = anc.getElementsByTagName("compoundname")
                cname = self.cpp_to_swig_name(anc_node[0].firstChild.data)
                # self.add_text(' %s::%s "\n%s'%(cname, name, defn))
                self.add_text(f' {cname}::{name} "')
                # print("***", name, defn)

            # make sure that the docstring won't be empty before writing any text
            current_length = len(self.pieces)

            for n in node.childNodes:
                # if n not in first.values():
                if n not in first.values() and (
                    n.__class__.__name__ != "Element" or "description" in n.tagName
                ):
                    self.parse(n)

            # make sure that the docstring won't be empty before writing any text
            if current_length == len(self.pieces):
                # restore the old self.pieces
                self.pieces = pieces_backup
            else:
                self.end_docstring()


def d2s_dir(in_dir_name, out_swig_i):
    with open(in_dir_name, encoding="utf-8") as conffile:
        output = StringIO()
        for l in conffile:
            l = l.strip()
            if l != "":
                ls = l.split("\t")
                xfn = ls[0]
                if os.path.isfile(xfn):  # make sure the assumed file exists
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
                    print(f"Warning: {xfn} does not exist. Ignore it.", file=sys.stderr)
    text = output.getvalue()
    # An unchanged file keeps its timestamp, so the build skips SWIG for it.
    if os.path.isfile(out_swig_i):
        with open(out_swig_i, encoding="utf-8") as f:
            if f.read() == text:
                return
    with open(out_swig_i, "w", encoding="utf-8") as f:
        f.write(text)


def main(in_dir_name, out_swig_i):
    d2s_dir(in_dir_name, out_swig_i)


if __name__ == "__main__":
    if len(sys.argv) != 3:
        print(__doc__)
        sys.exit(1)
    main(sys.argv[1], sys.argv[2])
