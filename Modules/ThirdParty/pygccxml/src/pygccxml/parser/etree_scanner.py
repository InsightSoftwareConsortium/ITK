# Copyright 2014-2016 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

import warnings
from . import scanner

# keep py2exe happy
import xml.etree.ElementTree

import xml.etree.cElementTree as ElementTree


class etree_saxifier_t(object):

    def __init__(self, etree, handler):
        """
        Deprecated since 1.8.0. Will be removed in 1.9.0.

        """
        warnings.warn("etree_saxifier_t is deprecated.\n", DeprecationWarning)
        self.__root_elem = etree.getroot()
        self.__handler = handler

    def saxify(self):
        self.__handler.startDocument()
        self.__recursive_saxify(self.__root_elem)
        self.__handler.endDocument()

    def __recursive_saxify(self, element):
        self.__handler.startElement(element.tag, element.attrib)
        for e in element:
            self.__recursive_saxify(e)
        self.__handler.endElement(element.tag)


class etree_scanner_t(scanner.scanner_t):

    def __init__(self, xml_file, decl_factory, *args):
        """
        Deprecated since 1.8.0. Will be removed in 1.9.0.

        """
        warnings.warn(
            "etree_scanner_t is deprecated.\n" +
            "Please use ietree_scanner_t instead.", DeprecationWarning)
        scanner.scanner_t.__init__(self, xml_file, decl_factory, *args)

    def read(self):
        tree = ElementTree.parse(self.xml_file)
        saxifier = etree_saxifier_t(tree, self)
        saxifier.saxify()


class ietree_scanner_t(scanner.scanner_t):

    def __init__(self, xml_file, decl_factory, *args):
        scanner.scanner_t.__init__(self, xml_file, decl_factory, *args)

    def read(self):
        context = ElementTree.iterparse(
            self.xml_file,
            events=("start", "end"))
        for event, elem in context:
            if event == 'start':
                self.startElement(elem.tag, elem.attrib)
            else:
                self.endElement(elem.tag)
                elem.clear()
        self.endDocument()
