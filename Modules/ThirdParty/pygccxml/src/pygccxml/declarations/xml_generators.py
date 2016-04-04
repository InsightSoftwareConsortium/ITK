# Copyright 2014-2015 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Contains enumeration of all xml_generators supported by the project.

"""

GCC_XML_06 = "GCC-XML 0.6"
GCC_XML_07 = "GCC-XML 0.7"
GCC_XML_09 = "GCC-XML 0.9"
GCC_XML_09_BUGGY = "GCC-XML 0.9 BUGGY"
# revision 122:
# After this fix, all constructors and destructors that exist for a class
# are dumped whether the user declared them or not.  Those that were
# implicitly declared by the xml_generator are marked as "artificial".

# CastXML has no version number for the moment so "None" is used.
CASTXML_None = "CastXML None"


def on_missing_functionality(xml_generator, functionality):
    raise NotImplementedError(
        '"%s" xml_generator doesn\'t support functionality "%s"' %
        (xml_generator, functionality))
