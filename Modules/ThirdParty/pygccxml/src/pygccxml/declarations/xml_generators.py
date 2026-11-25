# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Contains enumeration of all xml_generators supported by the project.

"""

gccxml_06 = "GCC-XML 0.6"
gccxml_07 = "GCC-XML 0.7"
gccxml_09 = "GCC-XML 0.9"
gccxml_09_buggy = "GCC-XML 0.9 BUGGY"
# revision 122:
# After this fix, all constructors and destructors that exist for a class
# are dumped whether the user declared them or not.  Those that were
# implicitly declared by the xml_generator are marked as "artificial".

# CastXML has no version number for the moment so "None" is used.
castxml_none = "CastXML None"


def on_missing_functionality(xml_generator, functionality):
    raise NotImplementedError(
        '"%s" xml_generator doesn\'t support functionality "%s"' %
        (xml_generator, functionality))
