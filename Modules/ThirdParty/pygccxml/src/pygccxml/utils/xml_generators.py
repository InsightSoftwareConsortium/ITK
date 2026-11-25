# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt


class xml_generators(object):
    """
    Helper class for the xml generator versions
    """
    __gccxml_06 = "GCC-XML 0.6"
    __gccxml_07 = "GCC-XML 0.7"
    __gccxml_09 = "GCC-XML 0.9"
    __gccxml_09_buggy = "GCC-XML 0.9 BUGGY"
    __castxml = "CastXML (gccxml like)"
    __castxml1 = "CastXML (format version 1)"
    __separator = "@"

    def __init__(self, logger, gccxml_cvs_revision=None, castxml_format=None):
        """
        Create a new xml_generators object.

        Args:
            logger (logging.Logger) : a logger for debugging output
            gccxml_cvs_revision (str|None): the xml output version
            castxml_format (str|None): the xml output version
        """

        if castxml_format is not None and gccxml_cvs_revision is not None:
            raise RuntimeError("Setting both gccxml_cvs_revision and"
                               "castxml_format is not allowed!")

        self._is_castxml1 = False
        self._is_castxml = False
        self._is_gccxml = False

        if castxml_format is not None:
            self._xml_generator_version = self.__castxml
            self._xml_output_version = castxml_format
            self._is_castxml = True
            self._is_castxml1 = True
        elif gccxml_cvs_revision is not None:
            self._xml_generator_version, self._xml_output_version = \
                self.__extract_versions(logger, gccxml_cvs_revision)
            self._is_gccxml = "GCC-XML" in self._xml_generator_version
            self._is_castxml = "CastXML" in self._xml_generator_version
        else:
            raise RuntimeError("Either castxml_format or gccxml_cvs_revision"
                               "need to be defined!")

    def __extract_versions(self, logger, xml_output_version):
        if xml_output_version is None or xml_output_version == "0.6":
            # It is not clear what gccxml 0.6 had as version (0.6 or None),
            # but both cases get caught here.
            logger.debug("GCCXML version - 0.6")
            xml_generator = self.__gccxml_06
            xml_output_version = 0.6
        else:
            xml_output_version = float(xml_output_version)
            if xml_output_version <= 1.114:
                logger.debug("GCCXML version - 0.7")
                xml_generator = self.__gccxml_07
            elif 1.115 <= xml_output_version <= 1.126:
                logger.debug(
                    "GCCXML version - 0.9 BUGGY ( %s )", xml_output_version)
                xml_generator = self.__gccxml_09_buggy
            elif 1.127 <= xml_output_version <= 1.135:
                logger.debug(
                    "GCCXML version - 0.9 ( %s )", xml_output_version)
                xml_generator = self.__gccxml_09
            else:
                # CastXML starts with revision 1.136, but still writes the
                # GCCXML tag and the 0.9 version number in the XML files
                # for backward compatibility.
                logger.debug(
                    "CASTXML version - None ( %s )", xml_output_version)
                xml_generator = self.__castxml
        return xml_generator, str(xml_output_version)

    def get_string_repr(self):
        """
        Get a string identifier for the current type of xml generator

        Returns:
            str: identifier
        """
        return \
            self._xml_generator_version + \
            self.__separator + \
            str(self._xml_output_version)

    @property
    def is_gccxml(self):
        """
        Is the current xml generator gccxml?

        Returns:
            bool: is gccxml being used?
        """
        return self._is_gccxml

    @property
    def is_castxml(self):
        """
        Is the current xml generator castxml?

        Returns:
            bool: is castxml being used?
        """
        return self._is_castxml

    @property
    def is_castxml1(self):
        """
        Is the current xml generator castxml (with output format version 1)?

        Returns:
            bool: is castxml (with output format version 1) being used?
        """
        return self._is_castxml1

    @property
    def is_gccxml_06(self):
        """
        Is the current xml generator gccxml (version 0.6)?

        Returns:
            bool: is gccxml 0.6 being used?
        """
        return self._xml_generator_version == self.__gccxml_06

    @property
    def is_gccxml_07(self):
        """
        Is the current xml generator gccxml (version 0.7)?

        Returns:
            bool: is gccxml 0.7 being used?
        """
        return self._xml_generator_version == self.__gccxml_07

    @property
    def is_gccxml_09(self):
        """
        Is the current xml generator gccxml (version 0.9)?

        Returns:
            bool: is gccxml 0.9 being used?
        """
        return self._xml_generator_version == self.__gccxml_09

    @property
    def is_gccxml_09_buggy(self):
        """
        Is the current xml generator gccxml (version 0.9 - buggy)?

        Returns:
            bool: is gccxml 0.9 (buggy) being used?
        """
        return self._xml_generator_version == self.__gccxml_09_buggy

    @property
    def xml_output_version(self):
        """
        The current xml output version for the parsed file.

        Returns:
            str: the xml output version
        """
        return self._xml_output_version
