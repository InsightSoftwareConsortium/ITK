# Copyright 2014-2015 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
Logger classes and a few convenience methods.

"""

import os
import sys
import platform
import logging
import tempfile
import subprocess


def is_str(s):
    """
    Python 2 and 3 compatible string checker.

    """

    if sys.version_info >= (3, 0):
        return isinstance(s, str)
    else:
        return isinstance(s, basestring)


def find_xml_generator(name=None):
    """
    Try to find a c++ parser. Returns path and name.

    :param name: name of the c++ parser: castxml or gccxml
    :type name: str

    If no name is given the function first looks for gccxml,
    then for castxml. If no c++ parser is found the function
    raises an exception.

    """

    if platform.system() == "Windows":
        command = "where"
    else:
        command = "which"

    if name is None:
        name = "gccxml"
        p = subprocess.Popen([command, name], stdout=subprocess.PIPE)
        path = p.stdout.read().decode("utf-8")
        if path == "":
            name = "castxml"
            p = subprocess.Popen([command, name], stdout=subprocess.PIPE)
            path = p.stdout.read().decode("utf-8")
    else:
        p = subprocess.Popen([command, name], stdout=subprocess.PIPE)
        path = p.stdout.read().decode("utf-8")
    if path == "":
        raise(Exception(
            "No c++ parser found. Please install castxml or gccxml."))
    else:
        return path.rstrip(), name


def _create_logger_(name):
    """
    Implementation detail, creates a logger.

    """

    logger = logging.getLogger(name)
    handler = logging.StreamHandler()
    handler.setFormatter(logging.Formatter('%(levelname)s %(message)s'))
    logger.addHandler(handler)
    logger.setLevel(logging.INFO)
    return logger


class loggers(object):
    """
    Class-namespace, defines a few loggers classes, used in the project.

    """

    cxx_parser = _create_logger_('pygccxml.cxx_parser')
    """
    Logger for C++ parser functionality

    If you set this logger level to DEBUG, you will be able to see the exact
    command line, used to invoke GCC-XML  and errors that occures during XML
    parsing

    """

    pdb_reader = _create_logger_('pygccxml.pdb_reader')
    """
    Logger for MS .pdb file reader functionality

    """

    queries_engine = _create_logger_('pygccxml.queries_engine')
    """
    Logger for query engine functionality.

    If you set this logger level to DEBUG, you will be able to see what queries
    you do against declarations tree, measure performance and may be even to
    improve it.
    Query engine reports queries and whether they are optimized or not.

    """

    declarations_cache = _create_logger_('pygccxml.declarations_cache')
    """
    Logger for declarations tree cache functionality

    If you set this logger level to DEBUG, you will be able to see what is
    exactly happens, when you read the declarations from cache file. You will
    be able to decide, whether it worse for you to use this or that cache
    strategy.

    """

    root = logging.getLogger('pygccxml')
    """
    Root logger exists for your convenience only.

    """

    all_loggers = [
        root, cxx_parser, queries_engine, declarations_cache, pdb_reader]
    """
    Contains all logger classes, defined by the class.

    """

    @staticmethod
    def set_level(level):
        """
        Set the same logging level for all the loggers at once.

        """

        for logger in loggers.all_loggers:
            logger.setLevel(level)


def remove_file_no_raise(file_name, config):
    """
    Removes file from disk if exception is raised.

    """

    # The removal can be disabled by the config for debugging purposes.
    if config.keep_xml:
        return True

    try:
        if os.path.exists(file_name):
            os.remove(file_name)
    except Exception as error:
        loggers.root.error(
            "Error ocured while removing temprorary created file('%s'): %s" %
            (file_name, str(error)))


def create_temp_file_name(suffix, prefix=None, dir=None):
    """
    Small convenience function that creates temporary files.

    This function is a wrapper around the Python built-in
    function tempfile.mkstemp.

    """

    if not prefix:
        prefix = tempfile.gettempprefix()
    fd, name = tempfile.mkstemp(suffix=suffix, prefix=prefix, dir=dir)
    file_obj = os.fdopen(fd)
    file_obj.close()
    return name


def normalize_path(some_path):
    """
    Return os.path.normpath(os.path.normcase(some_path))

    """

    return os.path.normpath(os.path.normcase(some_path))


def contains_parent_dir(fpath, dirs):
    """
    Returns true if paths in dirs start with fpath.

    Precondition: dirs and fpath should be normalizeed before calling
    this function.

    """

    # Note: this function is used nowhere in pygccxml but is used
    # at least by pypluplus; so it should stay here.

    return bool([x for x in dirs if _f(fpath, x)])


def _f(fpath, dir_):
    """
    Helper function for contains_parent_dir function.

    """

    return fpath.startswith(dir_)


def get_architecture():
    """
    Returns computer architecture: 32 or 64.

    The guess is based on maxint.

    """

    if sys.maxsize == 2147483647:
        return 32
    elif sys.maxsize == 9223372036854775807:
        return 64
    else:
        raise RuntimeError("Unknown architecture")


class cached(property):
    """
    Convert a method into a cached attribute.

    """

    # The following code is cut-and-paste from this post:
    # http://groups.google.com/group/comp.lang.python/browse_thread/
    # thread/5b71896c06bd0f76/
    # Thanks to Michele Simionato

    def __init__(self, method):
        private = '_' + method.__name__

        def fget(s):
            try:
                return getattr(s, private)
            except AttributeError:
                value = method(s)
                setattr(s, private, value)
                return value

        def fdel(s):
            del s.__dict__[private]
        super(cached, self).__init__(fget, fdel=fdel)

    @staticmethod
    def reset(self):
        cls = self.__class__
        for name in dir(cls):
            attr = getattr(cls, name)
            if isinstance(attr, cached):
                delattr(self, name)


class enum(object):
    """
    Usage example:

    .. code-block:: python

       class fruits(enum):
           apple = 0
           orange = 1

       fruits.has_value( 1 )
       fruits.name_of( 1 )

    """

    @classmethod
    def has_value(cls, enum_numeric_value):
        for name, value in cls.__dict__.items():
            if enum_numeric_value == value:
                return True
        else:
            return False

    @classmethod
    def name_of(cls, enum_numeric_value):
        for name, value in cls.__dict__.items():
            if enum_numeric_value == value:
                return name
        else:
            raise RuntimeError(
                'Unable to find name for value(%d) in enumeration "%s"' %
                (enum_numeric_value, cls.__name__))


class native_compiler(object):
    """
    Provides information about the compiler which was used to build the
    Python executable

    """

    @staticmethod
    def get_version():
        if 'nt' != os.name:
            return None  # not implemented yet
        else:
            from distutils import msvccompiler
            return 'msvc', str(msvccompiler.get_build_version())

    @staticmethod
    def get_gccxml_compiler():
        compiler = native_compiler.get_version()
        if not compiler:
            return None
        else:
            n = compiler[1].replace('.', '')
            if n.endswith('0'):
                n = n[:-1]
            return compiler[0] + n.replace('.', '')


def get_tr1(name):
    """
    When using libstd++, there is a tr1 namespace.

    Note that tr1 was also replaced by std in declarations.py,
    for the parent attribute.

    Return either an empty string or tr1::, useful for
    appending to search patterns.

    """

    tr1 = ""
    if "tr1" in name:
        tr1 = "tr1::"
    return tr1
