# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

import os

from . import templates
from . import matcher_base_t
from . import variable
from . import cpptypes
from . import namespace
from . import calldef
from . import calldef_members
from .. import utils


class declaration_matcher_t(matcher_base_t):

    """
    Instance of this class will match declarations by next criteria:
          - declaration name, also could be fully qualified name
            Example: `wstring` or `::std::wstring`
          - declaration type
            Example: :class:`class_t`, :class:`namespace_t`,
            :class:`enumeration_t`
          - location within file system ( file or directory )
    """

    def __init__(
            self,
            name=None,
            decl_type=None,
            header_dir=None,
            header_file=None):
        """
        :param decl_type: declaration type to match by. For example
        :class:`enumeration_t`.
        :type decl_type: any class that derives from :class:`declaration_t`
        class

        :param name: declaration name, could be full name.
        :type name: str

        :param header_dir: absolute directory path
        :type header_dir: str

        :param header_file: absolute file path
        :type header_file: str

        """
        # An other option is that pygccxml will create absolute path using
        # os.path.abspath function. But I think this is just wrong, because
        # abspath builds path using current working directory. This behavior
        # is fragile and very difficult to find a bug.
        matcher_base_t.__init__(self)
        self.decl_type = decl_type
        self.__name = None
        self.__opt_is_tmpl_inst = None
        self.__opt_tmpl_name = None
        self.__opt_is_full_name = None
        self.__decl_name_only = None

        # Set the name through the setter.
        self.name = name

        self.header_dir = header_dir
        self.header_file = header_file

        if self.header_dir:
            self.header_dir = utils.normalize_path(self.header_dir)
            if not os.path.isabs(self.header_dir):
                raise RuntimeError(
                    "Path to header directory should be absolute!")

        if self.header_file:
            self.header_file = utils.normalize_path(self.header_file)
            if not os.path.isabs(self.header_file):
                raise RuntimeError("Path to header file should be absolute!")

    @property
    def name(self):
        return self.__name

    @name.setter
    def name(self, name):
        self.__name = name
        if not self.__name:
            self.__opt_is_tmpl_inst = None
            self.__opt_tmpl_name = None
            self.__opt_is_full_name = None
            self.__decl_name_only = None
        else:
            self.__opt_is_tmpl_inst = templates.is_instantiation(self.__name)
            self.__opt_tmpl_name = templates.name(self.__name)
            if self.__opt_is_tmpl_inst:
                if '::' in self.__opt_tmpl_name:
                    self.__opt_is_full_name = True
                    self.__decl_name_only = \
                        self.__opt_tmpl_name.split('::')[-1]
                else:
                    self.__opt_is_full_name = False
                    self.__decl_name_only = self.__opt_tmpl_name
                self.__name = templates.normalize(name)
            else:
                if '::' in self.__name:
                    self.__opt_is_full_name = True
                    self.__decl_name_only = self.__name.split('::')[-1]
                else:
                    self.__opt_is_full_name = False
                    self.__decl_name_only = self.__name

    def __str__(self):
        msg = []
        if self.decl_type is not None:
            msg.append('(decl type==%s)' % self.decl_type.__name__)
        if self.name is not None:
            msg.append('(name==%s)' % self.name)
        if self.header_dir is not None:
            msg.append('(header dir==%s)' % self.header_dir)
        if self.header_file is not None:
            msg.append('(header file==%s)' % self.header_file)
        if not msg:
            msg.append('any')
        return ' and '.join(msg)

    def __call__(self, decl):
        if self.decl_type is not None:
            if not isinstance(decl, self.decl_type):
                return False
        if self.name is not None:
            if not self.check_name(decl):
                return False
        if self.header_dir is not None:
            if decl.location:
                decl_dir = os.path.abspath(
                    os.path.dirname(decl.location.file_name))
                decl_dir = utils.normalize_path(decl_dir)
                if decl_dir[:len(self.header_dir)] != self.header_dir:
                    return False
            else:
                return False
        if self.header_file is not None:
            if decl.location:
                decl_file = os.path.abspath(decl.location.file_name)
                decl_file = utils.normalize_path(decl_file)
                if decl_file != self.header_file:
                    return False
            else:
                return False
        return True

    def check_name(self, decl):
        assert self.name is not None
        if self.__opt_is_tmpl_inst:
            if not self.__opt_is_full_name:
                if self.name != templates.normalize_name(decl) \
                   and self.name != templates.normalize_partial_name(decl):
                    return False
            else:
                if self.name != templates.normalize_full_name_true(decl) and \
                        self.name != templates.normalize_full_name_false(decl):
                    return False
        else:
            if not self.__opt_is_full_name:
                if self.name != decl.name and self.name != decl.partial_name:
                    return False
            else:
                if self.name != templates.normalize_full_name_true(decl) and \
                        self.name != templates.normalize_full_name_false(decl):
                    return False
        return True

    def is_full_name(self):
        return self.__opt_is_full_name

    @property
    def decl_name_only(self):
        return self.__decl_name_only


class variable_matcher_t(declaration_matcher_t):

    """
    Instance of this class will match variables by next criteria:
        - :class:`declaration_matcher_t` criteria
        - variable type. Example: :class:`int_t` or 'int'
    """

    def __init__(
            self,
            name=None,
            decl_type=None,
            header_dir=None,
            header_file=None):
        """
        :param decl_type: variable type
        :type decl_type: string or instance of :class:`type_t` derived class
        """

        declaration_matcher_t.__init__(
            self,
            name=name,
            decl_type=variable.variable_t,
            header_dir=header_dir,
            header_file=header_file)
        self._decl_type = decl_type

    def __call__(self, decl):
        if not super(variable_matcher_t, self).__call__(decl):
            return False
        if self._decl_type is not None:
            if isinstance(self._decl_type, cpptypes.type_t):
                if self._decl_type != decl.decl_type:
                    return False
            else:
                if self._decl_type != decl.decl_type.decl_string:
                    return False
        return True

    def __str__(self):
        msg = [super(variable_matcher_t, self).__str__()]
        if msg == ['any']:
            msg = []
        if self._decl_type is not None:
            msg.append('(value type==%s)' % str(self._decl_type))
        if not msg:
            msg.append('any')
        return ' and '.join(msg)


class namespace_matcher_t(declaration_matcher_t):

    """Instance of this class will match namespaces by name."""

    def __init__(self, name=None):
        declaration_matcher_t.__init__(
            self,
            name=name,
            decl_type=namespace.namespace_t)

    def __call__(self, decl):
        if self.name and decl.name == '':
            # unnamed namespace have same name as thier parent, we should
            # prevent this happens. The price is: user should search for
            # unnamed namespace directly.
            return False
        return super(namespace_matcher_t, self).__call__(decl)


class calldef_matcher_t(declaration_matcher_t):

    """
    Instance of this class will match callable by the following criteria:
       * :class:`declaration_matcher_t` criteria
       * return type. For example: :class:`int_t` or 'int'
       * argument types

    """

    def __init__(
            self,
            name=None,
            return_type=None,
            arg_types=None,
            decl_type=None,
            header_dir=None,
            header_file=None):
        """
        :param return_type: callable return type
        :type return_type: string or instance of :class:`type_t` derived class

        :type arg_types: list
        :param arg_types: list of function argument types. `arg_types` can
                          contain.
                          Any item within the list could be string or instance
                          of :class:`type_t` derived class. If you don't want
                          some argument to participate in match you can put
                          None.

        For example:

          .. code-block:: python

             calldef_matcher_t( arg_types=[ 'int &', None ] )

        will match all functions that takes 2 arguments, where the first one is
        reference to integer and second any
        """
        if None is decl_type:
            decl_type = calldef.calldef_t
        declaration_matcher_t.__init__(
            self,
            name=name,
            decl_type=decl_type,
            header_dir=header_dir,
            header_file=header_file)

        self.return_type = return_type
        self.arg_types = arg_types

    def __call__(self, decl):
        if not super(calldef_matcher_t, self).__call__(decl):
            return False
        if self.return_type is not None \
           and not self.__compare_types(self.return_type, decl.return_type):
            return False
        if self.arg_types:
            if isinstance(self.arg_types, (list, tuple)):
                if len(self.arg_types) != len(decl.arguments):
                    return False
                for type_or_str, arg in zip(self.arg_types, decl.arguments):
                    if type_or_str is None:
                        continue
                    else:
                        if not self.__compare_types(
                                type_or_str, arg.decl_type):
                            return False
        return True

    @staticmethod
    def __compare_types(type_or_str, decl_type):
        assert type_or_str
        if decl_type is None:
            return False
        if isinstance(type_or_str, cpptypes.type_t):
            if type_or_str != decl_type:
                return False
        else:
            if type_or_str != decl_type.decl_string:
                return False
        return True

    def __str__(self):
        msg = [super(calldef_matcher_t, self).__str__()]
        if msg == ['any']:
            msg = []
        if self.return_type is not None:
            msg.append('(return type==%s)' % str(self.return_type))
        if self.arg_types:
            for i, arg_type in enumerate(self.arg_types):
                if arg_type is None:
                    msg.append('(arg %d type==any)' % i)
                else:
                    msg.append('(arg %d type==%s)' % (i, str(arg_type)))
        if not msg:
            msg.append('any')
        return ' and '.join(msg)


class operator_matcher_t(calldef_matcher_t):

    """
    Instance of this class will match operators by next criteria:
        * :class:`calldef_matcher_t` criteria
        * operator symbol: =, !=, (), [] and etc
    """

    def __init__(
            self,
            name=None,
            symbol=None,
            return_type=None,
            arg_types=None,
            decl_type=None,
            header_dir=None,
            header_file=None):
        """
        :param symbol: operator symbol
        :type symbol: str
        """
        if None is decl_type:
            decl_type = calldef_members.operator_t
        calldef_matcher_t.__init__(
            self,
            name=name,
            return_type=return_type,
            arg_types=arg_types,
            decl_type=decl_type,
            header_dir=header_dir,
            header_file=header_file)
        self.symbol = symbol

    def __call__(self, decl):
        if not super(operator_matcher_t, self).__call__(decl):
            return False
        if self.symbol is not None:
            if self.symbol != decl.symbol:
                return False
        return True

    def __str__(self):
        msg = [super(operator_matcher_t, self).__str__()]
        if msg == ['any']:
            msg = []
        if self.symbol is not None:
            msg.append('(symbol==%s)' % str(self.symbol))
        if not msg:
            msg.append('any')
        return ' and '.join(msg)
