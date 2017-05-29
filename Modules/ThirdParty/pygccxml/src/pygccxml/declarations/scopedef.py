# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""Defines :class:`scopedef_t` class"""

import time
import warnings
import collections
from . import algorithm
from . import templates
from . import declaration
from . import mdecl_wrapper
from . import byte_info
from . import elaborated_info
from . import runtime_errors
from .. import utils


class matcher(object):

    """Class-namespace, contains implementation of a few "find" algorithms"""

    @staticmethod
    def find(decl_matcher, decls, recursive=True):
        """
        Returns a list of declarations that match `decl_matcher` defined
        criteria or None

        :param decl_matcher: Python callable object, that takes one argument -
            reference to a declaration
        :param decls: the search scope, :class:declaration_t object or
            :class:declaration_t objects list t
        :param recursive: boolean, if True, the method will run `decl_matcher`
            on the internal declarations too
        """

        where = []
        if isinstance(decls, list):
            where.extend(decls)
        else:
            where.append(decls)
        if recursive:
            where = make_flatten(where)
        return list(filter(decl_matcher, where))

    @staticmethod
    def find_single(decl_matcher, decls, recursive=True):
        """
        Returns a reference to the declaration, that match `decl_matcher`
        defined criteria.

        if a unique declaration could not be found the method will return None.

        :param decl_matcher: Python callable object, that takes one argument -
            reference to a declaration
        :param decls: the search scope, :class:declaration_t object or
            :class:declaration_t objects list t
        :param recursive: boolean, if True, the method will run `decl_matcher`
            on the internal declarations too
        """
        answer = matcher.find(decl_matcher, decls, recursive)
        if len(answer) == 1:
            return answer[0]

    @staticmethod
    def get_single(decl_matcher, decls, recursive=True):
        """
        Returns a reference to declaration, that match `decl_matcher` defined
        criteria.

        If a unique declaration could not be found, an appropriate exception
        will be raised.

        :param decl_matcher: Python callable object, that takes one argument -
            reference to a declaration
        :param decls: the search scope, :class:declaration_t object or
            :class:declaration_t objects list t
        :param recursive: boolean, if True, the method will run `decl_matcher`
            on the internal declarations too
        """
        answer = matcher.find(decl_matcher, decls, recursive)
        if len(answer) == 1:
            return answer[0]
        elif not answer:
            raise runtime_errors.declaration_not_found_t(decl_matcher)
        else:
            raise runtime_errors.multiple_declarations_found_t(decl_matcher)


class scopedef_t(declaration.declaration_t):

    """
    Base class for :class:`namespace_t` and :class:`class_t` classes.

    This is the base class for all declaration classes that may have
    children nodes. The children can be accessed via the
    :attr:`scopedef_t.declarations` property.

    Also this class provides "get/select/find" interface. Using this class you
    can get instance or instances of internal declaration(s).

    You can find declaration(s) using next criteria:

        1. `name` - declaration name, could be full qualified name

        2. `header_dir` - directory, to which belongs file, that the
            declaration was declared in.
            `header_dir` should be absolute path.

        3. `header_file` - file that the declaration was declared in.

        4. `function` - user ( your ) custom criteria. The interesting thing
            is that this function will be joined with other arguments
            (criteria).

        5. `recursive` - the search declaration range, if True will be search
            in internal declarations too.


    Every ""query"" API, takes name or function as the first argument.

    .. code-block:: python

        global_namespace.member_function("do_something)

    the statement returns reference to member function named "do_something".
    If there the function doesn't exist or more than one function exists,
    an exception is raised.

    If you want to query for many declarations, use other function(s):

    .. code-block:: python

        do_something = global_namespace.member_functions("do_something")

    the statement returns :class:`mdecl_wrapper_t` instance. That object will
    save you writing `for` loops. For more information see
    :class:`the class <mdecl_wrapper_t>` documentation.
    """

    RECURSIVE_DEFAULT = True
    ALLOW_EMPTY_MDECL_WRAPPER = False

    @property
    def declaration_not_found_t(self):
        warnings.warn(
            "The declaration_not_found_t attribute is deprecated. Please use"
            "pygccxml.declaration.declaration_not_found_t instead.",
            DeprecationWarning)
        # Deprecated since 1.9.0, will be removed in 2.0.0
        return runtime_errors.declaration_not_found_t

    @property
    def multiple_declarations_found_t(self):
        warnings.warn(
            "The multiple_declarations_found_t attribute is deprecated. "
            "Please use pygccxml.declaration.multiple_declarations_found_t "
            "instead.",
            DeprecationWarning)
        # Deprecated since 1.9.0, will be removed in 2.0.0
        return runtime_errors.multiple_declarations_found_t

    # this class variable is used to prevent recursive imports
    _impl_matchers = {}
    # this class variable is used to prevent recursive imports
    _impl_decl_types = {}
    # this class variable is used to prevent recursive imports
    _impl_all_decl_types = []

    def __init__(self, name=''):
        declaration.declaration_t.__init__(self, name)

        self._optimized = False
        self._type2decls = {}
        self._type2name2decls = {}
        self._type2decls_nr = {}
        self._type2name2decls_nr = {}
        self._all_decls = None
        self._all_decls_not_recursive = []

    @property
    def _logger(self):
        """reference to :attr:`pygccxml.utils.loggers.queries_engine` logger"""
        return utils.loggers.queries_engine

    def _get__cmp__scope_items(self):
        """implementation details"""
        raise NotImplementedError()

    def _get__cmp__items(self):
        """implementation details"""
        items = []
        if self._optimized:
            # in this case we don't need to build class internal declarations
            # list
            items.append(self._all_decls_not_recursive.sort())
        else:
            items.append(self.declarations.sort())
        items.extend(self._get__cmp__scope_items())
        return items

    def __eq__(self, other):
        if not declaration.declaration_t.__eq__(self, other):
            return False
        return self.declarations[:].sort() == other.declarations[:].sort()

    def __hash__(self):
        return super(scopedef_t, self).__hash__()

    def _get_declarations_impl(self):
        raise NotImplementedError()

    @property
    def declarations(self):
        """
        List of children declarations.

        Returns:
            List[declarations.declaration_t]
        """
        if self._optimized:
            return self._all_decls_not_recursive
        else:
            return self._get_declarations_impl()

    @declarations.setter
    def declarations(self, declarations):
        """
        Set list of all declarations defined in the namespace.

        Args:
            List[declarations.declaration_t]: list of declarations

        Not implemented.

        """
        raise NotImplementedError()

    def remove_declaration(self, decl):
        raise NotImplementedError()

    @staticmethod
    def __decl_types(decl):
        """implementation details"""
        types = []
        bases = list(decl.__class__.__bases__)
        if 'pygccxml' in decl.__class__.__module__:
            types.append(decl.__class__)
        while bases:
            base = bases.pop()
            if base is declaration.declaration_t:
                continue
            if base is byte_info.byte_info:
                continue
            if base is elaborated_info.elaborated_info:
                continue
            if 'pygccxml' not in base.__module__:
                continue
            types.append(base)
            bases.extend(base.__bases__)
        return types

    def clear_optimizer(self):
        """Cleans query optimizer state"""
        self._optimized = False
        self._type2decls = {}
        self._type2name2decls = {}
        self._type2decls_nr = {}
        self._type2name2decls_nr = {}
        self._all_decls = None
        self._all_decls_not_recursive = None

        for decl in self.declarations:
            if isinstance(decl, scopedef_t):
                decl.clear_optimizer()

    def init_optimizer(self):
        """
        Initializes query optimizer state.

        There are 4 internals hash tables:
            1. from type to declarations
            2. from type to declarations for non-recursive queries
            3. from type to name to declarations
            4. from type to name to declarations for non-recursive queries

        Almost every query includes declaration type information. Also very
        common query is to search some declaration(s) by name or full name.
        Those hash tables allows to search declaration very quick.
        """
        if self.name == '::':
            self._logger.debug(
                "preparing data structures for query optimizer - started")
        start_time = time.clock()

        self.clear_optimizer()

        for dtype in scopedef_t._impl_all_decl_types:
            self._type2decls[dtype] = []
            self._type2decls_nr[dtype] = []
            self._type2name2decls[dtype] = {}
            self._type2name2decls_nr[dtype] = {}

        self._all_decls_not_recursive = self.declarations
        self._all_decls = make_flatten(
            self._all_decls_not_recursive)
        for decl in self._all_decls:
            types = self.__decl_types(decl)
            for type_ in types:
                self._type2decls[type_].append(decl)
                name2decls = self._type2name2decls[type_]
                if decl.name not in name2decls:
                    name2decls[decl.name] = []
                name2decls[decl.name].append(decl)
                if self is decl.parent:
                    self._type2decls_nr[type_].append(decl)
                    name2decls_nr = self._type2name2decls_nr[type_]
                    if decl.name not in name2decls_nr:
                        name2decls_nr[decl.name] = []
                    name2decls_nr[decl.name].append(decl)

        for decl in self._all_decls_not_recursive:
            if isinstance(decl, scopedef_t):
                decl.init_optimizer()
        if self.name == '::':
            self._logger.debug((
                "preparing data structures for query optimizer - " +
                "done( %f seconds ). "), (time.clock() - start_time))
        self._optimized = True

    @staticmethod
    def _build_operator_function(name, function):
        if isinstance(name, collections.Callable):
            return name
        else:
            return function

    @staticmethod
    def _build_operator_name(name, function, symbol):
        """implementation details"""
        def add_operator(sym):
            if 'new' in sym or 'delete' in sym:
                return 'operator ' + sym
            else:
                return 'operator' + sym
        if isinstance(name, collections.Callable) and None is function:
            name = None
        if name:
            if 'operator' not in name:
                name = add_operator(name)
            return name
        elif symbol:
            return add_operator(symbol)
        return name  # both name and symbol are None

    def _on_rename(self):
        for decl in self.decls(allow_empty=True):
            decl.cache.reset_name_based()
        # I am not sure whether to introduce this or not?
        # It could be very time consuming operation + it changes optimize query
        # data structures.
        # if self.parent:
        #    if self.parent._optimized:
        #        self.parent.init_optimizer()

    @staticmethod
    def __normalize_args(**keywds):
        """implementation details"""
        if isinstance(keywds['name'], collections.Callable) and \
                None is keywds['function']:
            keywds['function'] = keywds['name']
            keywds['name'] = None
        return keywds

    def __findout_recursive(self, **keywds):
        """implementation details"""
        if None is keywds['recursive']:
            return self.RECURSIVE_DEFAULT
        else:
            return keywds['recursive']

    def __findout_allow_empty(self, **keywds):
        """implementation details"""
        if None is keywds['allow_empty']:
            return self.ALLOW_EMPTY_MDECL_WRAPPER
        else:
            return keywds['allow_empty']

    @staticmethod
    def __findout_decl_type(match_class, **keywds):
        """implementation details"""
        if 'decl_type' in keywds:
            return keywds['decl_type']

        matcher_args = keywds.copy()
        del matcher_args['function']
        del matcher_args['recursive']
        if 'allow_empty' in matcher_args:
            del matcher_args['allow_empty']

        decl_matcher = match_class(**matcher_args)
        if decl_matcher.decl_type:
            return decl_matcher.decl_type
        return None

    def __create_matcher(self, match_class, **keywds):
        """implementation details"""
        matcher_args = keywds.copy()
        del matcher_args['function']
        del matcher_args['recursive']
        if 'allow_empty' in matcher_args:
            del matcher_args['allow_empty']

        decl_matcher = decl_matcher = match_class(**matcher_args)
        if keywds['function']:
            self._logger.debug(
                'running query: %s and <user defined function>',
                str(decl_matcher))
            return lambda decl: decl_matcher(decl) and keywds['function'](decl)
        else:
            self._logger.debug('running query: %s', str(decl_matcher))
            return decl_matcher

    def __findout_range(self, name, decl_type, recursive):
        """implementation details"""
        if not self._optimized:
            self._logger.debug(
                'running non optimized query - optimization has not been done')
            decls = self.declarations
            if recursive:
                decls = make_flatten(self.declarations)
            if decl_type:
                decls = [d for d in decls if isinstance(d, decl_type)]
            return decls

        if name and templates.is_instantiation(name):
            # templates has tricky mode to compare them, so lets check the
            # whole range
            name = None

        if name and decl_type:
            impl_match = scopedef_t._impl_matchers[scopedef_t.decl](name=name)
            if impl_match.is_full_name():
                name = impl_match.decl_name_only
            if recursive:
                self._logger.debug(
                    'query has been optimized on type and name')
                return self._type2name2decls[decl_type].get(name, [])
            else:
                self._logger.debug(
                    'non recursive query has been optimized on type and name')
                return self._type2name2decls_nr[decl_type].get(name, [])
        elif decl_type:
            if recursive:
                self._logger.debug('query has been optimized on type')
                return self._type2decls[decl_type]
            else:
                self._logger.debug(
                    'non recursive query has been optimized on type')
                return self._type2decls_nr[decl_type]
        else:
            if recursive:
                self._logger.debug((
                    'query has not been optimized ( hint: query does not ' +
                    'contain type and/or name )'))
                return self._all_decls
            else:
                self._logger.debug((
                    'non recursive query has not been optimized ( hint: ' +
                    'query does not contain type and/or name )'))
                return self._all_decls_not_recursive

    def _find_single(self, match_class, **keywds):
        """implementation details"""
        self._logger.debug('find single query execution - started')
        start_time = time.clock()
        norm_keywds = self.__normalize_args(**keywds)
        decl_matcher = self.__create_matcher(match_class, **norm_keywds)
        dtype = self.__findout_decl_type(match_class, **norm_keywds)
        recursive_ = self.__findout_recursive(**norm_keywds)
        decls = self.__findout_range(norm_keywds['name'], dtype, recursive_)
        found = matcher.get_single(decl_matcher, decls, False)
        self._logger.debug(
            'find single query execution - done( %f seconds )',
            (time.clock() - start_time))
        return found

    def _find_multiple(self, match_class, **keywds):
        """implementation details"""
        self._logger.debug('find all query execution - started')
        start_time = time.clock()
        norm_keywds = self.__normalize_args(**keywds)
        decl_matcher = self.__create_matcher(match_class, **norm_keywds)
        dtype = self.__findout_decl_type(match_class, **norm_keywds)
        recursive_ = self.__findout_recursive(**norm_keywds)
        allow_empty = self.__findout_allow_empty(**norm_keywds)
        decls = self.__findout_range(norm_keywds['name'], dtype, recursive_)
        found = matcher.find(decl_matcher, decls, False)
        mfound = mdecl_wrapper.mdecl_wrapper_t(found)
        self._logger.debug('%d declaration(s) that match query', len(mfound))
        self._logger.debug(
            'find single query execution - done( %f seconds )',
            (time.clock() - start_time))
        if not mfound and not allow_empty:
            raise RuntimeError(
                "Multi declaration query returned 0 declarations.")
        return mfound

    def decl(
            self,
            name=None,
            function=None,
            decl_type=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to declaration, that is matched defined
        criteria"""
        return (
            self._find_single(
                self._impl_matchers[
                    scopedef_t.decl],
                name=name,
                function=function,
                decl_type=decl_type,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def decls(
            self,
            name=None,
            function=None,
            decl_type=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of declarations, that are matched defined criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[
                    scopedef_t.decl],
                name=name,
                function=function,
                decl_type=decl_type,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def class_(
            self,
            name=None,
            function=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to class declaration, that is matched defined
        criteria"""
        return (
            self._find_single(
                self._impl_matchers[scopedef_t.class_],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.class_],
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def classes(
            self,
            name=None,
            function=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of class declarations, that are matched defined
        criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[scopedef_t.class_],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.class_],
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def variable(
            self,
            name=None,
            function=None,
            decl_type=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to variable declaration, that is matched defined
        criteria"""

        return (
            self._find_single(
                self._impl_matchers[
                    scopedef_t.variable],
                name=name,
                function=function,
                decl_type=decl_type,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def variables(
            self,
            name=None,
            function=None,
            decl_type=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):

        """returns a set of variable declarations, that are matched defined
        criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[
                    scopedef_t.variable],
                name=name,
                function=function,
                decl_type=decl_type,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def calldef(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to "calldef" declaration, that is matched defined
        criteria"""
        return (
            self._find_single(
                self._impl_matchers[scopedef_t.calldef],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.calldef],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def calldefs(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of :class:`calldef_t` declarations, that are matched
        defined criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[scopedef_t.calldef],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.calldef],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def operator(
            self,
            name=None,
            function=None,
            symbol=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to operator declaration, that is matched
        defined criteria"""
        return (
            self._find_single(
                self._impl_matchers[scopedef_t.operator],
                name=self._build_operator_name(name,
                                               function,
                                               symbol),
                symbol=symbol,
                function=self._build_operator_function(name,
                                                       function),
                decl_type=self._impl_decl_types[scopedef_t.operator],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def operators(
            self,
            name=None,
            function=None,
            symbol=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of operator declarations, that are matched
        defined criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[scopedef_t.operator],
                name=self._build_operator_name(name,
                                               function,
                                               symbol),
                symbol=symbol,
                function=self._build_operator_function(name,
                                                       function),
                decl_type=self._impl_decl_types[scopedef_t.operator],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def member_function(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to member declaration, that is matched
        defined criteria"""
        return (
            self._find_single(
                self._impl_matchers[scopedef_t.member_function],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.member_function],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def mem_fun(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """
        Deprecated method. Use the member_function() method instead.

        Deprecated since v1.9.0. Will be removed in v2.0.0
        """
        warnings.warn(
            "The mem_fun() method is deprecated. \n" +
            "Please use the member_function() method instead.",
            DeprecationWarning)

        return self.member_function(
            name, function,
            return_type, arg_types,
            header_dir, header_file,
            recursive)

    def member_functions(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of member function declarations, that are matched
        defined criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[scopedef_t.member_function],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.member_function],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def mem_funs(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """
        Deprecated method. Use the member_functions() method instead.

        Deprecated since v1.9.0. Will be removed in v2.0.0
        """
        warnings.warn(
            "The mem_funs() method is deprecated. \n" +
            "Please use the member_functions() method instead.",
            DeprecationWarning)

        return self.member_functions(
            name, function,
            return_type, arg_types,
            header_dir, header_file,
            recursive, allow_empty)

    def constructor(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to constructor declaration, that is matched
        defined criteria"""
        return (
            self._find_single(
                self._impl_matchers[scopedef_t.constructor],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.constructor],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def constructors(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of constructor declarations, that are matched
        defined criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[scopedef_t.constructor],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.constructor],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def member_operator(
            self,
            name=None,
            function=None,
            symbol=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to member operator declaration, that is matched
        defined criteria"""
        return (
            self._find_single(
                self._impl_matchers[scopedef_t.member_operator],
                name=self._build_operator_name(name,
                                               function,
                                               symbol),
                symbol=symbol,
                function=self._build_operator_function(name,
                                                       function),
                decl_type=self._impl_decl_types[scopedef_t.member_operator],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def mem_oper(
            self,
            name=None,
            function=None,
            symbol=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """
        Deprecated method. Use the member_operator() method instead.

        Deprecated since v1.9.0. Will be removed in v2.0.0
        """
        warnings.warn(
            "The mem_oper() method is deprecated. \n" +
            "Please use the member_operator() method instead.",
            DeprecationWarning)

        return self.member_operator(
            name, function,
            symbol, return_type,
            arg_types, header_dir,
            header_file, recursive)

    def member_operators(
            self,
            name=None,
            function=None,
            symbol=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of member operator declarations, that are matched
        defined criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[scopedef_t.member_operator],
                name=self._build_operator_name(name,
                                               function,
                                               symbol),
                symbol=symbol,
                function=self._build_operator_function(name,
                                                       function),
                decl_type=self._impl_decl_types[scopedef_t.member_operator],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def mem_opers(
            self,
            name=None,
            function=None,
            symbol=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """
        Deprecated method. Use the member_operators() method instead.

        Deprecated since v1.9.0. Will be removed in v2.0.0
        """
        warnings.warn(
            "The mem_opers() method is deprecated. \n" +
            "Please use the member_operators() method instead.",
            DeprecationWarning)

        return self.member_operators(
            name, function,
            symbol, return_type,
            arg_types, header_dir,
            header_file, recursive,
            allow_empty)

    def casting_operator(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to casting operator declaration, that is matched
        defined criteria"""
        return (
            self._find_single(
                self._impl_matchers[scopedef_t.casting_operator],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.casting_operator],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def casting_operators(
            self,
            name=None,
            function=None,
            return_type=None,
            arg_types=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of casting operator declarations, that are matched
        defined criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[scopedef_t.casting_operator],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.casting_operator],
                return_type=return_type,
                arg_types=arg_types,
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def enumeration(
            self,
            name=None,
            function=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to enumeration declaration, that is matched
        defined criteria"""
        return (
            self._find_single(
                self._impl_matchers[scopedef_t.enumeration],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.enumeration],
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def enum(
            self,
            name=None,
            function=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """
        Deprecated method. Use the enumeration() method instead.

        Deprecated since v1.9.0. Will be removed in v2.0.0
        """
        warnings.warn(
            "The enum() method is deprecated. \n" +
            "Please use the enumeration() method instead.",
            DeprecationWarning)

        return self.enumeration(
            name, function, header_dir, header_file, recursive)

    def enumerations(
            self,
            name=None,
            function=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of enumeration declarations, that are matched
        defined criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[scopedef_t.enumeration],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.enumeration],
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def enums(
            self,
            name=None,
            function=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """
        Deprecated method. Use the enumerations() method instead.

        Deprecated since v1.9.0. Will be removed in v2.0.0
        """
        warnings.warn(
            "The enums() method is deprecated. \n" +
            "Please use the enumerations() method instead.",
            DeprecationWarning)

        return self.enumerations(
            name, function, header_dir, header_file, recursive, allow_empty)

    def typedef(
            self,
            name=None,
            function=None,
            header_dir=None,
            header_file=None,
            recursive=None):
        """returns reference to typedef declaration, that is matched
        defined criteria"""
        return (
            self._find_single(
                self._impl_matchers[scopedef_t.typedef],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.typedef],
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive)
        )

    def typedefs(
            self,
            name=None,
            function=None,
            header_dir=None,
            header_file=None,
            recursive=None,
            allow_empty=None):
        """returns a set of typedef declarations, that are matched
        defined criteria"""
        return (
            self._find_multiple(
                self._impl_matchers[scopedef_t.typedef],
                name=name,
                function=function,
                decl_type=self._impl_decl_types[
                    scopedef_t.typedef],
                header_dir=header_dir,
                header_file=header_file,
                recursive=recursive,
                allow_empty=allow_empty)
        )

    def __getitem__(self, name_or_function):
        """
        Allow simple name based find of declarations.  Internally just calls
        `decls` method.
        :param name_or_function:  Name of `decl` to lookup or finder function.
        """
        return self.decls(name_or_function)

    def i_depend_on_them(self, recursive=True):
        raise NotImplementedError()


def make_flatten(decl_or_decls):
    """
    Converts tree representation of declarations to flatten one.

    :param decl_or_decls: reference to list of declaration's or single
        declaration
    :type decl_or_decls: :class:`declaration_t` or [ :class:`declaration_t` ]
    :rtype: [ all internal declarations ]

    """

    def proceed_single(decl):
        answer = [decl]
        if not isinstance(decl, scopedef_t):
            return answer
        for elem in decl.declarations:
            if isinstance(elem, scopedef_t):
                answer.extend(proceed_single(elem))
            else:
                answer.append(elem)
        return answer

    decls = []
    if isinstance(decl_or_decls, list):
        decls.extend(decl_or_decls)
    else:
        decls.append(decl_or_decls)
    answer = []
    for decl in decls:
        answer.extend(proceed_single(decl))
    return answer


def find_all_declarations(
        declarations,
        decl_type=None,
        name=None,
        parent=None,
        recursive=True,
        fullname=None):
    """
    Returns a list of all declarations that match criteria, defined by
    developer.

    For more information about arguments see :class:`match_declaration_t`
    class.

    :rtype: [ matched declarations ]

    """

    if recursive:
        decls = make_flatten(declarations)
    else:
        decls = declarations

    return list(
        filter(
            algorithm.match_declaration_t(
                decl_type=decl_type,
                name=name,
                fullname=fullname,
                parent=parent),
            decls))


def find_declaration(
        declarations,
        decl_type=None,
        name=None,
        parent=None,
        recursive=True,
        fullname=None):
    """
    Returns single declaration that match criteria, defined by developer.
    If more the one declaration was found None will be returned.

    For more information about arguments see :class:`match_declaration_t`
    class.

    :rtype: matched declaration :class:`declaration_t` or None

    """

    decl = find_all_declarations(
        declarations,
        decl_type=decl_type,
        name=name,
        parent=parent,
        recursive=recursive,
        fullname=fullname)
    if len(decl) == 1:
        return decl[0]


def find_first_declaration(
        declarations,
        decl_type=None,
        name=None,
        parent=None,
        recursive=True,
        fullname=None):
    """
    Returns first declaration that match criteria, defined by developer.

    For more information about arguments see :class:`match_declaration_t`
    class.

    :rtype: matched declaration :class:`declaration_t` or None

    """

    decl_matcher = algorithm.match_declaration_t(
        decl_type=decl_type,
        name=name,
        fullname=fullname,
        parent=parent)
    if recursive:
        decls = make_flatten(declarations)
    else:
        decls = declarations
    for decl in decls:
        if decl_matcher(decl):
            return decl
    return None


def declaration_files(decl_or_decls):
    """
    Returns set of files

    Every declaration is declared in some file. This function returns set, that
    contains all file names of declarations.

    :param decl_or_decls: reference to list of declaration's or single
        declaration
    :type decl_or_decls: :class:`declaration_t` or [:class:`declaration_t`]
    :rtype: set(declaration file names)

    """

    files = set()
    decls = make_flatten(decl_or_decls)
    for decl in decls:
        if decl.location:
            files.add(decl.location.file_name)
    return files
