# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
defines classes, that describes "callable" declarations

This modules contains definition for next C++ declarations:
    - operator
        - member
        - free
    - function
        - member
        - free
    - constructor
    - destructor
"""
from . import cpptypes
from . import declaration
from . import calldef_types


# First level in hierarchy of calldef
class argument_t(object):

    """
    class, that describes argument of "callable" declaration
    """

    def __init__(
            self,
            name='',
            decl_type=None,
            default_value=None,
            attributes=None):
        object.__init__(self)

        self._name = name
        self._default_value = default_value
        self._decl_type = decl_type
        self._attributes = attributes

    def clone(self, **keywd):
        """constructs new argument_t instance

        return argument_t(
            name=keywd.get('name', self.name),
            decl_type=keywd.get('decl_type', self.decl_type),
            default_value=keywd.get('default_value', self.default_value),
            attributes=keywd.get('attributes', self.attributes ))

        """
        return argument_t(
            name=keywd.get('name', self.name),
            decl_type=keywd.get('decl_type', self.decl_type),
            default_value=keywd.get('default_value', self.default_value),
            attributes=keywd.get('attributes', self.attributes))

    def __str__(self):
        if self.ellipsis:
            return "..."

        if self.default_value is None:
            return "%s %s" % (self.decl_type, self.name)

        return "%s %s=%s" % (self.decl_type, self.name, self.default_value)

    def __eq__(self, other):
        if not isinstance(other, self.__class__):
            return False
        return self.name == other.name \
            and self.default_value == other.default_value \
            and self.decl_type == other.decl_type

    def __hash__(self):
        return (hash(self.__class__) ^
                hash(self.name) ^
                hash(self.default_value) ^
                hash(self.decl_type))

    def __ne__(self, other):
        return not self.__eq__(other)

    def __lt__(self, other):
        if not isinstance(other, self.__class__):
            return self.__class__.__name__ < other.__class__.__name__
        if other.default_value is None:
            return False
        if self.default_value is None:
            return self.name < other.name \
                and self.decl_type < other.decl_type
        return self.name < other.name \
            and self.default_value < other.default_value \
            and self.decl_type < other.decl_type

    @property
    def name(self):
        """Argument name.
            @type: str"""
        return self._name

    @name.setter
    def name(self, name):
        self._name = name

    @property
    def ellipsis(self):
        """bool, if True argument represents ellipsis ( "..." )
        in function definition"""
        return isinstance(self.decl_type, cpptypes.ellipsis_t)

    @property
    def default_value(self):
        """Argument's default value or None.
            @type: str"""
        return self._default_value

    @default_value.setter
    def default_value(self, default_value):
        self._default_value = default_value

    @property
    def decl_type(self):
        return self._decl_type

    @decl_type.setter
    def decl_type(self, decl_type):
        self._decl_type = decl_type

    @property
    def attributes(self):
        """GCCXML attributes, set using __attribute__((gccxml("...")))
            @type: str"""
        return self._attributes

    @attributes.setter
    def attributes(self, attributes):
        self._attributes = attributes


class calldef_t(declaration.declaration_t):

    """base class for all "callable" declarations"""

    def __init__(
            self,
            name='',
            arguments=None,
            exceptions=None,
            return_type=None,
            has_extern=False,
            does_throw=True,
            mangled=None):
        declaration.declaration_t.__init__(self, name)
        if not arguments:
            arguments = []
        self._arguments = arguments
        if not exceptions:
            exceptions = []
        self._does_throw = does_throw
        self._exceptions = exceptions
        self._return_type = return_type
        self._has_extern = has_extern
        self._calling_convention = None
        self._has_inline = None
        self._mangled = mangled
        self._overrides = None

    def _get__cmp__call_items(self):
        """
        Implementation detail.

        """

        raise NotImplementedError()

    def _get__cmp__items(self):
        """
        Implementation detail.

        """

        items = [
            self.arguments,
            self.return_type,
            self.has_extern,
            self.does_throw,
            self.exceptions.sort(),
            self.has_inline]

        items.extend(self._get__cmp__call_items())
        return items

    def __eq__(self, other):
        if not declaration.declaration_t.__eq__(self, other):
            return False

        return self.return_type == other.return_type \
            and self.arguments == other.arguments \
            and self.has_extern == other.has_extern \
            and self.does_throw == other.does_throw \
            and self.exceptions.sort() == other.exceptions.sort()

    def __hash__(self):
        return (super(calldef_t, self).__hash__() ^
                hash(self.return_type) ^
                hash(self.name))

    @property
    def arguments(self):
        """The argument list.
            @type: list of :class:`argument_t`"""
        return self._arguments

    @arguments.setter
    def arguments(self, arguments):
        self._arguments = arguments

    @property
    def has_ellipsis(self):
        return self.arguments and self.arguments[-1].ellipsis

    @property
    def argument_types(self):
        """list of all argument types"""
        return [arg.decl_type for arg in self.arguments]

    @property
    def required_args(self):
        """list of all required arguments"""
        r_args = []
        for arg in self.arguments:
            if not arg.default_value:
                r_args.append(arg)
            else:
                break
        return r_args

    @property
    def optional_args(self):
        """list of all optional arguments, the arguments that have default
        value"""
        return self.arguments[len(self.required_args):]

    @property
    def overrides(self):
        """If a function is marked as an overrides, contains
        the declaration which this function overrides."""
        return self._overrides

    @overrides.setter
    def overrides(self, overrides):
        self._overrides = overrides

    @property
    def does_throw(self):
        """If False, than function does not throw any exception.
            In this case, function was declared with empty throw
            statement."""
        return self._does_throw

    @does_throw.setter
    def does_throw(self, does_throw):
        self._does_throw = does_throw

    @property
    def exceptions(self):
        """The list of exceptions.
            @type: list of :class:`declaration_t`"""
        return self._exceptions

    @exceptions.setter
    def exceptions(self, exceptions):
        self._exceptions = exceptions

    @property
    def return_type(self):
        """The type of the return value of the "callable" or None
            (constructors).
            @type: :class:`type_t`"""
        return self._return_type

    @return_type.setter
    def return_type(self, return_type):
        self._return_type = return_type

    @property
    def overloads(self):
        """A list of overloaded "callables" (i.e. other callables with the
        same name within the same scope.

        @type: list of :class:`calldef_t`
        """
        if not self.parent:
            return []
        # finding all functions with the same name
        return self.parent.calldefs(
            name=self.name,
            function=lambda decl: decl is not self,
            allow_empty=True,
            recursive=False)

    @property
    def has_extern(self):
        """Was this callable declared as "extern"?
            @type: bool"""
        return self._has_extern

    @has_extern.setter
    def has_extern(self, has_extern):
        self._has_extern = has_extern

    @property
    def has_inline(self):
        """Was this callable declared with "inline" specifier
            @type: bool"""
        return self._has_inline

    @has_inline.setter
    def has_inline(self, has_inline):
        self._has_inline = has_inline

    def _report(self, *args, **keywd):
        # Implementation detail. Will be removed when the deprecated
        # i_depend_on_them method is dropped
        from . import dependencies  # pylint: disable=R0401
        return dependencies.dependency_info_t(self, *args, **keywd)

    def i_depend_on_them(self, recursive=True):
        self._warn_deprecated()
        answer = []
        if self.return_type:
            answer.append(
                self._report(self.return_type, hint="return type"))
        for arg in self.arguments:
            answer.append(self._report(arg.decl_type))
        for exc in self.exceptions:
            answer.append(self._report(exc, hint="exception"))
        return answer

    # pylint: disable=R0201
    def guess_calling_convention(self):
        """This function should be overriden in the derived classes and return
        more-or-less successfull guess about calling convention"""
        return calldef_types.CALLING_CONVENTION_TYPES.UNKNOWN

    @property
    def calling_convention(self):
        """function calling convention. See
            :class:CALLING_CONVENTION_TYPES class for possible values"""
        if self._calling_convention is None:
            self._calling_convention = \
                calldef_types.CALLING_CONVENTION_TYPES.extract(self.attributes)
            if not self._calling_convention:
                self._calling_convention = self.guess_calling_convention()
        return self._calling_convention

    @calling_convention.setter
    def calling_convention(self, cc):
        self._calling_convention = cc

    @property
    def mangled(self):
        """
        Unique declaration name generated by the compiler.

        :return: the mangled name
        :rtype: str

        """

        return self.get_mangled_name()

    @mangled.setter
    def mangled(self, mangled):
        self._mangled = mangled
