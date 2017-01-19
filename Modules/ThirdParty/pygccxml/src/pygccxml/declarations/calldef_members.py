# Copyright 2014-2016 Insight Software Consortium.
# Copyright 2004-2008 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

import warnings
from . import calldef
from . import declaration_utils
from . import cpptypes
from . import calldef_types


# Second level in hierarchy of calldef
class member_calldef_t(calldef.calldef_t):

    """base class for "callable" declarations that defined within
    C++ class or struct"""

    def __init__(
            self,
            virtuality=None,
            has_const=None,
            has_static=None,
            *args,
            **keywords):
        calldef.calldef_t.__init__(self, *args, **keywords)
        self._virtuality = virtuality
        self._has_const = has_const
        self._has_static = has_static

    def __str__(self):
        # Get the full name of the calldef...
        name = declaration_utils.full_name(self)
        if name[:2] == "::":
            name = name[2:]
        # Add the arguments...
        args = [str(a) for a in self.arguments]
        res = "%s(%s)" % (name, ", ".join(args))
        # Add the return type...
        if self.return_type is not None:
            res = "%s %s" % (self.return_type, res)
        # const?
        if self.has_const:
            res += " const"
        # static?
        if self.has_static:
            res = "static " + res
        # Append the declaration class
        cls = self.__class__.__name__
        if cls[-2:] == "_t":
            cls = cls[:-2]
        cls = cls.replace('_', ' ')
        return "%s [%s]" % (res, cls)

    def _get__cmp__call_items(self):
        """implementation details"""
        return [self.virtuality, self.has_static, self.has_const]

    def __eq__(self, other):
        if not calldef.calldef_t.__eq__(self, other):
            return False
        return self.virtuality == other.virtuality \
            and self.has_static == other.has_static \
            and self.has_const == other.has_const

    def __hash__(self):
        return super.__hash__(self)

    @property
    def virtuality(self):
        """Describes the "virtuality" of the member (as defined by the
            string constants in the class :class:VIRTUALITY_TYPES).
            @type: str"""
        return self._virtuality

    @virtuality.setter
    def virtuality(self, virtuality):
        assert virtuality in calldef_types.VIRTUALITY_TYPES.ALL
        self._virtuality = virtuality

    @property
    def access_type(self):
        """Return the access type of the member (as defined by the
            string constants in the class :class:ACCESS_TYPES.
            @type: str"""
        return self.parent.find_out_member_access_type(self)

    @property
    def has_const(self):
        """describes, whether "callable" has const modifier or not"""
        return self._has_const

    @has_const.setter
    def has_const(self, has_const):
        self._has_const = has_const

    @property
    def has_static(self):
        """describes, whether "callable" has static modifier or not"""
        return self._has_static

    @has_static.setter
    def has_static(self, has_static):
        self._has_static = has_static

    def function_type(self):
        """returns function type. See :class:`type_t` hierarchy"""
        if self.has_static:
            return cpptypes.free_function_type_t(
                return_type=self.return_type,
                arguments_types=[arg.decl_type for arg in self.arguments])
        else:
            return cpptypes.member_function_type_t(
                class_inst=self.parent,
                return_type=self.return_type,
                arguments_types=[arg.decl_type for arg in self.arguments],
                has_const=self.has_const)

    def create_decl_string(self, with_defaults=True):
        f_type = self.function_type()
        if with_defaults:
            return f_type.decl_string
        else:
            return f_type.partial_decl_string

    def guess_calling_convention(self):
        if self.has_static:
            return calldef_types.CALLING_CONVENTION_TYPES.SYSTEM_DEFAULT
        else:
            return calldef_types.CALLING_CONVENTION_TYPES.THISCALL


class operator_t(object):

    """base class for "operator" declarations"""
    OPERATOR_WORD_LEN = len('operator')

    def __init__(self):
        object.__init__(self)

    @property
    def symbol(self):
        """operator's symbol. For example: operator+, symbol is equal to '+'"""
        return self.name[operator_t.OPERATOR_WORD_LEN:].strip()


# Third level in hierarchy of calldef
class member_function_t(member_calldef_t):

    """describes member function declaration"""

    def __init__(self, *args, **keywords):
        member_calldef_t.__init__(self, *args, **keywords)


class constructor_t(member_calldef_t):

    """describes constructor declaration"""

    def __init__(self, *args, **keywords):
        member_calldef_t.__init__(self, *args, **keywords)
        self._explicit = True

    @property
    def explicit(self):
        """True, if constructor has "explicit" keyword, False otherwise
            @type: bool"""
        return self._explicit

    @explicit.setter
    def explicit(self, explicit):
        if explicit in [True, '1']:
            self._explicit = True
        else:
            self._explicit = False

    def __str__(self):
        # Get the full name of the calldef...
        name = declaration_utils.full_name(self)
        if name[:2] == "::":
            name = name[2:]
        # Add the arguments...
        args = [str(a) for a in self.arguments]
        res = "%s(%s)" % (name, ", ".join(args))
        # Append the declaration class
        cls = 'constructor'
        return "%s [%s]" % (res, cls)

    @property
    def is_copy_constructor(self):
        """
        Returns True if described declaration is copy constructor,
        otherwise False.

        """

        # Deprecated since 1.8.0. Will be removed in 1.9.0
        warnings.warn(
            "The is_copy_constructor attribute is deprecated. \n" +
            "Please use the is_copy_constructor function from the \n" +
            "declarations module instead.",
            DeprecationWarning)

        # prevent cyclic dependencies
        from . import type_traits_classes
        return type_traits_classes.is_copy_constructor(self)

    @property
    def is_trivial_constructor(self):

        # Deprecated since 1.8.0. Will be removed in 1.9.0
        warnings.warn(
            "The is_trivial_constructor attribute is deprecated. \n" +
            "Please use the is_trivial_constructor function from the \n" +
            "declarations module instead.",
            DeprecationWarning)

        # prevent cyclic dependencies
        from . import type_traits_classes
        return type_traits_classes.is_trivial_constructor(self)


class destructor_t(member_calldef_t):

    """describes deconstructor declaration"""

    def __init__(self, *args, **keywords):
        member_calldef_t.__init__(self, *args, **keywords)


class member_operator_t(member_calldef_t, operator_t):

    """describes member operator declaration"""

    def __init__(self, *args, **keywords):
        member_calldef_t.__init__(self, *args, **keywords)
        operator_t.__init__(self, *args, **keywords)
        self.__class_types = None


class casting_operator_t(member_calldef_t, operator_t):

    """describes casting operator declaration"""

    def __init__(self, *args, **keywords):
        member_calldef_t.__init__(self, *args, **keywords)
        operator_t.__init__(self, *args, **keywords)
