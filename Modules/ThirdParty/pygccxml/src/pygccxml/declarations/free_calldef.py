# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko.
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

from . import calldef
from . import calldef_members
from . import calldef_types
from . import declaration_utils
from . import cpptypes
from . import type_traits
from . import type_traits_classes


class free_calldef_t(calldef.calldef_t):

    """base class for "callable" declarations that defined within
    C++ namespace"""

    def __init__(self, *args, **keywords):
        calldef.calldef_t.__init__(self, *args, **keywords)

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
        # extern?
        if self.has_extern:
            res = "extern " + res
        # Append the declaration class
        cls = self.__class__.__name__
        if cls[-2:] == "_t":
            cls = cls[:-2]
        cls = cls.replace('_', ' ')
        return "%s [%s]" % (res, cls)

    def _get__cmp__call_items(self):
        """implementation details"""
        return []

    def function_type(self):
        """returns function type. See :class:`type_t` hierarchy"""
        return cpptypes.free_function_type_t(
            return_type=self.return_type,
            arguments_types=[
                arg.decl_type for arg in self.arguments])

    def create_decl_string(self, with_defaults=True):
        f_type = self.function_type()
        if with_defaults:
            return f_type.decl_string

        return f_type.partial_decl_string

    def guess_calling_convention(self):
        """This function should be overriden in the derived classes and return
        more-or-less successfull guess about calling convention"""
        return calldef_types.CALLING_CONVENTION_TYPES.UNKNOWN


class free_function_t(free_calldef_t):

    """describes free function declaration"""

    def __init__(self, *args, **keywords):
        free_calldef_t.__init__(self, *args, **keywords)

    def get_mangled_name(self):
        if not self._mangled \
           and '<' not in self.name and not self.overloads:
            # it is possible we deal with C function, so lets put it name as
            # mangled one
            return self.name

        return self._mangled


class free_operator_t(free_calldef_t, calldef_members.operator_t):

    """describes free operator declaration"""

    def __init__(self, *args, **keywords):
        free_calldef_t.__init__(self, *args, **keywords)
        calldef_members.operator_t.__init__(self, *args, **keywords)
        self.__class_types = None

    @property
    def class_types(self):
        """list of class/class declaration types, extracted from the
        operator arguments"""

        if None is self.__class_types:
            self.__class_types = []
            for type_ in self.argument_types:
                decl = None
                type_ = type_traits.remove_reference(type_)
                if type_traits_classes.is_class(type_):
                    decl = type_traits_classes.class_traits.get_declaration(
                        type_)
                elif type_traits_classes.is_class_declaration(type_):
                    tt = type_traits_classes.class_declaration_traits
                    decl = tt.get_declaration(type_)
                else:
                    pass
                if decl:
                    self.__class_types.append(decl)
        return self.__class_types
