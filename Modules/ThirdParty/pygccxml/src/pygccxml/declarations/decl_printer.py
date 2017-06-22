# Copyright 2014-2017 Insight Software Consortium.
# Copyright 2004-2009 Roman Yakovenko, 2006 Allen Bierbaum, Matthias Baas
# Distributed under the Boost Software License, Version 1.0.
# See http://www.boost.org/LICENSE_1_0.txt

"""
defines class, :class:`decl_printer_t` that prints declarations tree in a
user friendly format
"""

import os
import sys
from . import calldef_members
from . import algorithm
from . import decl_visitor
from . import variable_t
from . import calldef_t
from . import type_traits_classes


def _stdout_writer(x):
    sys.stdout.write(x)


class decl_printer_t(decl_visitor.decl_visitor_t):

    """helper class for printing declarations tree"""

    JUSTIFY = 20
    INDENT_SIZE = 4

    def __init__(
            self,
            level=0,
            print_details=True,
            recursive=True,
            writer=_stdout_writer,
            verbose=True):
        decl_visitor.decl_visitor_t.__init__(self)
        self.__inst = None
        self.__level = level
        self.__print_details = print_details
        self.__recursive = recursive
        self.__verbose = verbose
        self.__writer = writer

    def clone(self, increment_level=True):
        level = self.__level
        if increment_level:
            level += 1
        return decl_printer_t(
            level,
            self.print_details,
            recursive=self.recursive,
            verbose=self.__verbose,
            writer=self.writer)

    @property
    def recursive(self):
        return self.__recursive

    @recursive.setter
    def recursive(self, recursive):
        self.__recursive = recursive

    @property
    def verbose(self):
        return self.__verbose

    @verbose.setter
    def verbose(self, verbose):
        self.__verbose = verbose

    @property
    def level(self):
        return self.__level

    @level.setter
    def level(self, level):
        self.__level = level

    @property
    def print_details(self):
        return self.__print_details

    @print_details.setter
    def print_details(self, details):
        self.__print_details = details

    @property
    def writer(self):
        return self.__writer

    @writer.setter
    def writer(self, writer):
        self.__writer = writer

    @property
    def instance(self):
        return self.__inst

    @instance.setter
    def instance(self, inst):
        self.__inst = inst

    @staticmethod
    def is_builtin_decl(decl):
        if not decl.name.startswith('__builtin_'):
            return False
        # FIXME: This won't probably not work with CastXML.
        # It either needs to be removed or improved
        return decl.location and decl.location.file_name \
            and decl.location.file_name.endswith('gccxml_builtins.h')

    @staticmethod
    def __nice_decl_name(inst):
        name = inst.__class__.__name__
        return name
        # if name.endswith( '_t' ):
        #    name = name[:-len('_t')]
        # return name.replace( '_', ' ' )

    def print_decl_header(self):
        header = self.__nice_decl_name(
            self.__inst) + ": '%s'" % self.__inst.name
        self.writer(
            ' ' *
            self.level *
            self.INDENT_SIZE +
            header.ljust(
                self.JUSTIFY) +
            os.linesep)
        if self.__print_details:
            curr_level = self.level + 1
            if self.__inst.location:
                location = 'location: [%s]:%s' % (
                    self.__inst.location.file_name, self.__inst.location.line)
                self.writer(
                    ' ' *
                    curr_level *
                    self.INDENT_SIZE +
                    location)
            if self.verbose:
                artificial = 'artificial: ' + \
                    "'%s'" % str(self.__inst.is_artificial)
                self.writer(
                    ' ' *
                    curr_level *
                    self.INDENT_SIZE +
                    artificial.ljust(
                        self.JUSTIFY))
                if self.__inst.attributes:
                    attributes = 'attributes: %s' % self.__inst.attributes
                    self.writer(
                        ' ' *
                        curr_level *
                        self.INDENT_SIZE +
                        attributes)
                if self.__inst.demangled:
                    # Working only with gccxml.
                    # No demangled attribute with castxml
                    demangled = 'demangled: %s' % self.__inst.demangled
                    self.writer(
                        ' ' *
                        curr_level *
                        self.INDENT_SIZE +
                        demangled)

                # Mangled name is only available for functions and variables
                # when using castxml.
                print_mangled = False
                if self.__inst.mangled and \
                    (isinstance(self.__inst, variable_t) or
                     isinstance(self.__inst, calldef_t)):
                    print_mangled = True

                if print_mangled:
                    mangled = 'mangled: %s' % self.__inst.mangled
                    self.writer(
                        ' ' *
                        curr_level *
                        self.INDENT_SIZE +
                        mangled)

    def print_calldef_info(self, decl=None):
        if None is decl:
            decl = self.__inst

        retval = None
        if decl.return_type:
            retval = decl.return_type.decl_string
        args = []
        for arg in decl.arguments:
            args.append(arg.decl_type.decl_string + ' ' + arg.name)
        indent = ' ' * (self.level + 1) * self.INDENT_SIZE
        self.writer(indent + "is extern: " + str(decl.has_extern))
        self.writer(indent + "return type: " + str(retval))
        self.writer(indent + "arguments type: " + ', '.join(args))
        self.writer(
            indent +
            "calling convention: __%s__" %
            decl.calling_convention +
            os.linesep)
        if isinstance(decl, calldef_members.member_calldef_t):
            self.writer(indent +
                        "virtual: " +
                        str(decl.virtuality) +
                        os.linesep)
            self.writer(indent +
                        "is const: " +
                        str(decl.has_const) +
                        os.linesep)
            self.writer(indent +
                        "is static: " +
                        str(decl.has_static) +
                        os.linesep)

    def visit_member_function(self):
        self.print_decl_header()
        self.print_calldef_info()

    def visit_constructor(self):
        self.print_decl_header()
        self.print_calldef_info()

        indent = ' ' * (self.level + 1) * self.INDENT_SIZE
        self.writer(indent +
                    "explicit: " +
                    str(self.__inst.explicit) +
                    os.linesep)

        if self.__print_details:
            self.writer(indent +
                        'copy constructor: ' +
                        str(type_traits_classes.is_copy_constructor(
                            self.__inst)) +
                        os.linesep)

    def visit_destructor(self):
        self.print_decl_header()

    def visit_member_operator(self):
        self.print_decl_header()
        self.print_calldef_info()

    def visit_casting_operator(self):
        self.print_decl_header()
        self.print_calldef_info()

    def visit_free_function(self):
        self.print_decl_header()
        self.print_calldef_info()

    def visit_free_operator(self):
        self.print_decl_header()
        self.print_calldef_info()

    def visit_class_declaration(self):
        self.print_decl_header()

    def visit_class(self):
        self.print_decl_header()
        curr_level = self.level + 1
        class_type = 'class type: ' + "'%s'" % str(self.__inst.class_type)
        self.writer(
            ' ' *
            curr_level *
            self.INDENT_SIZE +
            class_type.ljust(
                self.JUSTIFY))
        if self.__print_details:
            byte_size = 'size: %d' % self.__inst.byte_size
            self.writer(
                ' ' *
                curr_level *
                self.INDENT_SIZE +
                byte_size.ljust(
                    self.JUSTIFY))
            try:
                byte_align = 'align: %d' % self.__inst.byte_align
                self.writer(
                    ' ' *
                    curr_level *
                    self.INDENT_SIZE +
                    byte_align.ljust(
                        self.JUSTIFY))
            except NotImplementedError:
                self.writer(
                    ' ' *
                    curr_level *
                    self.INDENT_SIZE +
                    "align: not implemented".ljust(
                        self.JUSTIFY))

        if self.__inst.aliases:
            aliases = sorted([typedef.name for typedef in self.__inst.aliases])
            msg = 'aliases: ' + repr(aliases)
            self.writer(
                ' ' *
                curr_level *
                self.INDENT_SIZE +
                msg.ljust(
                    self.JUSTIFY) +
                os.linesep)

        def print_hierarchy(hierarchy_type, classes, curr_level):
            self.writer(
                ' ' *
                curr_level *
                self.INDENT_SIZE +
                hierarchy_type.ljust(
                    self.JUSTIFY) +
                os.linesep)
            curr_level += 1
            for class_ in classes:
                class_str = 'class: ' + \
                    "'%s'" % str(class_.related_class.decl_string)
                self.writer(
                    ' ' *
                    curr_level *
                    self.INDENT_SIZE +
                    class_str.ljust(
                        self.JUSTIFY) +
                    os.linesep)
                access = 'access type: ' + "'%s'" % str(class_.access)
                self.writer(' ' *
                            (curr_level +
                             1) *
                            self.INDENT_SIZE +
                            access.ljust(self.JUSTIFY) +
                            os.linesep)
                if class_.is_virtual is not None:
                    is_virtual = 'virtual inheritance: ' + \
                        "'%s'" % str(class_.is_virtual)
                    self.writer(' ' *
                                (curr_level +
                                 1) *
                                self.INDENT_SIZE +
                                is_virtual.ljust(self.JUSTIFY) +
                                os.linesep)

        def print_members(members_type, members, curr_level):
            members = sorted(members[:])
            self.writer(
                ' ' *
                curr_level *
                self.INDENT_SIZE +
                members_type.ljust(
                    self.JUSTIFY) +
                os.linesep)
            if self.__recursive:
                curr_level += 1
                for member in members:
                    prn = self.clone()
                    prn.instance = member
                    algorithm.apply_visitor(prn, member)

        if self.__inst.bases:
            print_hierarchy('base classes: ', self.__inst.bases, curr_level)

        if self.__inst.derived:
            print_hierarchy(
                'derived classes: ',
                self.__inst.derived,
                curr_level)

        print_members('public: ', self.__inst.public_members, curr_level)
        print_members('protected: ', self.__inst.protected_members, curr_level)
        print_members('private: ', self.__inst.private_members, curr_level)

    def visit_enumeration(self):
        self.print_decl_header()
        curr_level = self.level + 1
        self.writer(
            ' ' *
            curr_level *
            self.INDENT_SIZE +
            'values:'.ljust(
                self.JUSTIFY) +
            os.linesep)
        value_level = ' ' * (curr_level + 1) * self.INDENT_SIZE
        self.writer(os.linesep + os.linesep)
        for name, value in self.__inst.values:
            self.writer(value_level + "%s : %s" % (name, value) + os.linesep)

    def visit_namespace(self):
        if not self.verbose and not self.__inst.declarations:
            return  # don't print info about empty namespaces
        self.print_decl_header()
        if self.__recursive:
            inst_decls = sorted(self.__inst.declarations[:])
            for decl in inst_decls:
                if self.is_builtin_decl(decl):
                    continue
                prn = self.clone()
                prn.instance = decl
                algorithm.apply_visitor(prn, decl)

    def visit_typedef(self):
        self.print_decl_header()
        curr_level = self.level + 1
        self.writer(
            ' ' *
            curr_level *
            self.INDENT_SIZE +
            'alias to: ' +
            self.__inst.decl_type.decl_string +
            os.linesep)

    def visit_variable(self):
        self.print_decl_header()
        curr_level = self.level + 1
        self.writer(
            ' ' *
            curr_level *
            self.INDENT_SIZE +
            'type: %s' %
            self.__inst.decl_type.decl_string)
        self.writer(
            ' ' *
            curr_level *
            self.INDENT_SIZE +
            'value: %s' %
            self.__inst.value)
        if self.__print_details:
            if self.__inst.bits:
                bits = 'bits: %d' % self.__inst.bits
                self.writer(
                    ' ' *
                    curr_level *
                    self.INDENT_SIZE +
                    bits.ljust(
                        self.JUSTIFY))

            byte_size = 'size: %d' % self.__inst.decl_type.byte_size
            self.writer(
                ' ' *
                curr_level *
                self.INDENT_SIZE +
                byte_size.ljust(
                    self.JUSTIFY))
            try:
                byte_align = 'align: %d' % self.__inst.decl_type.byte_align
                self.writer(
                    ' ' *
                    curr_level *
                    self.INDENT_SIZE +
                    byte_align.ljust(
                        self.JUSTIFY))
            except NotImplementedError:
                self.writer(
                    ' ' *
                    curr_level *
                    self.INDENT_SIZE +
                    "align: not implemented".ljust(
                        self.JUSTIFY))
            byte_offset = 'offset: %d' % self.__inst.byte_offset
            self.writer(
                ' ' *
                curr_level *
                self.INDENT_SIZE +
                byte_offset +
                os.linesep)


def print_declarations(
        decls,
        detailed=True,
        recursive=True,
        writer=lambda x: sys.stdout.write(x + os.linesep),
        verbose=True):
    """
    print declarations tree rooted at each of the included nodes.

    :param decls: either a single :class:declaration_t object or list
        of :class:declaration_t objects
    """
    prn = decl_printer_t(0, detailed, recursive, writer, verbose=verbose)
    if not isinstance(decls, list):
        decls = [decls]
    for d in decls:
        prn.level = 0
        prn.instance = d
        algorithm.apply_visitor(prn, d)


def dump_declarations(declarations, file_path):
    """
    Dump declarations tree rooted at each of the included nodes to the file

    :param declarations: either a single :class:declaration_t object or a list
        of :class:declaration_t objects
    :param file_path: path to a file

    """

    with open(file_path, "w+") as f:
        print_declarations(declarations, writer=f.write)
