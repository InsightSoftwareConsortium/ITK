#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Import unicode literals so that StringIO works on both Python 2 and 3
from __future__ import unicode_literals
from __future__ import print_function

import sys
import os
import re
from argparse import ArgumentParser

try:
    # Python 3
    from io import StringIO
except ImportError:
    # Python 2
    from cStringIO import StringIO


def getType(v):
    if hasattr(v, "decl_type"):
        return getType(v.decl_type)
    if hasattr(v, "declaration"):
        return getType(v.declaration)
    return v


class IdxGenerator(object):
    """Generates a the .idx file for an ITK wrapping submodule (which usually
    corresponds to a class)."""

    def __init__(self, moduleName):
        self.moduleName = moduleName
        # the output file
        self.outputFile = StringIO()

    def create_idxfile(self, idxFilePath, wrappersNamespace):
        # iterate over all the typedefs in the _wrapping_::wrappers namespace
        for typedef in wrappersNamespace.typedefs():
            n = typedef.name
            s = getType(typedef).decl_string
            # drop the :: prefix - it make swig produce invalid code
            if s.startswith("::"):
                s = s[2:]
            self.outputFile.write("{%s} {%s} {%s}\n" % (s, n, self.moduleName))

        content = self.outputFile.getvalue()

        with open(idxFilePath, "w") as f:
            f.write(content)


class SwigInputGenerator(object):
    """Generates a swig input .i file for an ITK module."""

    notWrapped = [
        "std::_Deque_alloc<.+>",
        "itk::AtomicInt<.+>",
        "itk::MapContainer< unsigned long, itk::CellInterface<.+>",
        "itk::VectorContainer< unsigned long, itk::CellInterface<.+>",
        "itk::CellInterface< double, itk::QuadEdgeMeshCellTraitsInfo<.+>",
        "itk::QuadEdgeMeshLineCell< itk::CellInterface<.+>",
        "itk::SmartPointerForwardReference<.+>",
        "itk::LibHandle",
        "itk::NeighborhoodAllocator<.+>",
        # to avoid wrapping all the region for all the dims
        "itk::ImageRegion<.+>",
        "itk::ImportImageContainer<.+>",
        "itk::DefaultPixelAccessor<.+>",
        "itk::NeighborhoodAccessorFunctor<.+>",
        "itk::DefaultVectorPixelAccessor<.+>",
        "itk::VectorImageNeighborhoodAccessorFunctor<.+>",
        "itk::.*Iterator.*",  # TODO: remove this one ?
        "itk::Neighborhood<.+>",  # TODO: remove this one
        "itk::ThreadFunctionType",
        "itk::Functor::.+",
        "itk::SmartPointer< itk::Functor::.+",
        "itk::Function::.+",
        "itk::.+Function.*",  # Level set functions
        "itk::watershed::.+",  # ignore the internal classes of the watershed
        # require to wrap too more type
        "itk::SmartPointer< itk::VoronoiDiagram2D<.+> >",
        # used internally in ImageToImageMetric
        "itk::Image< itk::CovariantVector< double, \d+u >, \d+u >",
        "itk::FixedArray< itk::SmartPointer.+ >",
        # used internally in itkTransformBase
        "itk::SmartPointer< itk::Transform.+ >",
        # used internally in itkMattesMutualInformationImageToImageMetric
        "itk::SmartPointer< itk::Image.+ >",
        "itk::ObjectFactoryBasePrivate"
    ]

    notWrappedRegExp = re.compile("|".join(["^" + s + "$" for s in notWrapped]))

    # stdcomplex code

    stdcomplex_headers = {
        "D": """ class stdcomplexD {
       public:
         ~stdcomplexD();
         stdcomplexD & operator=(stdcomplexD const & arg0);
         stdcomplexD(stdcomplexD const & arg0);
         stdcomplexD(stdcomplexD __z);
         stdcomplexD(double __r = 0.0, double __i = 0.0);
         stdcomplexD(stdcomplexF const & __z);
         double real();
         double const real() const;
         double imag();
         double const imag() const;
         stdcomplexD & operator=(double __d);
         stdcomplexD & operator+=(double __d);
         stdcomplexD & operator-=(double __d);
         stdcomplexD & operator*=(double __d);
         stdcomplexD & operator/=(double __d);
         // stdcomplexD const & __rep() const;
       private:
       protected:
     };
    """,

        "F": """class stdcomplexF {
       public:
         ~stdcomplexF();
         stdcomplexF & operator=(stdcomplexF const & arg0);
         stdcomplexF(stdcomplexF const & arg0);
         stdcomplexF(stdcomplexF __z);
         stdcomplexF(float r = 0.0f, float i = 0.0f);
         stdcomplexF(stdcomplexD const & __z);
         float real();
         float const real() const;
         float imag();
         float const imag() const;
         stdcomplexF & operator=(float __f);
         stdcomplexF & operator+=(float __f);
         stdcomplexF & operator-=(float __f);
         stdcomplexF & operator*=(float __f);
         stdcomplexF & operator/=(float __f);
         // stdcomplexF const & __rep() const;
       private:
       protected:
     };
    """}

    def __init__(self, moduleName, options):
        self.moduleName = moduleName
        self.options = options

        self.outputFile = StringIO()
        self.applyFileNames = []

        # a dict to let us use the alias name instead of the full c++ name. Without
        # that, in many cases, swig don't know that's the same type
        self.aliases = {}

        # a set of used types
        self.usedTypes = set()

        # a dict to store the file where the def comes from
        self.typedefSource = {}

        self.warnings = set()

        self.mdx_loaded = set()

        self.verbose = options.verbose


    def warn(self, id, msg, doWarn=True):
        if not doWarn:
            # don't warn for anything
            return
        if str(id) not in self.options.warnings:
            if not self.verbose and (id, msg) in self.warnings:
                # just do nothing
                return
            self.warnings.add((id, msg))
            if self.verbose:
                if self.options.warningError:
                    print("error(%s): %s" % (str(id), msg), file=sys.stderr)
                else:
                    print("warning(%s): %s" % (str(id), msg), file=sys.stderr)
            else:
                if self.options.warningError:
                    print(
                        "%s: error(%s): %s" %
                        (self.moduleName, str(id), msg), file=sys.stderr)
                else:
                    print(
                        "%s: warning(%s): %s" %
                        (self.moduleName, str(id), msg), file=sys.stderr)

    def info(self, msg):
        if self.verbose:
            print("info: %s" % msg, file=sys.stderr)

    @staticmethod
    def getDeclarationString(t):
        t = getType(t)
        if t.decl_string == "::PyObject *":
            # don't go further - we want to keep that one as is
            return "::PyObject *"
        if isinstance(t, pygccxml.declarations.cpptypes.pointer_t):
            return SwigInputGenerator.getDeclarationString(getType(t.base)) + " *"
        elif isinstance(t, pygccxml.declarations.cpptypes.const_t):
            return SwigInputGenerator.getDeclarationString(getType(t.base)) + " const"
        elif isinstance(t, pygccxml.declarations.cpptypes.reference_t):
            return SwigInputGenerator.getDeclarationString(getType(t.base)) + " &"
        return t.decl_string

    def renameTypesInSTL(self, s):
        if s.startswith("std::") and \
                pygccxml.declarations.templates.is_instantiation(s):
            args = []
            for arg in pygccxml.declarations.templates.args(s):
                t, d = SwigInputGenerator.typeAndDecorators(arg)
                args.append(self.renameTypesInSTL(self.get_alias(t)) + d)
            return pygccxml.declarations.templates.join(
                pygccxml.declarations.templates.name(s),
                args) + SwigInputGenerator.typeAndDecorators(s)[1]
        return s

    @staticmethod
    def removeStdAllocator(s):
        if pygccxml.declarations.templates.is_instantiation(s):
            args = []
            for arg in pygccxml.declarations.templates.args(s):
                if not arg.startswith("std::allocator"):
                    t, d = SwigInputGenerator.typeAndDecorators(arg)
                    args.append(SwigInputGenerator.removeStdAllocator(t) + d)
            return pygccxml.declarations.templates.join(
                pygccxml.declarations.templates.name(s),
                args) + SwigInputGenerator.typeAndDecorators(s)[1]
        return s

    @staticmethod
    def typeAndDecorators(s):
        end = ""
        s = s.strip()
        ends = [" ", "*", "&", "const"]
        needToContinue = True
        while needToContinue:
            needToContinue = False
            for e in ends:
                if s.endswith(e):
                    end = e + end
                    s = s[:-len(e)]
                    needToContinue = True
        return (s, end)

    def get_alias(self, decl_string, w=True):
        s = str(decl_string)

        # drop the :: prefix - it make swig produce invalid code
        if s.startswith("::"):
            s = s[2:]

        # normalize string
        s = SwigInputGenerator.normalize(s)

        # workaround a bug - or is it a feature ? - somewhere
        s = s.replace("complex float", "std::complex<float>")
        s = s.replace("complex double", "std::complex<double>")
        s = s.replace("complex long double", "std::complex<long double>")

        (s, end) = SwigInputGenerator.typeAndDecorators(s)

        if s in self.aliases:
            self.usedTypes.add(self.aliases[s])
            return self.aliases[s] + end

        if s.startswith("itk::Templates::"):
            # that's a explicitly instantiated type. The name is the same than
            # the WrapITK one, so lets use it as a base for WrapITK
            # Ex: itk::Templates::RGBPixelUC
            # don't store the new string in s, because we need it unchanged if
            # the type is explicitly instantiated, but not wrapped
            new_s = s.replace("::Templates::", "")
            if new_s.split("::")[0] in self.aliases.values():
                self.usedTypes.add(new_s)
                return new_s + end

        if s[:s.rfind("::")] in self.aliases:
            # take care of subtypes/enum/...
            alias = self.aliases[s[:s.rfind("::")]] + s[s.rfind("::"):]
            self.usedTypes.add(alias)
            return alias + end

        # replace the types defined in this type, to support
        # std::vector<itkDataObject> for example
        s = self.renameTypesInSTL(s)

        # drop the allocator part of the type, because it is not supported by the
        # %template directive with some generators (like tcl)
        s = SwigInputGenerator.removeStdAllocator(s)

        # rename basic_string to std::string to make name shorter
        s = s.replace("std::basic_string< char >", "std::string")
        s = s.replace(
            "std::basic_string< char, std::char_traits< char > >",
            "std::string")
        s = s.replace(
            "std::basic_ostream< char, std::char_traits< char > >",
            "std::ostream")
        s = s.replace(
            "std::basic_istream< char, std::char_traits< char > >",
            "std::istream")
        s = s.replace(
            "std::basic_ofstream< char, std::char_traits< char > >",
            "std::ostream")
        s = s.replace(
            "std::basic_ifstream< char, std::char_traits< char > >",
            "std::istream")

        # rename some types not renamed by gccxml (why ?)
        s = s.replace("itk::SerieUIDContainer", "std::vector< std::string >")
        s = s.replace("itk::FilenamesContainer", "std::vector< std::string >")

        if s.startswith("itk::") and not self.notWrappedRegExp.match(s):
            self.warn(
                4,
                "ITK type not wrapped, or currently not known: %s" %
                s,
                w)

        self.usedTypes.add(s)
        return s + end

    def load_idx(self, file_name):
        with open(file_name, "r") as f:
            for line in f:
                (full_name, alias, module) = \
                    re.findall(r'{(.*)} {(.*)} {(.*)}', line)[0]
                # workaround lack of :: prefix in idx files
                # TODO: would it be better to remove the :: prefix in the output of
                # pygccxml ?
                # full_name = "::"+full_name
                # normalize some basic type names
                full_name = self.normalize(full_name)

                if full_name in self.aliases:
                    # If the full_name key alreay exists, do not overwrite the
                    # value. load_idx() is called once before load_mdx(), making
                    # sure the first aliases loaded are the ones belonging to
                    # the current submodule (and the next load_idx() calls
                    # should not overwrite these aliases.
                    continue

                self.aliases[full_name] = alias
                # store the source of the def
                if alias in self.typedefSource and file_name != self.typedefSource[alias]:
                    self.warn(
                        7, "%s in %s is already defined in %s." %
                        (alias, file_name, self.typedefSource[alias]))
                else:
                    self.typedefSource[alias] = file_name

    def load_mdx(self, file_name):
        if file_name in self.mdx_loaded:
            # already loaded - no need to do it again
            return
        self.mdx_loaded.add(file_name)
        with open(file_name, "r") as f:
            lines = f.readlines()
        for line in lines:
            line_stripped = line.strip()
            if line.startswith('%') or line.isspace():
                # exclude the lines which are starting with % - that's not the idx
                # files
                pass
            elif line_stripped.endswith(".mdx"):
                self.load_mdx(os.path.dirname(file_name) + os.sep + line_stripped)
            elif line_stripped[:-4] == self.moduleName:
                continue
            else:
                self.load_idx(os.path.dirname(file_name) + os.sep + line_stripped)

    @staticmethod
    def normalize(name):
        name = name.replace("short unsigned int", "unsigned short")
        name = name.replace("long unsigned int", "unsigned long")
        name = name.replace("long long unsigned int", "unsigned long long")
        name = name.replace("short int", "short")
        name = name.replace("long int", "long")
        name = name.replace("long long int", "long long")
    #  name = name.replace("unsigned int", "unsigned")
        # normalize spaces
        name = " ".join(name.replace(',', ', ').split())
        return name

    def generate_class(self, typedef, indent=0):
        self.info("Generating interface for %s." % typedef.name)

        decls = pygccxml.declarations

        if not typedef.name.startswith("stdcomplex"):
            super_classes = []
            for super_class in getType(typedef).bases:
                super_classes.append(
                    "%s %s" %
                    (super_class.access,
                     self.get_alias(
                         super_class.related_class.decl_string)))
            s = ""
            if super_classes:
                s = " : " + ", ".join(super_classes)
            self.outputFile.write("  " * indent)
            self.outputFile.write("class %s%s {\n" % (typedef.name, s))

            # iterate over access
            for access in decls.ACCESS_TYPES.ALL:

                # the access type
                self.outputFile.write("  " * indent)
                self.outputFile.write("  %s:\n" % access)

                # warnings or no warning?
                w = access not in self.options.access_warnings

                # iterate over the members
                for member in getType(typedef).get_members(access=access):
                    if isinstance(member, decls.typedef.typedef_t):
                        self.warn(
                            51,
                            "Member typedef are not supported: %s" %
                            member.name,
                            w)
                    elif isinstance(member, decls.member_function_t):
                        self.generate_method(typedef, member, indent, w)
                    elif isinstance(member, decls.constructor_t):
                        self.generate_constructor(typedef, member, indent, w)
                    elif isinstance(member, decls.member_operator_t):
                        self.generate_method(typedef, member, indent, w)
                    elif isinstance(member, decls.destructor_t):
                        self.generate_destructor(typedef, member, indent, w)
                    elif isinstance(member, decls.enumeration_t):
                        self.generate_nested_enum(typedef, member, indent, w)
                    elif isinstance(member, decls.variable_t):
                        self.warn(
                            52,
                            "Member variables are not supported: %s" %
                            member.name,
                            w)
                    elif isinstance(member, decls.class_declaration.class_t):
                        self.warn(
                            53,
                            "Member classes are not supported: %s" %
                            member.name,
                            w)
                    elif isinstance(
                            member, decls.class_declaration.class_declaration_t):
                        self.warn(
                            53,
                            "Member classes are not supported: %s" %
                            member.name,
                            w)
                    elif isinstance(member, decls.casting_operator_t):
                        self.warn(
                            54,
                            "Member casting operators are not supported: %s" %
                            member.name,
                            w)
                    else:
                        self.warn(
                            50,
                            "Unknown member type: %s" %
                            repr(member),
                            w)

            # finally, close the class
            self.outputFile.write("  " * indent)
            self.outputFile.write("};\n\n\n")

        elif typedef.name == "stdcomplexD":
            self.outputFile.write(self.stdcomplex_headers["D"] + '\n')
        elif typedef.name == "stdcomplexF":
            self.outputFile.write(self.stdcomplex_headers["F"] + '\n')
        else:
            print('stdcomplex', typedef.name)
            # stdcomplex is too difficult to wrap in some cases. Only wrap the
            # constructor.
            self.outputFile.write("  " * indent)
            self.outputFile.write("class %s%s {\n" % (typedef.name, s))

            # iterate over access
            for access in pygccxml.declarations.ACCESS_TYPES.ALL:

                # the access type
                self.outputFile.write("  " * indent)
                self.outputFile.write("  %s:\n" % access)

                # warnings or no warning?
                w = access not in self.options.access_warnings
                for member in getType(typedef).get_members(access=access):
                    if isinstance(member, decls.constructor_t):
                        self.generate_constructor(typedef, member, indent, w)
                    elif isinstance(member, decls.destructor_t):
                        self.generate_destructor(typedef, member, indent, w)
            # finally, close the class
            self.outputFile.write("  " * indent)
            self.outputFile.write("};\n\n\n")

    def generate_constructor(self, typedef, constructor, indent, w):
        # iterate over the arguments
        args = []
        for arg in constructor.arguments:
            s = "%s %s" % (self.get_alias(self.getDeclarationString(arg), w), arg.name)
            # append the default value if it exists
            if arg.default_value:
                s += " = %s" % arg.default_value
            # and add the string to the arg list
            args.append(s)
        self.outputFile.write("  " * indent)
        self.outputFile.write("    %s(%s);\n" % (typedef.name, ", ".join(args)))

    def generate_destructor(self, typedef, destructor, indent, w):
        self.outputFile.write("  " * indent)
        self.outputFile.write("    ~%s();\n" % typedef.name)

    def generate_enum(self, typedef):
        name = typedef.name
        enum = getType(typedef)
        decl_string = typedef.decl_type.decl_string
        # extract the namespace to put it in c++ code. Without that, the code
        # generated by swig
        # is wrong because it doesn't include the namespace
        ns = "::".join(decl_string.split("::")[:-1])
        self.outputFile.write("%{\n")
        self.outputFile.write("using namespace %s;\n" % ns)
        self.outputFile.write("%}\n")
        content = [" %s = %i" % (key, value) for key, value in enum.values]
        self.outputFile.write("enum %s { %s };\n" % (name, ", ".join(content)))

    def generate_nested_enum(self, typedef, enum, indent, w):
        content = [" %s = %i" % (key, value) for key, value in enum.values]
        self.outputFile.write("  " * indent)
        self.outputFile.write("    enum %s { %s };\n" % (enum.name, ", ".join(content)))

    def generate_method(self, typedef, method, indent, w):
        self.info("Generating interface for method  '%s::%s'." %
            (typedef.name, method.name))
        # avoid the apply method for the class vnl_c_vector: the signature is
        # quite strange and currently confuse swig :-/
        if "(" in getType(method.return_type).decl_string:
            self.warn(
                1, "ignoring method not supported by swig '%s::%s'." %
                (typedef.name, method.name), w)
            return

        names = [
            "rBegin",
            "rEnd",
            "GetSpacingCallback",
            "GetOriginCallback",
            "Begin",
            "End"]

        if ((typedef.name.startswith('vnl_') and method.name in ["as_ref"])
                or (typedef.name.startswith('itk') and method.name in names)):
            self.warn(
                3, "ignoring black listed method '%s::%s'." %
                (typedef.name, method.name), w)
            return

        # iterate over the arguments
        args = []
        for arg in method.arguments:
            s = "%s %s" % (self.get_alias(self.getDeclarationString(arg), w), arg.name)
            if "(" in s:
                self.warn(
                    1, "ignoring method not supported by swig '%s::%s'." %
                    (typedef.name, method.name), w)
                return
            # append the default value if it exists
            if arg.default_value:
                s += " = %s" % arg.default_value
            # and add the string to the arg list
            args.append(s)

        # find the method decorators
        static = ""
        const = ""
        if method.has_static:
            static = "static "
        if method.has_const:
            const = " const"
        if method.virtuality != "not virtual":
            static += "virtual "
        if method.virtuality == "pure virtual":
            const += " = 0"

        self.outputFile.write("  " * indent)
        self.outputFile.write(
            "    %s%s %s(%s)%s;\n" %
            (static,
             self.get_alias(
                 self.getDeclarationString(
                     method.return_type),
                 w),
                method.name,
                ", ".join(args),
                const))

        # Check the method arguments for std::string passed by reference.
        # In this case, save the name of the argument in the applyFileNames list
        # for further usage.
        for arg in method.arguments:
            dtype = arg.decl_type
            if pygccxml.declarations.is_reference(dtype) and \
                pygccxml.declarations.is_const(
                    pygccxml.declarations.remove_reference(dtype)) is False and \
                    pygccxml.declarations.is_std_string(dtype):
                    self.applyFileNames.append(arg.name)


    def generate_headerfile(self, idxFile, wrappersNamespace):
        # and begin to write the output
        headerFile = StringIO()
        headerFile.write("// This file is automatically generated.\n")
        headerFile.write("// Do not modify this file manually.\n\n\n")

        langs = [
            "CHICKEN",
            "CSHARP",
            "GUILE",
            "JAVA",
            "LUA",
            "MODULA3",
            "MZSCHEME",
            "OCAML",
            "PERL",
            "PERL5",
            "PHP",
            "PHP4",
            "PHP5",
            "PIKE",
            "PYTHON",
            "R",
            "RUBY",
            "SEXP",
            "TCL",
            "XML"]

        # first, define the module
        # [1:-1] is there to drop the quotes
        for lang in langs:
            headerFile.write("#ifdef SWIG%s\n" % lang)
            headerFile.write("%%module %s%s\n" % (self.moduleName, lang.title()))
            headerFile.write("#endif\n")
        headerFile.write('\n')

        # add the includes
        # use a set to avoid putting many times the same include
        s = set()
        headerFile.write("%{\n")
        # the include files passed in option
        include = self.moduleName + 'SwigInterface.h'
        i = '#include "%s"' % include
        if i not in s:
            headerFile.write(i + '\n')
            s.add(i)
        headerFile.write("%}\n\n\n")

        # load the aliases files
        headerFile.write("%{\n")
        self.load_idx(idxFile)
        # and the idx files in the mdx ones
        for f in self.options.mdx:
            self.load_mdx(f)
        # iterate over all the typedefs in the _wrapping_::wrappers namespace
        # to fill the alias dict
        for typedef in wrappersNamespace.typedefs():  # allow_empty=True):
            s = getType(typedef).decl_string
            # drop the :: prefix - it make swig produce invalid code
            if s.startswith("::"):
                s = s[2:]
            if s not in self.aliases:
                self.warn(
                    2, "%s (%s) should be already defined in the idx files." %
                    (s, typedef.name))
                self.aliases[s] = typedef.name
                # declare the typedef
                headerFile.write("typedef %s %s;\n" % (s, typedef.name))

        headerFile.write("%}\n\n\n")

        return headerFile

    def generate_importfile(self, usedSources):
        # add the imports
        importFile = StringIO()
        for f in self.options.imports:
            importFile.write("%%import %s\n" % f)
        importFile.write("\n\n")

        for src in usedSources:
            importFile.write("%%import %s.i\n" % src)
        importFile.write('\n\n')
        return importFile

    def generate_includefile(self):
        # add the swig includes
        includeFile = StringIO()
        includeFile.write("%include itk.i\n")
        for f in options.swig_includes:
            includeFile.write("%%include %s\n" % f)
        includeFile.write("%%include %s\n" % (self.moduleName + "_ext.i"))
        includeFile.write('\n\n')
        return includeFile

    def generate_applyfile(self):
        # When a std::string is passed by reference, we need to add the %apply
        # line with the argument name, and the INOUT command.
        # Use a set() to remove duplicates, this will work event if we got
        # multiple functions with the same argument name in the same .i file
        # (swig should take care of it).
        applyFileNames = set(self.applyFileNames)
        # Apply file, for passing std::string as reference in methods
        applyFile = StringIO()
        for name in applyFileNames:
            applyFile.write(
                "%apply (std::string& INOUT) { std::string & " + name + "};\n")
        applyFile.write("\n\n")
        return applyFile

    def create_typedefheader(self, usedSources):
        # create the typedef header
        typedefFile = StringIO()
        typedefFile.write("#ifndef __%sSwigInterface_h\n" % self.moduleName)
        typedefFile.write("#define __%sSwigInterface_h\n" % self.moduleName)
        typedefInput = os.path.join(options.library_output_dir,
                                    self.moduleName + 'SwigInterface.h.in')
        with open(typedefInput, "r") as f:
            typedefFile.write(f.read() + '\n')
        for src in usedSources:
            typedefFile.write('#include "%sSwigInterface.h"\n' % src)
        typedefFile.write("#endif\n")
        typedefOutput = os.path.join(options.interface_output_dir,
                                     self.moduleName + 'SwigInterface.h')
        with open(typedefOutput, "w") as f:
            f.write(typedefFile.getvalue())

    def create_interfacefile(self, interfaceFile, idxFile, wrappersNamespace):
        headerFile = self.generate_headerfile(idxFile, wrappersNamespace)

        # iterate over all the typedefs in the _wrapping_::wrappers namespace
        # to build a list of classes with the dependecies
        # classes :: [(name, [dep_name], typedef)]
        classes = []
        for typedef in wrappersNamespace.typedefs():
            # begin a new class
            if isinstance(
                    getType(typedef),
                    pygccxml.declarations.class_declaration.class_t):

                classes.append((
                    typedef.name,
                    [self.get_alias(super_class.related_class.decl_string) for
                        super_class in getType(typedef).bases], typedef))
            elif isinstance(
                    getType(typedef),
                    pygccxml.declarations.enumeration.enumeration_t):
                # warn( 6, "Enum are currently supported only nested in a
                # class." )
                self.generate_enum(typedef)
            else:
                self.warn(
                    5, "Unknown type type: %s" % str(typedef.decl_type.declaration))

        # copy the classes in a new ordered list, according to the dependencies
        # classes is sorted to be sure to always get the same result everywhere
        name_local_classes = [c[0] for c in classes]
        classes = sorted(classes)
        name_already_in_typedefs = []
        typedefs = []
        while len(classes) != 0:
            nclasses = []
            for name, deps, typedef in classes:
                ok = True
                for d in deps:
                    if d in name_local_classes and d not in name_already_in_typedefs:
                        ok = False
                if ok:
                    name_already_in_typedefs.append(name)
                    typedefs.append(typedef)
                else:
                    nclasses.append((name, deps, typedef))
            classes = nclasses

        # now really generate the swig interface
        for typedef in typedefs:
            # begin a new class
            self.generate_class(typedef)

        if len(self.warnings) > 0 and self.options.warningError:
            sys.exit(1)

        # search the files to import
        usedSources = set()
        for alias in self.usedTypes:
            if alias in self.typedefSource:
                idxName = os.path.basename(self.typedefSource[alias])
                iName = idxName[:-len(".idx")]
                usedSources.add(iName)
        outputFileName = os.path.basename(interfaceFile)
        if outputFileName in usedSources:
            usedSources.remove(outputFileName)

        importFile = self.generate_importfile(usedSources)
        includeFile = self.generate_includefile()
        applyFile = self.generate_applyfile()

        self.create_typedefheader(usedSources)

        # finally, really write the output
        content = headerFile.getvalue() + importFile.getvalue() + \
            includeFile.getvalue() + applyFile.getvalue() + self.outputFile.getvalue()

        if self.options.keep and os.path.exists(interfaceFile):
            with open(interfaceFile, "r") as f:
                filecontent = f.read()

        if self.options.keep and os.path.exists(interfaceFile) and \
                filecontent == content:
            self.info("%s unchanged." % interfaceFile)
        else:
            self.info("Writing %s." % interfaceFile)
            with open(interfaceFile, "w") as f:
                f.write(content)


if __name__ == '__main__':
    argParser = ArgumentParser()
    argParser.add_argument(
        "--mdx",
        action="append",
        dest="mdx",
        default=[],
        metavar="FILE",
        help="master idx file to be used.")
    argParser.add_argument(
        "--import",
        action="append",
        dest="imports",
        default=[],
        metavar="FILE",
        help="File to be imported in the generated interface file.")
    argParser.add_argument(
        "--swig-include",
        action="append",
        dest="swig_includes",
        default=[],
        metavar="FILE",
        help=(
            "File to be included by swig (%include) in the generated "
            "interface file."))
    argParser.add_argument(
        "-w",
        "--disable-warning",
        action="append",
        dest="warnings",
        default=[],
        metavar="WARNING",
        help="Warning to be disabled.")
    argParser.add_argument(
        "-A",
        "--disable-access-warning",
        action="append",
        dest="access_warnings",
        default=[],
        metavar="LEVEL",
        help=(
            "Access level where warnings are disabled "
            "(public, protected, private)."))
    argParser.add_argument(
        "-W",
        "--warning-error",
        action="store_true",
        dest="warningError",
        help="Treat warnings as errors.")
    argParser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        dest="verbose",
        help="Log what is currently done.")
    argParser.add_argument(
        "-k",
        "--keep",
        action="store_true",
        dest="keep",
        help="Don't rewrite the output file if the content is unchanged.")
    argParser.add_argument(
        "-p",
        "--pygccxml-path",
        action="store",
        dest="pygccxml_path",
        help="Path to pygccxml")
    argParser.add_argument(
        "-g",
        "--castxml-path",
        action="store",
        dest="castxml_path",
        help="Path to castxml")
    argParser.add_argument(
        "-o",
        "--interface-output-dir",
        action="store",
        dest="interface_output_dir",
        help="Directory to write the Swig input files")
    argParser.add_argument(
        "-l",
        "--library-output-dir",
        action="store",
        dest="library_output_dir",
        help="Directory to read the xml abstract syntax tree input files")
    argParser.add_argument(
        "-s",
        "--submodule-order",
        action="store",
        dest="submodule_order",
        help="List of submodules that must be wrapped in the given order")
    options = argParser.parse_args()

    sys.path.insert(1, options.pygccxml_path)
    import pygccxml
    import logging
    # init the pygccxml stuff
    pygccxml.utils.loggers.cxx_parser.setLevel(logging.CRITICAL)
    pygccxml.declarations.scopedef_t.RECURSIVE_DEFAULT = False
    pygccxml.declarations.scopedef_t.ALLOW_EMPTY_MDECL_WRAPPER = True

    pygccxml_config = pygccxml.parser.config.xml_generator_configuration_t(
        xml_generator_path=options.castxml_path,
        xml_generator="castxml")

    moduleNames = []
    # The first mdx file is the master index file for this module.
    with open(options.mdx[0], 'r') as ff:
        for line in ff.readlines():
            stripped = line.strip()
            if line.startswith('%') or line.isspace():
                # exclude the lines which are starting with % - that's not the idx
                # files
                pass
            elif stripped.endswith(".mdx"):
                pass
            else:
                moduleName = stripped.rsplit('.')[0]
                if moduleName.startswith('(const char*)'):
                    moduleName = moduleName[len('(const char*)'):]
                moduleName = moduleName.strip('"')
                moduleNames.append(moduleName)

    def generate_wrapping_namespace(moduleName):
        xmlFilePath = os.path.join(options.library_output_dir,
                                   moduleName + '.xml')
        pygccxml_reader = pygccxml.parser.source_reader.source_reader_t(
            pygccxml_config)
        abstractSyntaxTree = pygccxml_reader.read_xml_file(xmlFilePath)
        globalNamespace = pygccxml.declarations.get_global_namespace(abstractSyntaxTree)
        wrappingNamespace = globalNamespace.namespace('_wrapping_')
        return wrappingNamespace.namespace('wrappers')

    wrappingNamespaces = dict()
    # Limit the number of cached, parsed abstract syntax trees to avoid very
    # high memory usage
    wrappingCacheLength = min(len(moduleNames), 20)
    for ii in range(wrappingCacheLength):
        moduleName = moduleNames[ii]
        wrappingNamespace = generate_wrapping_namespace(moduleName)
        wrappingNamespaces[moduleName] = wrappingNamespace

    for moduleName in moduleNames:
        if moduleName in wrappingNamespaces:
            wrappersNamespace = wrappingNamespaces[moduleName]
        else:
            wrappersNamespace = generate_wrapping_namespace(moduleName)

        idxFilePath = os.path.join(options.interface_output_dir,
                                   moduleName + '.idx')
        idx_generator = IdxGenerator(moduleName)
        idx_generator.create_idxfile(idxFilePath, wrappersNamespace)

    def generate_swig_input(moduleName):
        if moduleName in wrappingNamespaces:
            wrappersNamespace = wrappingNamespaces[moduleName]
        else:
            wrappersNamespace = generate_wrapping_namespace(moduleName)

        idxFilePath = os.path.join(options.interface_output_dir,
                                   moduleName + '.idx')
        swigInputFilePath = os.path.join(options.interface_output_dir,
                                   moduleName + '.i')

        swig_input_generator = SwigInputGenerator(moduleName, options)
        swig_input_generator.create_interfacefile(swigInputFilePath, idxFilePath,
                wrappersNamespace)

    if options.submodule_order:
        for moduleName in options.submodule_order.split(';'):
            generate_swig_input(moduleName)
            moduleNames.remove(moduleName)
    for moduleName in moduleNames:
        generate_swig_input(moduleName)
