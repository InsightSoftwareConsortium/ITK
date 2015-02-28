#!/usr/bin/env python
# -*- coding: utf-8 -*-

# Import unicode literals so that StringIO works on both Python 2 and 3
from __future__ import unicode_literals
from __future__ import print_function

import sys
import os
import re
import time
from optparse import OptionParser

try:
    # Python 3
    from io import StringIO
except ImportError:
    # Python 2
    from cStringIO import StringIO

# Start process time measurement
t0 = time.clock()

optionParser = OptionParser()
optionParser.add_option(
    "--idx",
    action="append",
    dest="idx",
    default=[],
    metavar="FILE",
    help="idx file to be used.")
optionParser.add_option(
    "--mdx",
    action="append",
    dest="mdx",
    default=[],
    metavar="FILE",
    help="master idx file to be used.")
optionParser.add_option(
    "--include",
    action="append",
    dest="includes",
    default=[],
    metavar="FILE",
    help="File to be included in the generated interface file.")
optionParser.add_option(
    "--take-includes",
    action="append",
    dest="take_includes",
    default=[],
    metavar="FILE",
    help=(
        "File which contains the include to take, and include in the "
        "generated interface file."))
optionParser.add_option(
    "--swig-include",
    action="append",
    dest="swig_includes",
    default=[],
    metavar="FILE",
    help=(
        "File to be included by swig (%include) in the generated "
        "interface file."))
optionParser.add_option(
    "--import",
    action="append",
    dest="imports",
    default=[],
    metavar="FILE",
    help="File to be imported in the generated interface file.")
optionParser.add_option(
    "--typedef-input",
    action="store",
    type="string",
    dest="typedef_input")
optionParser.add_option(
    "--typedef-output",
    action="store",
    type="string",
    dest="typedef_output")
optionParser.add_option(
    "-w",
    "--disable-warning",
    action="append",
    dest="igenerator_warnings",
    default=[],
    metavar="WARNING",
    help="Warning to be disabled.")
optionParser.add_option(
    "-A",
    "--disable-access-warning",
    action="append",
    dest="access_warnings",
    default=[],
    metavar="LEVEL",
    help=(
        "Access level where warnings are disabled "
        "(public, protected, private)."))
optionParser.add_option(
    "-W",
    "--warning-error",
    action="store_true",
    dest="warningError",
    help="Treat warnings as errors.")
optionParser.add_option(
    "-v",
    "--verbose",
    action="store_true",
    dest="verbose",
    help="Log what is currently done.")
optionParser.add_option(
    "-k",
    "--keep",
    action="store_true",
    dest="keep",
    help="Don't rewrite the output file if the content is unchanged.")
optionParser.add_option(
    "-p",
    "--pygccxml-path",
    action="store",
    dest="pygccxml_path",
    help="Path to pygccxml")
optionParser.add_option(
    "-g",
    "--gccxml-path",
    action="store",
    dest="gccxml_path",
    help="Path to gccxml")
options, args = optionParser.parse_args()

sys.path.append(options.pygccxml_path)
import pygccxml
import logging

pygccxml.utils.loggers.cxx_parser.setLevel(logging.CRITICAL)

# Apply file, for passing std::string as reference in methods
applyFile = StringIO()
applyFileNames = []
# the output file
outputFile = StringIO()

# a dict to let us use the alias name instead of the full c++ name. Without
# that, in many cases, swig don't know that's the same type
aliases = {}


# a set of used types
usedTypes = set()

# a dict to store the file where the def comes from
typedefSource = {}


igenerator_warnings = set()


def igenerator_warn(id, msg, doWarn=True):
    if not doWarn:
        # don't warn for anything
        return
    if str(id) not in options.igenerator_warnings:
        if not options.verbose and (id, msg) in igenerator_warnings:
            # just do nothing
            return
        igenerator_warnings.add((id, msg))
        if options.verbose:
            if options.warningError:
                print("error(%s): %s" % (str(id), msg), file=sys.stderr)
            else:
                print("warning(%s): %s" % (str(id), msg), file=sys.stderr)
        else:
            if options.warningError:
                print(
                    "%s: error(%s): %s" %
                    (args[0], str(id), msg), file=sys.stderr)
            else:
                print(
                    "%s: warning(%s): %s" %
                    (args[0], str(id), msg), file=sys.stderr)


def info(msg):
    if options.verbose:
        print("info: %s" % msg, file=sys.stderr)

notWrapped = [
    "itk::MapContainer< unsigned long, itk::CellInterface<.+>",
    "itk::VectorContainer< unsigned long, itk::CellInterface<.+>",
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
]

notWrappedRegExp = re.compile("|".join(["^" + s + "$" for s in notWrapped]))

# vcl_complex code

vcl_complex_headers = {
    "D": """ class vcl_complexD {
   public:
     ~vcl_complexD();
     vcl_complexD & operator=(vcl_complexD const & arg0);
     vcl_complexD(vcl_complexD const & arg0);
     vcl_complexD(vcl_complexD __z);
     vcl_complexD(double __r = 0.0, double __i = 0.0);
     vcl_complexD(vcl_complexF const & __z);
     vcl_complexD(vcl_complexLD const & __z);
     double real();
     double const real() const;
     double imag();
     double const imag() const;
     vcl_complexD & operator=(double __d);
     vcl_complexD & operator+=(double __d);
     vcl_complexD & operator-=(double __d);
     vcl_complexD & operator*=(double __d);
     vcl_complexD & operator/=(double __d);
     // vcl_complexD const & __rep() const;
   private:
   protected:
 };
""",

    "F": """class vcl_complexF {
   public:
     ~vcl_complexF();
     vcl_complexF & operator=(vcl_complexF const & arg0);
     vcl_complexF(vcl_complexF const & arg0);
     vcl_complexF(vcl_complexF __z);
     vcl_complexF(float r = 0.0f, float i = 0.0f);
     vcl_complexF(vcl_complexD const & __z);
     vcl_complexF(vcl_complexLD const & __z);
     float real();
     float const real() const;
     float imag();
     float const imag() const;
     vcl_complexF & operator=(float __f);
     vcl_complexF & operator+=(float __f);
     vcl_complexF & operator-=(float __f);
     vcl_complexF & operator*=(float __f);
     vcl_complexF & operator/=(float __f);
     // vcl_complexF const & __rep() const;
   private:
   protected:
 };
""",

    "LD": """ class vcl_complexLD {
   public:
     ~vcl_complexLD();
     vcl_complexLD & operator=(vcl_complexLD const & arg0);
     vcl_complexLD(vcl_complexLD const & arg0);
     vcl_complexLD(vcl_complexLD __z);
     vcl_complexLD(long double __r = 0.0l, long double __i = 0.0l);
     vcl_complexLD(vcl_complexF const & __z);
     vcl_complexLD(vcl_complexD const & __z);
     long double real();
     long double const real() const;
     long double imag();
     long double const imag() const;
     vcl_complexLD & operator=(long double __r);
     vcl_complexLD & operator+=(long double __r);
     vcl_complexLD & operator-=(long double __r);
     vcl_complexLD & operator*=(long double __r);
     vcl_complexLD & operator/=(long double __r);
     // vcl_complexLD const & __rep() const;
   private:
   protected:
 };
"""}


def getType(v):
    if hasattr(v, "type"):
        return getType(v.type)
    if hasattr(v, "declaration"):
        return getType(v.declaration)
    return v


def getDeclarationString(t):
    t = getType(t)
    if t.decl_string == "::PyObject *":
        # don't go further - we want to keep that one as is
        return "::PyObject *"
    if isinstance(t, pygccxml.declarations.cpptypes.pointer_t):
        return getDeclarationString(getType(t.base)) + " *"
    elif isinstance(t, pygccxml.declarations.cpptypes.const_t):
        return getDeclarationString(getType(t.base)) + " const"
    elif isinstance(t, pygccxml.declarations.cpptypes.reference_t):
        return getDeclarationString(getType(t.base)) + " &"
    return t.decl_string


def renameTypesInSTL(s):
    if s.startswith("std::") and \
            pygccxml.declarations.templates.is_instantiation(s):
        args = []
        for arg in pygccxml.declarations.templates.args(s):
            t, d = typeAndDecorators(arg)
            args.append(renameTypesInSTL(get_alias(t)) + d)
        return pygccxml.declarations.templates.join(
            pygccxml.declarations.templates.name(s),
            args) + typeAndDecorators(s)[1]
    return s


def removeStdAllocator(s):
    if pygccxml.declarations.templates.is_instantiation(s):
        args = []
        for arg in pygccxml.declarations.templates.args(s):
            if not arg.startswith("std::allocator"):
                t, d = typeAndDecorators(arg)
                args.append(removeStdAllocator(t) + d)
        return pygccxml.declarations.templates.join(
            pygccxml.declarations.templates.name(s),
            args) + typeAndDecorators(s)[1]
    return s


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


def fix_std_namespace(s):
    # everything from stl in std::__1, with libstdc++?
    return s.replace("std::__1::", "std::")


def get_alias(decl_string, w=True):
    s = str(decl_string)

    # drop the :: prefix - it make swig produce invalid code
    if s.startswith("::"):
        s = s[2:]

    # normalize string
    s = normalize(s)

    # workaround a bug - or is it a feature ? - somewhere
    s = s.replace("complex float", "std::complex<float>")
    s = s.replace("complex double", "std::complex<double>")
    s = s.replace("complex long double", "std::complex<long double>")

    (s, end) = typeAndDecorators(s)

    if s in aliases:
        #    print(s, end, "        ", aliases[s], file=sys.stderr)
        usedTypes.add(aliases[s])
        return aliases[s] + end

    if s.startswith("itk::Templates::"):
        # that's a explicitly instantiated type. The name is the same than
        # the WraITK one, so lets use it as a base for WrapITK
        # Ex: itk::Templates::RGBPixelUC
        # don't store the new string in s, because we need it unchanged if
        # the type is explicitly instantiated, but not wrapped
        new_s = s.replace("::Templates::", "")
        if new_s.split("::")[0] in aliases.values():
            usedTypes.add(new_s)
            return new_s + end

    if s[:s.rfind("::")] in aliases:
        # take care of subtypes/enum/...
        alias = aliases[s[:s.rfind("::")]] + s[s.rfind("::"):]
        usedTypes.add(alias)
        return alias + end

    s = fix_std_namespace(s)

    # replace the types defined in this type, to support
    # std::vector<itkDataObject> for example
    s = renameTypesInSTL(s)

    # drop the allocator part of the type, because it is not supported by the
    # %template directive with some generators (like tcl)
    s = removeStdAllocator(s)

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

    if s.startswith("itk::") and not notWrappedRegExp.match(s):
        igenerator_warn(
            4,
            "ITK type not wrapped, or currently not known: %s" %
            s,
            w)

    usedTypes.add(s)
    return s + end


def load_idx(file_name):
    with open(file_name, "r") as f:
        for l in f:
            (full_name, alias, module) = \
                re.findall(r'{(.*)} {(.*)} {(.*)}', l)[0]
            # workaround lack of :: prefix in idx files
            # TODO: would it be better to remove the :: prefix in the output of
            # pygccxml ?
            # full_name = "::"+full_name
            # normalize some basic type names
            full_name = normalize(full_name)
            # TODO: add a warning if the type is defined several times
            aliases[full_name] = alias
            # store the source of the def
            if alias in typedefSource and file_name != typedefSource[alias]:
                igenerator_warn(
                    7, "%s in %s is already defined in %s." %
                    (alias, file_name, typedefSource[alias]))
            else:
                typedefSource[alias] = file_name
            # don't declare the typedef - they are included from .include files
            # outputFile.write("typedef %s %s;\n" % (full_name, alias))

mdx_loaded = set()


def load_mdx(file_name):
    if file_name in mdx_loaded:
        # already loaded - no need to do it again
        return
    mdx_loaded.add(file_name)
    with open(file_name, "r") as f:
        ls = f.readlines()
    for l in ls:
        if l.startswith('%') or l.isspace():
            # exclude the lines which are starting with % - that's not the idx
            # files
            pass
        elif l.strip().endswith(".mdx"):
            load_mdx(os.path.dirname(file_name) + os.sep + l.strip())
        else:
            load_idx(os.path.dirname(file_name) + os.sep + l.strip())


def normalize(name):
    name = name.replace("short unsigned int", "unsigned short")
    name = name.replace("long unsigned int", "unsigned long")
    name = name.replace("short int", "short")
    name = name.replace("long int", "long")
#  name = name.replace("unsigned int", "unsigned")
    # normalize spaces
    name = " ".join(name.replace(',', ', ').split())
    return name


def generate_class(typedef, indent=0):
    info("Generating interface for %s." % typedef.name)

    decls = pygccxml.declarations

    if not typedef.name.startswith("vcl_complex"):
        super_classes = []
        for super_class in getType(typedef).bases:
            super_classes.append(
                "%s %s" %
                (super_class.access,
                 get_alias(
                     super_class.related_class.decl_string)))
        s = ""
        if super_classes:
            s = " : " + ", ".join(super_classes)
        outputFile.write("  " * indent)
        outputFile.write("class %s%s {\n" % (typedef.name, s))

        # iterate over access
        for access in decls.ACCESS_TYPES.ALL:

            # the access type
            outputFile.write("  " * indent)
            outputFile.write("  %s:\n" % access)

            # warnings or no warning?
            w = access not in options.access_warnings

            # iterate over the members
            for member in getType(typedef).get_members(access=access):
                if isinstance(member, decls.typedef.typedef_t):
                    igenerator_warn(
                        51,
                        "Member typedef are not supported: %s" %
                        member.name,
                        w)
                elif isinstance(member, decls.calldef.member_function_t):
                    generate_method(typedef, member, indent, w)
                elif isinstance(member, decls.calldef.constructor_t):
                    generate_constructor(typedef, member, indent, w)
                elif isinstance(member, decls.calldef.member_operator_t):
                    generate_method(typedef, member, indent, w)
                elif isinstance(member, decls.calldef.destructor_t):
                    generate_destructor(typedef, member, indent, w)
                elif isinstance(member, decls.enumeration.enumeration_t):
                    generate_nested_enum(typedef, member, indent, w)
                elif isinstance(member, decls.variable.variable_t):
                    igenerator_warn(
                        52,
                        "Member variables are not supported: %s" %
                        member.name,
                        w)
                elif isinstance(member, decls.class_declaration.class_t):
                    igenerator_warn(
                        53,
                        "Member classes are not supported: %s" %
                        member.name,
                        w)
                elif isinstance(
                        member, decls.class_declaration.class_declaration_t):
                    igenerator_warn(
                        53,
                        "Member classes are not supported: %s" %
                        member.name,
                        w)
                elif isinstance(member, decls.calldef.casting_operator_t):
                    igenerator_warn(
                        54,
                        "Member casting operators are not supported: %s" %
                        member.name,
                        w)
                else:
                    igenerator_warn(
                        50,
                        "Unknown member type: %s" %
                        repr(member),
                        w)

        # finally, close the class
        outputFile.write("  " * indent)
        outputFile.write("};\n\n\n")

    elif typedef.name == "vcl_complexD":
        outputFile.write(vcl_complex_headers["D"] + '\n')
    elif typedef.name == "vcl_complexF":
        outputFile.write(vcl_complex_headers["F"] + '\n')
    elif typedef.name == "vcl_complexLD":
        outputFile.write(vcl_complex_headers["LD"] + '\n')
    else:
        # vcl_complex is too difficult to wrap in some cases. Only wrap the
        # constructor.
        outputFile.write("  " * indent)
        outputFile.write("class %s%s {\n" % (typedef.name, s))

        # iterate over access
        for access in pygccxml.declarations.ACCESS_TYPES.ALL:

            # the access type
            outputFile.write("  " * indent)
            outputFile.write("  %s:\n" % access)

            # warnings or no warning?
            w = access not in options.access_warnings
            for member in getType(typedef).get_members(access=access):
                if isinstance(member, decls.calldef.constructor_t):
                    generate_constructor(typedef, member, indent, w)
                elif isinstance(member, decls.calldef.destructor_t):
                    generate_destructor(typedef, member, indent, w)
        # finally, close the class
        outputFile.write("  " * indent)
        outputFile.write("};\n\n\n")


def generate_constructor(typedef, constructor, indent, w):
    # iterate over the arguments
    args = []
    for arg in constructor.arguments:
        s = "%s %s" % (get_alias(getDeclarationString(arg), w), arg.name)
        # append the default value if it exists
        if arg.default_value:
            s += " = %s" % arg.default_value
        # and add the string to the arg list
        args.append(s)
    outputFile.write("  " * indent)
    outputFile.write("    %s(%s);\n" % (typedef.name, ", ".join(args)))


def generate_destructor(typedef, destructor, indent, w):
    outputFile.write("  " * indent)
    outputFile.write("    ~%s();\n" % typedef.name)


def generate_enum(typedef):
    name = typedef.name
    enum = getType(typedef)
    decl_string = typedef.type.decl_string
    # extract the namespace to put it in c++ code. Without that, the code
    # generated by swig
    # is wrong because it doesn't include the namespace
    ns = "::".join(decl_string.split("::")[:-1])
    outputFile.write("%{\n")
    outputFile.write("using namespace %s;\n" % ns)
    outputFile.write("%}\n")
    content = [" %s = %i" % (key, value) for key, value in enum.values]
    outputFile.write("enum %s { %s };\n" % (name, ", ".join(content)))


def generate_nested_enum(typedef, enum, indent, w):
    content = [" %s = %i" % (key, value) for key, value in enum.values]
    outputFile.write("  " * indent)
    outputFile.write("    enum %s { %s };\n" % (enum.name, ", ".join(content)))


def generate_method(typedef, method, indent, w):
    info(
        "Generating interface for method  '%s::%s'." %
        (typedef.name, method.name))
    # avoid the apply method for the class vnl_c_vector: the signature is
    # quite strange and currently confuse swig :-/
    if "(" in getType(method.return_type).decl_string:
        igenerator_warn(
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
        igenerator_warn(
            3, "ignoring black listed method '%s::%s'." %
            (typedef.name, method.name), w)
        return

    # iterate over the arguments
    args = []
    for arg in method.arguments:
        s = "%s %s" % (get_alias(getDeclarationString(arg), w), arg.name)
        if "(" in s:
            igenerator_warn(
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

    outputFile.write("  " * indent)
    outputFile.write(
        "    %s%s %s(%s)%s;\n" %
        (static,
         get_alias(
             getDeclarationString(
                 method.return_type),
             w),
            method.name,
            ", ".join(args),
            const))

    # Check the method arguments for std::string passed by reference.
    # In this case, save the name of the argument in the applyFileNames list
    # for further usage.
    for arg in method.arguments:
        # arg.type is an instance, there is no clean API in pygccxml to get the
        # name of the type as a string. This functionnality needs to be added
        # to pygccxml. Meanwhile, we can use the __str__() method.
        if fix_std_namespace(arg.type.__str__()) == "std::string &":
            applyFileNames.append(arg.name)

# init the pygccxml stuff
pygccxml.declarations.scopedef_t.RECURSIVE_DEFAULT = False
pygccxml.declarations.scopedef_t.ALLOW_EMPTY_MDECL_WRAPPER = True
pygccxml_config = pygccxml.parser.config.gccxml_configuration_t(
    gccxml_path=options.gccxml_path)
# create a reader
pygccxml_reader = pygccxml.parser.source_reader.source_reader_t(
    pygccxml_config)
# and read a xml file
info("Processing %s." % args[0])
res = pygccxml_reader.read_xml_file(args[0])

global_ns = pygccxml.declarations.get_global_namespace(res)
cable_ns = global_ns.namespace('_cable_')
wrappers_ns = cable_ns.namespace('wrappers')

moduleName = cable_ns.variable('group').value
if moduleName.startswith('(const char*)'):
  moduleName = moduleName[len('(const char*)'):]
moduleName = moduleName.strip('"')

# and begin to write the output
headerFile = StringIO()
info("Generating %s header." % args[1])
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
    headerFile.write("%%module %s%s\n" % (moduleName, lang.title()))
    headerFile.write("#endif\n")
headerFile.write('\n')

# add the includes
# use a set to avoid putting many times the same include
s = set()
headerFile.write("%{\n")
# the include files passed in option
for f in options.includes:
    i = '#include "%s"' % f
    if i not in s:
        headerFile.write(i + '\n')
        s.add(i)
# and the includes files from other files
for file_name in options.take_includes:
    with open(file_name, "r") as f:
        for l in f:
            if l.startswith('#include'):
                i = " ".join(l.strip().split())
                if i not in s:
                    headerFile.write(i + '\n')
                    s.add(i)
headerFile.write("%}\n\n\n")

# load the aliases files
headerFile.write("%{\n")
# the idx files passed in option
for f in options.idx:
    load_idx(f)
# and the idx files in the mdx ones
for f in options.mdx:
    load_mdx(f)
# iterate over all the typedefs in the _cable_::wrappers namespace
# to fill the alias dict
for typedef in wrappers_ns.typedefs():  # allow_empty=True):
    s = getType(typedef).decl_string
    # drop the :: prefix - it make swig produce invalid code
    if s.startswith("::"):
        s = s[2:]
    if s not in aliases:
        igenerator_warn(
            2, "%s (%s) should be already defined in the idx files." %
            (s, typedef.name))
        aliases[s] = typedef.name
        # declare the typedef
        headerFile.write("typedef %s %s;\n" % (s, typedef.name))

headerFile.write("%}\n\n\n")

# add the imports
importFile = StringIO()
for f in options.imports:
    importFile.write("%%import %s\n" % f)
importFile.write("\n\n")

# add the swig includes
includeFile = StringIO()
for f in options.swig_includes:
    includeFile.write("%%include %s\n" % f)
includeFile.write('\n\n')


# iterate over all the typedefs in the _cable_::wrappers namespace
# to build a list of classes with the dependecies
# classes :: [(name, [dep_name], typedef)]
classes = []
for typedef in wrappers_ns.typedefs():
    # begin a new class
    if isinstance(
            getType(typedef),
            pygccxml.declarations.class_declaration.class_t):

        classes.append((
            typedef.name,
            [get_alias(super_class.related_class.decl_string) for
                super_class in getType(typedef).bases], typedef))

    elif isinstance(
            getType(typedef),
            pygccxml.declarations.enumeration.enumeration_t):
        # igenerator_warn( 6, "Enum are currently supported only nested in a
        # class." )
        generate_enum(typedef)

    else:
        igenerator_warn(
            5, "Unknown type type: %s" % str(typedef.type.declaration))


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
    generate_class(typedef)


if len(igenerator_warnings) > 0 and options.warningError:
    sys.exit(1)


# search the files to import
usedSources = set()
for alias in usedTypes:
    if alias in typedefSource:
        idxName = os.path.basename(typedefSource[alias])
        iName = idxName[:-len(".idx")]
        usedSources.add(iName)
outputFileName = os.path.basename(args[1])
if outputFileName in usedSources:
    usedSources.remove(outputFileName)
for src in usedSources:
    importFile.write("%%import %s.i\n" % src)
importFile.write('\n\n')


# create the typedef header
if options.typedef_output:
    typedefFile = StringIO()
    typedefFile.write("#ifndef __%sSwigInterface_h\n" % moduleName)
    typedefFile.write("#define __%sSwigInterface_h\n" % moduleName)
    if options.typedef_input:
        with open(options.typedef_input, "r") as f:
            typedefFile.write(f.read() + '\n')
    for src in usedSources:
        typedefFile.write('#include "%sSwigInterface.h"\n' % src)
    typedefFile.write("#endif\n")
    with open(options.typedef_output, "w") as f:
        f.write(typedefFile.getvalue())

# When a std::string is passed by reference, we need to add the %apply
# line with the argument name, and the INOUT command.
# Use a set() to have remove duplicates, this will work event if we got
# multiple functions with the same argument name in the same .i file
# (swig should take care of it).
applyFileNames = set(applyFileNames)
for name in applyFileNames:
    applyFile.write(
        "%apply (std::string& INOUT) { std::string & " + name + "};\n")
applyFile.write("\n\n")

# finally, really write the output
content = headerFile.getvalue() + importFile.getvalue() + \
    includeFile.getvalue() + applyFile.getvalue() + outputFile.getvalue()

if args[1] != '-':

    if options.keep and os.path.exists(args[1]):
        with open(args[1], "r") as f:
            filecontent = f.read()

    if options.keep and os.path.exists(args[1]) and \
            filecontent == content:
        info("%s unchanged." % args[1])
    else:
        info("Writing %s." % args[1])
        with open(args[1], "w") as f:
            f.write(content)
else:
    sys.stdout.write(content)

# Stop process time measurement
info("%s seconds i-generation time." % (time.clock() - t0))
