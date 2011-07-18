#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys, os
sys.path.append(sys.path[0]+os.sep+'pygccxml-1.0.0')

import pygccxml, sys, re, cStringIO, time, os
from optparse import OptionParser

# Start process time measurement
t0 = time.clock()

optionParser = OptionParser()
optionParser.add_option("--idx", action="append", dest="idx", default=[], metavar="FILE", help="idx file to be used.")
optionParser.add_option("--mdx", action="append", dest="mdx", default=[], metavar="FILE", help="master idx file to be used.")
optionParser.add_option("--include", action="append", dest="includes", default=[], metavar="FILE", help="File to be included in the generated interface file.")
optionParser.add_option("--take-includes", action="append", dest="take_includes", default=[], metavar="FILE", help="File which contains the include to take, and include in the generated interface file.")
optionParser.add_option("--swig-include", action="append", dest="swig_includes", default=[], metavar="FILE", help="File to be included by swig (%include) in the generated interface file.")
optionParser.add_option("--import", action="append", dest="imports", default=[], metavar="FILE", help="File to be imported in the generated interface file.")
optionParser.add_option("--typedef-input", action="store", type="string", dest="typedef_input")
optionParser.add_option("--typedef-output", action="store", type="string", dest="typedef_output")
optionParser.add_option("-w", "--disable-warning", action="append", dest="warnings", default=[], metavar="WARNING", help="Warning to be disabled.")
optionParser.add_option("-A", "--disable-access-warning", action="append", dest="access_warnings", default=[], metavar="LEVEL", help="Access level where warnings are disabled (public, protected, private).")
optionParser.add_option("-W", "--warning-error", action="store_true", dest="warningError", help="Treat warnings as errors.")
optionParser.add_option("-v", "--verbose", action="store_true", dest="verbose", help="Log what is currently done.")
optionParser.add_option("-k", "--keep", action="store_true", dest="keep", help="Don't rewrite the output file if the content is unchanged.")
options, args = optionParser.parse_args()

# the output file
outputFile = cStringIO.StringIO()

# a dict to let us use the alias name instead of the full c++ name. Without
# that, in many cases, swig don't know that's the same type
aliases = {}


# a set of used types
usedTypes = set()

# a dict to store the file where the def comes from
typedefSource = {}


warnings = set()

def warn( id, msg, doWarn=True ):
  if not doWarn:
    # don't warn for anything
    return
  if str(id) not in options.warnings:
    if not options.verbose and (id, msg) in warnings:
      # just do nothing
      return
    warnings.add((id, msg))
    if options.verbose:
      if options.warningError:
        print >> sys.stderr, "error(%s): %s" % (str(id), msg)
      else:
        print >> sys.stderr, "warning(%s): %s" % (str(id), msg)
    else:
      if options.warningError:
        print >> sys.stderr, "%s: error(%s): %s" % (args[0], str(id), msg)
      else:
        print >> sys.stderr, "%s: warning(%s): %s" % (args[0], str(id), msg)

def info( msg ):
  if options.verbose:
      print >> sys.stderr, "info: %s" % msg

notWrapped = [
  "itk::SmartPointerForwardReference<.+>",
  "itk::LibHandle",
  "itk::NeighborhoodAllocator<.+>",
  "itk::ImageRegion<.+>", # to avoid wrapping all the region for all the dims
  "itk::ImportImageContainer<.+>",
  "itk::DefaultPixelAccessor<.+>",
  "itk::NeighborhoodAccessorFunctor<.+>",
  "itk::DefaultVectorPixelAccessor<.+>",
  "itk::VectorImageNeighborhoodAccessorFunctor<.+>",
  "itk::.*Iterator.*",  # TODO: remove this one ?
  "itk::Neighborhood<.+>", # TODO: remove this one
  "itk::ThreadFunctionType",
  "itk::Functor::.+",
  "itk::SmartPointer< itk::Functor::.+",
  "itk::Function::.+",
  "itk::.+Function.*",  # Level set functions
  "itk::InterpolateImageFunction<.+>",  # use one more dimension than the wrapped one
  "itk::watershed::.+",  # ignore the internal classes of the watershed
  "itk::SmartPointer< itk::VoronoiDiagram2D<.+> >",  # require to wrap too more type
  "itk::Image< itk::CovariantVector< double, \d+u >, \d+u >",  # used internally in ImageToImageMetric
]

notWrappedRegExp = re.compile( "|".join( [ "^"+s+"$" for s in notWrapped] ) )

# vcl_complex code

vcl_complex_headers = {"D": """ class vcl_complexD {
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

"LD" : """ class vcl_complexLD {
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
"""
}

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

def renameTypesInSTL( s ):
  if s.startswith( "std::" ) and pygccxml.declarations.templates.is_instantiation(s):
    args = []
    for arg in pygccxml.declarations.templates.args(s):
      t, d = typeAndDecorators( arg );
      args.append( renameTypesInSTL( get_alias( t ) ) + d )
    return pygccxml.declarations.templates.join( pygccxml.declarations.templates.name(s), args ) + typeAndDecorators( s )[1]
  return s


def removeStdAllocator( s ):
  if pygccxml.declarations.templates.is_instantiation(s):
    args = []
    for arg in pygccxml.declarations.templates.args(s):
      if not arg.startswith("std::allocator"):
        t, d = typeAndDecorators( arg );
        args.append( removeStdAllocator( t ) + d )
    return pygccxml.declarations.templates.join( pygccxml.declarations.templates.name(s), args ) + typeAndDecorators( s )[1]
  return s


def typeAndDecorators( s ):
  end = ""
  s = s.strip()
  ends = [" ", "*", "&", "const"]
  needToContinue = True
  while needToContinue:
    needToContinue = False
    for e in ends:
      if s.endswith( e ):
        end = e + end
        s = s[:-len(e)]
        needToContinue = True
  return (s, end)


def get_alias( decl_string, w=True ):
  s = str(decl_string)

  # drop the :: prefix - it make swig produce invalid code
  if s.startswith("::"):
    s = s[2:]

  # normalize string
  s = normalize( s )

  # workaround a bug - or is it a feature ? - somewhere
  s = s.replace("complex float", "std::complex<float>")
  s = s.replace("complex double", "std::complex<double>")
  s = s.replace("complex long double", "std::complex<long double>")

  (s, end) = typeAndDecorators( s )

  if aliases.has_key( s ):
#    print >> sys.stderr, s, end, "        ", aliases[s]
    usedTypes.add( aliases[s] )
    return aliases[s] + end

  if s.startswith("itk::Templates::"):
    # that's a explicitly instantiated type. The name is the same than the WraITK
    # one, so lets use it as a base for WrapITK
    # Ex: itk::Templates::RGBPixelUC
    # don't store the new string in s, because we need it unchanged if the type is
    # explicitly instantiated, but not wrapped
    new_s = s.replace("::Templates::", "")
    if new_s.split("::")[0] in aliases.values():
      usedTypes.add( new_s )
      return new_s + end

  if s[:s.rfind("::")] in aliases:
    # take care of subtypes/enum/...
    alias = aliases[ s[:s.rfind("::")] ] + s[s.rfind("::"):]
    usedTypes.add( alias )
    return alias + end

  # replace the types defined in this type, to support std::vector<itkDataObject> for example
  s = renameTypesInSTL( s )

  # drop the allocator part of the type, because it is not supported by the %template directive with some languages (like tcl)
  s = removeStdAllocator( s )

  # rename basic_string to std::string to make name shorter
  s = s.replace("std::basic_string< char, std::char_traits< char > >", "std::string")

  # rename some types not renamed by gccxml (why ?)
  s = s.replace( "itk::SerieUIDContainer", "std::vector< std::string >")
  s = s.replace( "itk::FilenamesContainer", "std::vector< std::string >")

  if s.startswith( "itk::") and not notWrappedRegExp.match( s ):
    warn( 4, "ITK type not wrapped, or currently not known: %s" % s, w )

  usedTypes.add( s )
  return s + end


def load_idx(file_name):
  f = file(file_name)
  for l in f:
    (full_name, alias, module) = re.findall(r'{(.*)} {(.*)} {(.*)}', l)[0]
    # workaround lack of :: prefix in idx files
    # TODO: would it be better to remove the :: prefix in the output of pygccxml ?
    # full_name = "::"+full_name
    # normalize some basic type names
    full_name = normalize( full_name )
    # TODO: add a warning if the type is defined several times
    aliases[ full_name ] = alias
    # store the source of the def
    if typedefSource.has_key( alias ) and file_name != typedefSource[ alias ]:
      warn( 7, "%s in %s is already defined in %s." % (alias, file_name, typedefSource[ alias ]) )
    else:
      typedefSource[ alias ] = file_name
    # don't declare the typedef - they are included from .include files
    # print >> outputFile, "typedef %s %s;" % (full_name, alias)
  f.close()

mdx_loaded = set()
def load_mdx(file_name):
  if file_name in mdx_loaded:
    # already loaded - no need to do it again
    return
  mdx_loaded.add( file_name )
  f = file( file_name )
  ls = f.readlines()
  f.close()
  for l in ls :
    if l.startswith( '%' ) or l.isspace():
      # exclude the lines which are starting with % - that's not the idx files
      pass
    elif l.strip().endswith(".mdx"):
      load_mdx(os.path.dirname(file_name)+os.sep+l.strip())
    else:
      load_idx(os.path.dirname(file_name)+os.sep+l.strip())


def normalize(name):
  name = name.replace("short unsigned int", "unsigned short")
  name = name.replace("long unsigned int", "unsigned long")
  name = name.replace("short int", "short")
  name = name.replace("long int", "long")
#  name = name.replace("unsigned int", "unsigned")
  # normalize spaces
  name = " ".join(name.replace(',', ', ').split())
  return name


def generate_class( typedef, indent=0 ):
  info("Generating interface for %s." % typedef.name)

  if not typedef.name.startswith("vcl_complex"):
    super_classes = []
    for super_class in typedef.type.declaration.bases:
      super_classes.append( "%s %s" % ( super_class.access, get_alias(super_class.related_class.decl_string) ) )
    s = ""
    if super_classes:
      s = " : " + ", ".join( super_classes )
    print >> outputFile, "  "*indent, "class %s%s {" % ( typedef.name, s )

    # iterate over access
    for access in pygccxml.declarations.ACCESS_TYPES.ALL:

      # print the access type
      print >> outputFile, "  "*indent, "  %s:" % access

      # warnings or no warning?
      w = access not in options.access_warnings

      # iterate over the members
      for member in typedef.type.declaration.get_members( access=access ):
        if isinstance( member, pygccxml.declarations.typedef.typedef_t ):
          warn( 51, "Member typedef are not supported: %s" % member.name, w )
        elif isinstance( member, pygccxml.declarations.calldef.member_function_t ):
          generate_method( typedef, member, indent, w )
        elif isinstance( member, pygccxml.declarations.calldef.constructor_t ):
          generate_constructor( typedef, member, indent, w )
        elif isinstance( member, pygccxml.declarations.calldef.member_operator_t ):
          generate_method( typedef, member, indent, w )
        elif isinstance( member, pygccxml.declarations.calldef.destructor_t ):
          generate_destructor( typedef, member, indent, w )
        elif isinstance( member, pygccxml.declarations.enumeration.enumeration_t ):
          generate_nested_enum( typedef, member, indent, w )
        elif isinstance( member, pygccxml.declarations.variable.variable_t ):
          warn( 52, "Member variables are not supported: %s" % member.name, w )
        elif isinstance( member, pygccxml.declarations.class_declaration.class_t ):
          warn( 53, "Member classes are not supported: %s" % member.name, w )
        elif isinstance( member, pygccxml.declarations.class_declaration.class_declaration_t ):
          warn( 53, "Member classes are not supported: %s" % member.name, w )
        elif isinstance( member, pygccxml.declarations.calldef.casting_operator_t ):
          warn( 54, "Member casting operators are not supported: %s" % member.name, w )
        else :
          warn( 50, "Unknown member type: %s" % repr(member), w )

    # finally, close the class
    print >> outputFile, "  "*indent, "};"
    print >> outputFile
    print >> outputFile

  elif typedef.name == "vcl_complexD":
    print >> outputFile, vcl_complex_headers["D"]
  elif typedef.name == "vcl_complexF":
    print >> outputFile, vcl_complex_headers["F"]
  elif typedef.name == "vcl_complexLD":
    print >> outputFile, vcl_complex_headers["LD"]
  else:
    # vcl_complex is too difficult to wrap in some cases. Only wrap the constructor.
    print >> outputFile, "  "*indent, "class %s%s {" % ( typedef.name, s )

    # iterate over access
    for access in pygccxml.declarations.ACCESS_TYPES.ALL:

      # print the access type
      print >> outputFile, "  "*indent, "  %s:" % access

      # warnings or no warning?
      w = access not in options.access_warnings
      for member in typedef.type.declaration.get_members( access=access ):
        if isinstance( member, pygccxml.declarations.calldef.constructor_t ):
          generate_constructor( typedef, member, indent, w )
        elif isinstance( member, pygccxml.declarations.calldef.destructor_t ):
          generate_destructor( typedef, member, indent, w )
    # finally, close the class
    print >> outputFile, "  "*indent, "};"
    print >> outputFile
    print >> outputFile


def generate_constructor( typedef, constructor, indent, w ):
  # iterate over the arguments
  args = []
  for arg in constructor.arguments:
    s = "%s %s" % (get_alias(getDeclarationString(arg), w), arg.name)
    # append the default value if it exists
    if arg.default_value:
      s += " = %s" % arg.default_value
    # and add the string to the arg list
    args.append( s )
  print >> outputFile, "  "*indent, "    %s(%s);" % (typedef.name, ", ".join( args) )


def generate_destructor( typedef, destructor, indent, w ):
  print >> outputFile, "  "*indent, "    ~%s();" % typedef.name


def generate_enum( typedef ):
  name = typedef.name
  enum = typedef.type.declaration
  decl_string = typedef.type.decl_string
  # extract the namespace to put it in c++ code. Without that, the code generated by swig
  # is wrong because it doesn't include the namespace
  ns = "::".join( decl_string.split("::")[:-1])
  print >> outputFile, "%{"
  print >> outputFile, "using namespace %s;" % ns
  print >> outputFile, "%}"
  content = [" %s = %i" % (key, value) for key, value in enum.values]
  print >> outputFile, "enum %s { %s };" % ( name, ", ".join( content ) )


def generate_nested_enum( typedef, enum, indent, w ):
  content = [" %s = %i" % (key, value) for key, value in enum.values]
  print >> outputFile, "  "*indent, "    enum %s { %s };" % ( enum.name, ", ".join( content ) )


def generate_method( typedef, method, indent, w ):
  info("Generating interface for method  '%s::%s'." % (typedef.name, method.name) )
  # avoid the apply method for the class vnl_c_vector: the signature is quite strange
  # and currently confuse swig :-/
  if "(" in method.return_type.decl_string :
    warn( 1, "ignoring method not supported by swig '%s::%s'." % (typedef.name, method.name), w )
    return

  if ( (typedef.name.startswith('vnl_') and method.name in ["as_ref"])
       or (typedef.name.startswith('itk') and method.name in ["rBegin", "rEnd", "GetSpacingCallback", "GetOriginCallback", "Begin", "End"]) ) :
    warn( 3, "ignoring black listed method '%s::%s'." % (typedef.name, method.name), w )
    return

  # iterate over the arguments
  args = []
  for arg in method.arguments:
    s = "%s %s" % (get_alias(getDeclarationString(arg), w), arg.name)
    if "(" in s:
      warn( 1, "ignoring method not supported by swig '%s::%s'." % (typedef.name, method.name), w )
      return
    # append the default value if it exists
    if arg.default_value:
      s += " = %s" % arg.default_value
    # and add the string to the arg list
    args.append( s )

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

  print >> outputFile, "  "*indent, "    %s%s %s(%s)%s;" % (static, get_alias(getDeclarationString(method.return_type), w), method.name, ", ".join( args), const )



# init the pygccxml stuff
pygccxml.declarations.scopedef_t.RECURSIVE_DEFAULT = False
pygccxml.declarations.scopedef_t.ALLOW_EMPTY_MDECL_WRAPPER = True
# pass a fake gccxml path: it is not used here, but is required by
# pygccxml
pygccxml_config = pygccxml.parser.config.config_t()
#pygccxml_config = pygccxml.parser.config.config_t("gccxml")
# create a reader
pygccxml_reader = pygccxml.parser.source_reader.source_reader_t(pygccxml_config)
# and read a xml file
info("Processing %s." % args[0])
res = pygccxml_reader.read_xml_file(args[0])

global_ns = pygccxml.declarations.get_global_namespace( res )
cable_ns = global_ns.namespace('_cable_')
wrappers_ns = cable_ns.namespace('wrappers')
# pygccxml.declarations.print_declarations( global_ns )

moduleName = cable_ns.variable('group').value[len('(const char*)"'):-1]

# and begin to write the output
headerFile = cStringIO.StringIO()
info("Generating %s header." % args[1])
print >> headerFile, "// This file is automatically generated."
print >> headerFile, "// Do not modify this file manually."
print >> headerFile
print >> headerFile

# first, define the module
# [1:-1] is there to drop the quotes
for lang in ["CHICKEN", "CSHARP", "GUILE", "JAVA", "LUA", "MODULA3", "MZSCHEME", "OCAML", "PERL", "PERL5", "PHP", "PHP4", "PHP5", "PIKE", "PYTHON", "R", "RUBY", "SEXP", "TCL", "XML"]:
  print >> headerFile, "#ifdef SWIG%s" % lang
  print >> headerFile, "%%module %s%s" % ( moduleName, lang.title() )
  print >> headerFile, "#endif"
print >> headerFile

# add the includes
# use a set to avoid putting many times the same include
s = set()
print >> headerFile, "%{"
# the include files passed in option
for f in options.includes:
  i = '#include "%s"' % f
  if not i in s:
    print >> headerFile, i
    s.add( i )
# and the includes files from other files
for file_name in options.take_includes:
  f = file( file_name )
  for l in f :
    if l.startswith( '#include' ):
      i = " ".join(l.strip().split())
      if not i in s:
        print >> headerFile, i
        s.add( i )
  f.close()
print >> headerFile, "%}"
print >> headerFile
print >> headerFile

# load the aliases files
print >> headerFile, "%{"
# the idx files passed in option
for f in options.idx:
  load_idx(f)
# and the idx files in the mdx ones
for f in options.mdx:
  load_mdx(f)
# iterate over all the typedefs in the _cable_::wrappers namespace
# to fill the alias dict
for typedef in wrappers_ns.typedefs(): #allow_empty=True):
  s = str(typedef.type.decl_string)
  # drop the :: prefix - it make swig produce invalid code
  if s.startswith("::"):
    s = s[2:]
  if not aliases.has_key( s ) :
    warn( 2, "%s (%s) should be already defined in the idx files." % (s, typedef.name) )
    aliases[s] = typedef.name
    # declare the typedef
    print >> headerFile, "typedef %s %s;" % (s, typedef.name)

print >> headerFile, "%}"
print >> headerFile
print >> headerFile

# add the imports
importFile = cStringIO.StringIO()
for f in options.imports:
  print >> importFile, "%%import %s" % f
print >> importFile
print >> importFile

# add the swig includes
includeFile = cStringIO.StringIO()
for f in options.swig_includes:
  print >> includeFile, "%%include %s" % f
print >> includeFile
print >> includeFile


# iterate over all the typedefs in the _cable_::wrappers namespace
# to build a list of classes with the dependecies
# classes :: [(name, [dep_name], typedef)]
classes = []
for typedef in wrappers_ns.typedefs():

  # begin a new class
  if isinstance( typedef.type.declaration, pygccxml.declarations.class_declaration.class_t ):
    classes.append( (typedef.name, [get_alias(super_class.related_class.decl_string) for super_class in typedef.type.declaration.bases], typedef) )

  elif isinstance( typedef.type.declaration, pygccxml.declarations.enumeration.enumeration_t ):
    # warn( 6, "Enum are currently supported only nested in a class." )
    generate_enum( typedef )

  else:
    warn( 5, "Unknown type type: %s" % str(typedef.type.declaration) )


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
      nclasses.append( (name, deps, typedef) )
  classes = nclasses


# now really generate the swig interface
for typedef in typedefs:
  # begin a new class
  generate_class( typedef )


if len(warnings) > 0 and options.warningError:
  sys.exit(1)


# search the files to import
usedSources = set()
for alias in usedTypes:
  if typedefSource.has_key( alias ):
    idxName = os.path.basename( typedefSource[ alias ] )
    iName = idxName[:-len(".idx")]
    usedSources.add( iName )
outputFileName = os.path.basename( args[1] )
if outputFileName in usedSources:
  usedSources.remove( outputFileName )
# print usedSources
for src in usedSources:
  print >> importFile, "%%import %s.i" % src
print >> importFile
print >> importFile


# create the typedef header
if options.typedef_output:
  typedefFile = cStringIO.StringIO()
  print >> typedefFile, "#ifndef __%sSwigInterface_h" % moduleName
  print >> typedefFile, "#define __%sSwigInterface_h" % moduleName
  if options.typedef_input:
    f = file(options.typedef_input)
    print >> typedefFile, f.read()
    f.close()
  for src in usedSources:
    print >> typedefFile, '#include "%sSwigInterface.h"' % src
  print >> typedefFile, "#endif"
  f = file(options.typedef_output, "w")
  f.write( typedefFile.getvalue() )
  f.close()


# finally, really write the output
content = headerFile.getvalue()+ importFile.getvalue() + includeFile.getvalue() + outputFile.getvalue()

if args[1] != '-':
  if options.keep and os.path.exists( args[1] ) and file( args[1] ).read() == content:
    info("%s unchanged." % args[1])
  else:
    info("Writing %s." % args[1])
    f = file(args[1], "w")
    f.write( content )
    f.close()
else:
  sys.stdout.write( content )

# Stop process time measurement
info("%s seconds i-generation time." % (time.clock() - t0))
