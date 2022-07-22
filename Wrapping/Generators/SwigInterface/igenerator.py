#!/usr/bin/env python
"""
igenerator.py
PURPOSE:
An important component in wrapping ITK in Python. Converts the output of CastXML into a format readable by SWIG
INPUTS:
    mdx: master idx file to be used.
#      import: File to be imported in the generated interface file.
    Swig-include: File to be included by swig (%include) in the generated interface file.
    Disable-warning: Warning to be disabled.
Disable-access-warning: Access level where warnings are disabled (public, protected, private).
    Warning-error: Treat warnings as errors.
    Verbose: Log what is currently done.
    Keep: Don't rewrite the output file if the content is unchanged.
    Pygccxml-path: Path to pygccxml
    Castxml_path: Path to castxml
    Interface-output-dir: Directory to write the Swig input files
    Library-output-dir: Directory to read the xml abstract syntax tree input files
    Submodule-order: List of submodules that must be wrapped in the given order
Snake-case-file: The configuration file to be appended to if snake_case_functions are found
    Pyi_dir: The directory for .pyi files to be generated

STEPS: (Very brief summary, many steps excluded, does not discuss pyi generation steps)
Parse through the master index file (mdx) to produce a list of submodule_names_list
Read the xml AST file for each submodule
From that xml generate an AST using pygccxml
Convert the ASTs into a list of pygccxml namespace objects
Generate an .idx file from each submodule’s namespace object using ITK’s IdxGenerator.
I think that this is a file needed by swig to learn information about each typedef for each method
For each submodule, generate the swig input files (.i) and snake case process object functions again
using the wrapping_namespace objects generated in 2
This is where most of the work takes place
Create a list of classes defined in the wrappers_namespace
From that extract a list of typedefs

Typedefs listed in dependency order
For each typedef, generate its class


Call self.generate_process_object_snake_case_functions(typedefs)
Generate file header
Generate import file, include file, and apply file
Concatenate header, imports, includes, applies, and the generated class together and save to a .i file for SWIG

MDX Files
PURPOSE:
Used by igenerator.py to generate a list of submodule_names_list. One or several can be passed on any given run.
    Contains a list of dependencies
STEPS:
    Generated through CMake right before igenerator.py runs (in the same macro)
WRAPPER_LIBRARY_DEPENDS contains a list of .mdx dependencies that are added to the files
Also appears to be affected by the itk_end_wrap_submodule_swig_interface
 Macro through the SWIG_INTERFACE_MDX_CONTENT variable
 (but this macro is not called by the same macro that generates the file)

"""
# -*- coding: utf-8 -*-
import collections
import pickle
import sys
import os
import re
from argparse import ArgumentParser
from io import StringIO
from os.path import exists
from pathlib import Path
from keyword import iskeyword
from typing import List, Dict, Any


def argument_parser():
    cmdln_arg_parser = ArgumentParser()
    cmdln_arg_parser.add_argument(
        "--mdx",
        action="append",
        dest="mdx",
        default=[],
        metavar="FILE",
        help="master idx file to be used.",
    )
    cmdln_arg_parser.add_argument(
        "--import",
        action="append",
        dest="imports",
        default=[],
        metavar="FILE",
        help="File to be imported in the generated interface file.",
    )
    cmdln_arg_parser.add_argument(
        "--swig-include",
        action="append",
        dest="swig_includes",
        default=[],
        metavar="FILE",
        help=(
            "File to be included by swig (%include) in the generated " "interface file."
        ),
    )
    cmdln_arg_parser.add_argument(
        "-w",
        "--disable-warning",
        action="append",
        dest="warnings",
        default=[],
        metavar="WARNING",
        help="Warning to be disabled.",
    )
    cmdln_arg_parser.add_argument(
        "-A",
        "--disable-access-warning",
        action="append",
        dest="access_warnings",
        default=[],
        metavar="LEVEL",
        help=(
            "Access level where warnings are disabled " "(public, protected, private)."
        ),
    )
    cmdln_arg_parser.add_argument(
        "-W",
        "--warning-error",
        action="store_true",
        dest="warningError",
        help="Treat warnings as errors.",
    )
    cmdln_arg_parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        dest="verbose",
        help="Log what is currently done.",
    )
    cmdln_arg_parser.add_argument(
        "-k",
        "--keep",
        action="store_true",
        dest="keep",
        help="Don't rewrite the output file if the content is unchanged.",
    )
    cmdln_arg_parser.add_argument(
        "-p",
        "--pygccxml-path",
        action="store",
        dest="pygccxml_path",
        help="Path to pygccxml",
    )
    cmdln_arg_parser.add_argument(
        "-g",
        "--castxml-path",
        action="store",
        dest="castxml_path",
        help="Path to castxml",
    )
    cmdln_arg_parser.add_argument(
        "-o",
        "--interface-output-dir",
        action="store",
        dest="interface_output_dir",
        help="Directory to write the Swig input files",
    )
    cmdln_arg_parser.add_argument(
        "-l",
        "--library-output-dir",
        action="store",
        dest="library_output_dir",
        help="Directory to read the xml abstract syntax tree input files",
    )
    cmdln_arg_parser.add_argument(
        "-s",
        "--submodule-order",
        action="store",
        dest="submodule_order",
        help="List of submodules that must be wrapped in the given order",
    )
    cmdln_arg_parser.add_argument(
        "-a",
        "--snake-case-file",
        action="store",
        dest="snake_case_file",
        help="The configuration file to be appended to if snake_case_functions are found",
    )

    cmdln_arg_parser.add_argument(
        "--pyi_dir",
        action="store",
        dest="pyi_dir",
        default="",
        type=str,
        help="The directory for .pyi files to be generated",
    )

    cmdln_arg_parser.add_argument(
        "--pyi_index_list",
        action="store",
        dest="pyi_index_list",
        default="",
        type=str,
        help="The list of byproduct index files that contain the per-submodule pkl files "
        + "for each 'itk class' that is instantiated.  Useful for debugging for error checking.",
    )
    cmdln_arg_parser.add_argument(
        "--pkl_dir",
        action="store",
        dest="pkl_dir",
        default="",
        type=str,
        help="The directory for .pyi files to be generated",
    )
    cmdln_arg_parser.add_argument(
        "-d",
        action="store_true",
        dest="debug_code",
        help="Enable debugging dode",
    )

    options = cmdln_arg_parser.parse_args()
    return options


glb_options = argument_parser()
sys.path.insert(1, glb_options.pygccxml_path)
import pygccxml
import logging


# Global debugging variables
pyi_approved_index_list: List[Path] = [
    Path(x) for x in glb_options.pyi_index_list.split(";")
]
pyi_created_index_list: List[Path] = []


# The ITKClass is duplicated in pyi_generator.py
class ITKClass:
    # Structure
    # { method_name: [
    #                  # Each entry of this list contains a description of the definition for the single method
    #                  # Each description is similar but can contain different types or defaults
    #                  [
    #                    # Describe each argument of the method
    #                    (
    #                      argument_name,
    #                      python_arg_type,
    #                      default_value # Use None if no default
    #                    ),
    #                    ...,
    #                    # The final entry is unique, this contains the information about the return value
    #                    (
    #                      None, # This indicates that it is a return type and not an argument
    #                      return_type, # acquired through the `get_arg_type` method
    #                      is_static # True if this is a static method
    #                    )
    #                  ],
    #
    #                  # Describe other overloads of the same method (method_name) here:
    #                  [ ... ],
    #                  ...
    #                ],
    #
    #   another_method_name: ...
    #   ...
    # }
    def __init__(self, l_class_name):
        self.python_method_headers = {}
        self.has_new_method = False
        self.typed = False
        self.parent_class = []
        self.is_abstract = False
        self.class_name = l_class_name
        self.is_enum = False
        self.has_superclass = False
        self.enums = []
        self.submodule_name = ""

    def __eq__(self, other):
        if isinstance(other, ITKClass):
            return (
                self.python_method_headers == other.python_method_headers
                and self.has_new_method == other.has_new_method
                and self.typed == other.typed
                and self.parent_class == other.parent_class
                and self.is_abstract == other.is_abstract
                and self.class_name == other.class_name
                and self.is_enum == other.is_enum
                and self.enums == other.enums
                and self.submodule_name == other.submodule_name
            )

        return False


def remove_class_type(itkclass: str) -> (str, bool):
    typed = False
    if itkclass.startswith("itk"):
        itkclass = itkclass[3:]
    if itkclass.endswith("vnl_lbfgs") and itkclass != "vnl_lbfgs":
        itkclass = itkclass.replace("vnl_lbfgs", "")
        typed = True
    elif itkclass.endswith("vnl_lbfgsb") and itkclass != "vnl_lbfgsb":
        itkclass = itkclass.replace("vnl_lbfgsb", "")
        typed = True

    match = list(
        re.finditer(
            r"[A-Z_\d]+(Lanczos|Cosine|Welch|Hamming|Neighborhood)*[A-Z_\d]*$",
            itkclass,
        )
    )

    if itkclass.startswith("Array2D"):
        return "Array2D", True
    elif itkclass.startswith("DiffusionTensor3D"):
        return "DiffusionTensor3D", True
    elif itkclass.startswith("KdTreeBasedKmeansEstimatorKdTree"):
        return "KdTreeBasedKmeansEstimator", True
    elif itkclass.startswith("MapContainerIstring"):
        return "MapContainer", True
    elif itkclass.startswith("Point1D"):
        return "Point1D", False
    elif itkclass.startswith("PointBasedSpatialObject"):
        return "PointBasedSpatialObject", True
    elif itkclass.startswith("MetaDataObject") and not itkclass.startswith(
        "MetaDataObjectBase"
    ):
        return "MetaDataObject", True
    elif itkclass.startswith("SimpleDataObjectDecorator"):
        return "SimpleDataObjectDecorator", True
    elif "IO" in itkclass or ("v4" in itkclass and "Div" not in itkclass):
        itk_string = "IO" if "IO" in itkclass else "v4"
        position = itkclass.find(itk_string) + 2
        if len(itkclass) > position:
            end, is_typed = remove_class_type(itkclass[position:])
            if end == "REGv4":
                end = ""
                is_typed = True
            return itkclass[: itkclass.find(itk_string) + 2] + end, is_typed
        else:
            return itkclass[: itkclass.find(itk_string) + 2], False or typed
    elif len(match) > 0:
        itkclass = itkclass[: match[0].start(0)]
        return itkclass, True
    else:
        return itkclass, False or typed


def convert_cpp_to_python_value(cpp_value: str):
    if cpp_value.isnumeric():
        return cpp_value
    elif "." in cpp_value and cpp_value.replace(".", "", 1).isnumeric():
        if "." == cpp_value[-1]:
            return cpp_value[:-1]
        else:
            return cpp_value
    elif cpp_value == "false" or cpp_value == "true":
        return cpp_value.capitalize()
    else:
        return "..."


def get_arg_type(decls, arg_type, for_snake_case_hints=True):
    arg_type = decls.remove_alias(arg_type)
    arg_type = decls.remove_reference(arg_type)
    arg_type = decls.remove_cv(arg_type)
    arg_type_str = str(arg_type)

    lib = ""
    if for_snake_case_hints:
        lib = "itkt."

    if decls.is_bool(arg_type):
        return "bool"
    elif decls.is_integral(arg_type):
        return "int"
    elif decls.is_floating_point(arg_type):
        return "float"
    elif decls.is_std_string(arg_type):
        return "str"
    elif decls.is_array(arg_type):
        item_type = decls.array_item_type(arg_type)
        python_type = SwigInputGenerator.cpp_to_python(str(item_type))
        if python_type is not None:
            return f"Sequence[{python_type}]"
    elif not for_snake_case_hints and arg_type_str.endswith("::Iterator"):
        return None
    elif not for_snake_case_hints and decls.is_pointer(arg_type):
        return None
    elif (
        arg_type_str.startswith("itk::FixedArray")
        or arg_type_str.startswith("itk::Vector<")
        or arg_type_str.startswith("itk::CovariantVector<")
        or arg_type_str.startswith("itk::Point<")
        or arg_type_str.startswith("itk::Array<")
    ):
        item_type = decls.templates.split(arg_type_str)[1][0]
        python_type = SwigInputGenerator.cpp_to_python(item_type)
        if python_type is not None:
            return f"Sequence[{python_type}]"
    elif arg_type_str.startswith("itk::VectorContainer<"):
        item_type = decls.templates.split(arg_type_str)[1][1]
        python_type = SwigInputGenerator.cpp_to_python(item_type)
        if python_type is not None:
            return f"Sequence[{python_type}]"
        elif item_type.startswith("itk::Offset<"):
            return f"Sequence[Sequence[int]]"
    elif arg_type_str.startswith("std::vector<"):
        item_type = decls.templates.split(arg_type_str)[1][0]
        python_type = SwigInputGenerator.cpp_to_python(item_type)
        if python_type is not None:
            return f"Sequence[{python_type}]"
    elif (
        arg_type_str.startswith("itk::Size<")
        or arg_type_str.startswith("itk::Offset<")
        or arg_type_str.startswith("itk::Index<")
    ):
        return "Sequence[int]"
    elif arg_type_str.startswith("itk::ImageRegion<"):
        return lib + "ImageRegion"
    elif arg_type_str.startswith("itk::InterpolateImageFunction<"):
        return lib + "InterpolateImageFunction"
    elif arg_type_str.startswith("itk::ExtrapolateImageFunction<"):
        return lib + "ExtrapolateImageFunction"
    elif arg_type_str.startswith("itk::Image<"):
        return lib + "Image"
    elif arg_type_str.startswith("itk::VectorImage<"):
        return lib + "VectorImage"
    elif arg_type_str.startswith("itk::ImageBase<"):
        return lib + "ImageBase"
    elif arg_type_str.startswith("itk::PointSet<"):
        return lib + "PointSet"
    elif arg_type_str.startswith("itk::Mesh<"):
        return lib + "Mesh"
    elif arg_type_str.startswith("itk::QuadEdgeMesh<"):
        return lib + "QuadEdgeMesh"
    elif arg_type_str.startswith("itk::Transform<"):
        return lib + "Transform"
    elif arg_type_str.startswith("itk::ImageBoundaryCondition<"):
        return lib + "ImageBoundaryCondition"
    elif arg_type_str.startswith("itk::FlatStructuringElement<"):
        return lib + "FlatStructuringElement"
    elif arg_type_str.startswith("itk::RGBPixel<"):
        return "Tuple[int, int, int]"
    elif arg_type_str.startswith("itk::RGBAPixel<"):
        return "Tuple[int, int, int, int]"
    elif not for_snake_case_hints and arg_type_str == "void":
        return "None"
    elif not for_snake_case_hints and arg_type_str.startswith("vnl_"):
        return arg_type_str.split("<")[0]
    return None


def get_type(v):
    if hasattr(v, "decl_type"):
        return get_type(v.decl_type)
    if hasattr(v, "declaration"):
        return get_type(v.declaration)
    return v


class IdxGenerator:
    """Generates the .idx file for an ITK wrapping submodule (which usually
    corresponds to a class)."""

    def __init__(self, submodule_name: str):
        self.submodule_name: str = submodule_name
        # the output file
        self.outputFile = StringIO()

    def create_idxfile(self, idx_file_path: str, wrappers_namespace):
        # iterate over all the typedefs in the _wrapping_::wrappers namespace
        for typedef in wrappers_namespace.typedefs():
            n = typedef.name
            s = get_type(typedef).decl_string
            # drop the :: prefix - it make swig produce invalid code
            if s.startswith("::"):
                s = s[2:]
            idx_line_value: str = f"{{{s}}} {{{n}}} {{{self.submodule_name}}}\n"
            self.outputFile.write(idx_line_value)

        with open(idx_file_path, "w") as f:
            content: str = self.outputFile.getvalue()
            f.write(content)


class SwigInputGenerator:
    """Generates a swig input .i file for an ITK module."""

    notWrapped = [
        "std::_Deque_alloc<.+>",
        "itk::AtomicInt<.+>",
        "itk::MapContainer< unsigned long, itk::CellInterface<.+>",
        "itk::VectorContainer< unsigned long, itk::CellInterface<.+>",
        "itk::CellInterface< double, itk::QuadEdgeMeshCellTraitsInfo<.+>",
        "itk::QuadEdgeMeshLineCell< itk::CellInterface<.+>",
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
        r"itk::Image< itk::CovariantVector< double, \d+u >, \d+u >",
        "itk::FixedArray< itk::SmartPointer.+ >",
        # used internally in itkMattesMutualInformationImageToImageMetric
        "itk::SmartPointer< itk::Image.+ >",
        # used internally in itkImageRegistrationMethodv4
        "itk::SmartPointer< const itk::Image.+ >",
        "itk::SmartPointer< const itk::PointSet.+ >",
        "itk::SmartPointer< const itk::Mesh.+ >",
        "itk::ObjectFactoryBasePrivate",
        "itk::ThreadPoolGlobals",
        "itk::MultiThreaderBaseGlobals",
        ".+[(][*][)][(].+",  # functor functions
    ]

    forceSnakeCase = ["ImageDuplicator"]

    notWrappedRegExp = re.compile("|".join(["^" + s + "$" for s in notWrapped]))

    notWrappedMethods = [
        "IsNull",
        "IsNotNull",
        "GetPointer",
        "Swap",
        "Register",
        "UnRegister",
        "CreateAnother",
        "PrintSelf",
        "Delete",
        "SetReferenceCount",
        "PrintHeader",
        "PrintTrailer",
        "InternalClone",
    ]

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
    """,
    }

    new_override = '''
// some changes in the New() method
%rename(__New_orig__) {class_name}::New;
%extend {class_name} {{
%pythoncode %{{
    def New(*args, **kargs):
        """New() -> {class_name}

        Create a new object of the class {class_name} and set the input and the parameters if some
        named or non-named arguments are passed to that method.

        New() tries to assign all the non named parameters to the input of the new objects - the
        first non named parameter in the first input, etc.

        The named parameters are used by calling the method with the same name prefixed by 'Set'.

        Ex:

          {class_name}.New(reader, threshold=10)

        is (most of the time) equivalent to:

          obj = {class_name}.New()
          obj.SetInput(0, reader.GetOutput())
          obj.SetThreshold(10)
        """
        obj = {class_name}.__New_orig__()
        from itk.support import template_class
        template_class.New(obj, *args, **kargs)
        return obj
    New = staticmethod(New)
  %}}
}}
%pythoncode %{{
    def {class_name}_New():
        return {class_name}.New()
%}}


'''

    new_override_pycommand = '''
// some changes in the New() method
%rename(__New_orig__) {class_name}::New;
%extend {class_name} {{
%pythoncode %{{
    def New(*args, **kargs):
        """New() -> {class_name}
        """
        obj = {class_name}.__New_orig__()
        import itk
        itk.set_inputs(obj, *args, **kargs)
        return obj
    New = staticmethod(New)
%}}
}}
%pythoncode %{{
    def {class_name}_New():
        return {class_name}.New()
%}}


'''

    cpp_to_python_dict = {
        "bool": "bool",
        "float": "float",
        "double": "float",
        "long double": "float",
        "char": "int",
        "unsigned char": "int",
        "signed char": "int",
        "short": "int",
        "unsigned short": "int",
        "int": "int",
        "unsigned int": "int",
        "long": "int",
        "unsigned long": "int",
        "long long": "int",
        "unsigned long long": "int",
    }

    def __init__(self, submodule_name, options):
        self.submodule_name = submodule_name
        # The first mdx file is the master index file for this module.
        self.moduleName = Path(options.mdx[0]).stem
        self.options = options

        self.outputFile = StringIO()
        self.apply_file_names = []

        # A dict of sets containing the .pyi python equivalent for all class methods and params
        self.classes: Dict[str, ITKClass] = dict()
        self.current_class = ""

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
        self.snakeCaseProcessObjectFunctions = set()

    def warn(self, identifier, msg, do_warn=True):
        if not do_warn:
            # don't warn for anything
            return
        if str(identifier) not in self.options.warnings:
            if not self.verbose and (identifier, msg) in self.warnings:
                # just do nothing
                return
            self.warnings.add((identifier, msg))
            if self.verbose:
                if self.options.warningError:
                    print(f"error({str(identifier)}): {msg}", file=sys.stderr)
                else:
                    print(f"warning({str(identifier)}): {msg}", file=sys.stderr)
            else:
                if self.options.warningError:
                    print(
                        f"{self.submodule_name}: error({str(identifier)}): {msg}",
                        file=sys.stderr,
                    )
                else:
                    print(
                        f"{self.submodule_name}: warning({str(identifier)}): {msg}",
                        file=sys.stderr,
                    )

    def info(self, msg):
        if self.verbose:
            print(f"info: {msg}", file=sys.stderr)

    @staticmethod
    def get_declaration_string(t):
        t = get_type(t)
        if t.decl_string == "::PyObject *":
            # don't go further - we want to keep that one as is
            return "::PyObject *"
        if isinstance(t, pygccxml.declarations.cpptypes.pointer_t):
            return SwigInputGenerator.get_declaration_string(get_type(t.base)) + " *"
        elif isinstance(t, pygccxml.declarations.cpptypes.const_t):
            return (
                SwigInputGenerator.get_declaration_string(get_type(t.base)) + " const"
            )
        elif isinstance(t, pygccxml.declarations.cpptypes.reference_t):
            return SwigInputGenerator.get_declaration_string(get_type(t.base)) + " &"
        return t.decl_string

    def rename_types_in_stl(self, s):
        if s.startswith("std::") and pygccxml.declarations.templates.is_instantiation(
            s
        ):
            args = []
            for arg in pygccxml.declarations.templates.args(s):
                t, d = SwigInputGenerator.type_and_decorators(arg)
                args.append(self.rename_types_in_stl(self.get_alias(t)) + d)
            return (
                pygccxml.declarations.templates.join(
                    pygccxml.declarations.templates.name(s), args
                )
                + SwigInputGenerator.type_and_decorators(s)[1]
            )
        return s

    @staticmethod
    def remove_std_allocator(s):
        if pygccxml.declarations.templates.is_instantiation(s):
            args = []
            for arg in pygccxml.declarations.templates.args(s):
                if not arg.startswith("std::allocator"):
                    t, d = SwigInputGenerator.type_and_decorators(arg)
                    args.append(SwigInputGenerator.remove_std_allocator(t) + d)
            return (
                pygccxml.declarations.templates.join(
                    pygccxml.declarations.templates.name(s), args
                )
                + SwigInputGenerator.type_and_decorators(s)[1]
            )
        return s

    @staticmethod
    def type_and_decorators(s):
        end = ""
        s = s.strip()
        ends = [" ", "*", "&", "const"]
        need_to_continue = True
        while need_to_continue:
            need_to_continue = False
            for e in ends:
                if s.endswith(e):
                    end = e + end
                    s = s[: -len(e)]
                    need_to_continue = True
        return s, end

    _firstCapRE = re.compile(r"(.)([A-Z][a-z]+)")
    _allCapRE = re.compile(r"([a-z\d])([A-Z])")

    @staticmethod
    def camel_case_to_snake_case(camel_case):
        substitution = SwigInputGenerator._firstCapRE.sub(r"\1_\2", camel_case)
        return (
            SwigInputGenerator._allCapRE.sub(r"\1_\2", substitution)
            .lower()
            .replace("__", "_")
        )

    @staticmethod
    def kwarg_of_interest(member_name):
        """
        This function accepts a member function name and returns whether we
        want to list it explicitly in the process_object functional interface
        kwargs.
        """
        if not member_name.startswith("Set"):
            return False

        rest = member_name[3:]
        if rest in [
            "Input",
            "Input1",
            "Input2",
            "Input3",
            "InputImage",
            "InPlace",
            "CoordinateTolerance",
            "DirectionTolerance",
            "GlobalDefaultCoordinateTolerance",
            "GlobalDefaultDirectionTolerance",
            "NumberOfWorkUnits",
            "Lambda",
        ]:
            return False

        return True

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

        (s, end) = SwigInputGenerator.type_and_decorators(s)

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

        if s[: s.rfind("::")] in self.aliases:
            # take care of subtypes/enum/...
            alias = self.aliases[s[: s.rfind("::")]] + s[s.rfind("::") :]
            self.usedTypes.add(alias)
            return alias + end

        # replace the types defined in this type, to support
        # std::vector<itkDataObject> for example
        s = self.rename_types_in_stl(s)

        # drop the allocator part of the type, because it is not supported by the
        # %template directive with some generators (like tcl)
        s = SwigInputGenerator.remove_std_allocator(s)

        # rename basic_string to std::string to make name shorter
        s = s.replace("std::basic_string< char >", "std::string")
        s = s.replace(
            "std::basic_string< char, std::char_traits< char > >", "std::string"
        )
        s = s.replace(
            "std::basic_ostream< char, std::char_traits< char > >", "std::ostream"
        )
        s = s.replace(
            "std::basic_istream< char, std::char_traits< char > >", "std::istream"
        )
        s = s.replace(
            "std::basic_ofstream< char, std::char_traits< char > >", "std::ostream"
        )
        s = s.replace(
            "std::basic_ifstream< char, std::char_traits< char > >", "std::istream"
        )

        # rename some types not renamed by gccxml (why ?)
        s = s.replace("itk::SerieUIDContainer", "std::vector< std::string >")
        s = s.replace("itk::FilenamesContainer", "std::vector< std::string >")

        if s.startswith("itk::") and not self.notWrappedRegExp.match(s):
            self.warn(4, f"ITK type not wrapped, or currently not known: {s}", w)

        self.usedTypes.add(s)
        return s + end

    def load_idx(self, file_name):
        with open(file_name) as f:
            for line in f:
                (full_name, alias, module) = re.findall(r"{(.*)} {(.*)} {(.*)}", line)[
                    0
                ]
                # workaround lack of :: prefix in idx files
                # TODO: would it be better to remove the :: prefix in the output of
                # pygccxml ?
                # full_name = "::"+full_name
                # normalize some basic type names
                full_name = self.normalize(full_name)

                if full_name in self.aliases:
                    # If the full_name key already exists, do not overwrite the
                    # value. load_idx() is called once before load_mdx(), making
                    # sure the first aliases loaded are the ones belonging to
                    # the current submodule (and the next load_idx() calls
                    # should not overwrite these aliases.
                    continue

                self.aliases[full_name] = alias
                # store the source of the def
                if (
                    alias in self.typedefSource
                    and file_name != self.typedefSource[alias]
                ):
                    self.warn(
                        7,
                        "%s in %s is already defined in %s."
                        % (alias, file_name, self.typedefSource[alias]),
                    )
                else:
                    self.typedefSource[alias] = file_name

    def load_mdx(self, file_name):
        if file_name in self.mdx_loaded:
            # already loaded - no need to do it again
            return
        self.mdx_loaded.add(file_name)
        with open(file_name) as f:
            lines = f.readlines()
        for line in lines:
            line_stripped = line.strip()
            if line.startswith("%") or line.isspace():
                # exclude the lines which are starting with % - that's not the idx
                # files
                pass
            elif line_stripped.endswith(".mdx"):
                self.load_mdx(os.path.dirname(file_name) + os.sep + line_stripped)
            elif line_stripped[:-4] == self.submodule_name:
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
        name = " ".join(name.replace(",", ", ").split())
        return name

    @staticmethod
    def cpp_to_python(name):
        name = SwigInputGenerator.normalize(name)
        if name in SwigInputGenerator.cpp_to_python_dict:
            return SwigInputGenerator.cpp_to_python_dict[name]
        return None

    def generate_class(self, typedef, indent=0):
        self.info(f"Generating interface for {typedef.name}.")

        decls = pygccxml.declarations

        if typedef.name == "itkLightObject" and self.current_class is not None:
            self.classes[self.current_class].has_new_method = True

        s = ""  # Set default superclass name to an empty string
        if not typedef.name.startswith("stdcomplex"):
            for member in get_type(typedef).get_members(
                access=decls.ACCESS_TYPES.PUBLIC
            ):
                if (
                    isinstance(member, decls.member_function_t)
                    and member.name == "New"
                    and not typedef.name == "itkLightObject"
                ):
                    if self.current_class is not None:
                        curr_class = self.classes[self.current_class]
                        curr_class.has_new_method = True
                    if typedef.name == "itkPyCommand":
                        self.outputFile.write(
                            self.new_override_pycommand.format(class_name=typedef.name)
                        )
                    else:
                        self.outputFile.write(
                            self.new_override.format(class_name=typedef.name)
                        )
                    self.outputFile.write("\n")
                    break

            super_classes = []
            for super_class in get_type(typedef).bases:
                super_classes.append(
                    "%s %s"
                    % (
                        super_class.access,
                        self.get_alias(super_class.related_class.decl_string),
                    )
                )
            if super_classes:
                s = " : " + ", ".join(super_classes)
            self.outputFile.write("  " * indent)
            self.outputFile.write(f"class {typedef.name}{s} {{\n")

            # iterate over access
            for access in decls.ACCESS_TYPES.ALL:

                # the access type
                self.outputFile.write("  " * indent)
                self.outputFile.write(f"  {access}:\n")

                # warnings or no warning?
                w = access not in self.options.access_warnings

                # iterate over the members
                for member in get_type(typedef).get_members(access=access):
                    if isinstance(member, decls.typedef.typedef_t):
                        self.warn(
                            51, f"Member typedef are not supported: {member.name}", w
                        )
                    elif isinstance(member, decls.member_function_t):
                        self.generate_method(typedef, member, indent, w)
                    elif isinstance(member, decls.constructor_t):
                        self.generate_constructor(typedef, member, indent, w)
                    elif isinstance(member, decls.member_operator_t):
                        self.generate_method(
                            typedef, member, indent, w, is_operator=True
                        )
                    elif isinstance(member, decls.destructor_t):
                        self.generate_destructor(typedef, member, indent, w)
                    elif isinstance(member, decls.enumeration_t):
                        self.generate_nested_enum(typedef, member, indent, w)
                    elif isinstance(member, decls.variable_t):
                        self.warn(
                            52, f"Member variables are not supported: {member.name}", w
                        )
                    elif isinstance(member, decls.class_declaration.class_t):
                        self.warn(
                            53, f"Member classes are not supported: {member.name}", w
                        )
                    elif isinstance(
                        member, decls.class_declaration.class_declaration_t
                    ):
                        self.warn(
                            53, f"Member classes are not supported: {member.name}", w
                        )
                    elif isinstance(member, decls.casting_operator_t):
                        self.warn(
                            54,
                            "Member casting operators are not supported: %s"
                            % member.name,
                            w,
                        )
                    else:
                        self.warn(50, f"Unknown member type: {repr(member)}", w)

            # finally, close the class
            self.outputFile.write("  " * indent)
            self.outputFile.write("};\n\n")

        elif typedef.name == "stdcomplexD":
            self.outputFile.write(self.stdcomplex_headers["D"] + "\n")
        elif typedef.name == "stdcomplexF":
            self.outputFile.write(self.stdcomplex_headers["F"] + "\n")
        else:
            print("stdcomplex", typedef.name)
            # stdcomplex is too difficult to wrap in some cases. Only wrap the
            # constructor.
            self.outputFile.write("  " * indent)
            self.outputFile.write(f"class {typedef.name}{s} {{\n")

            # iterate over access
            for access in pygccxml.declarations.ACCESS_TYPES.ALL:

                # the access type
                self.outputFile.write("  " * indent)
                self.outputFile.write(f"  {access}:\n")

                # warnings or no warning?
                w = access not in self.options.access_warnings
                for member in get_type(typedef).get_members(access=access):
                    if isinstance(member, decls.constructor_t):
                        self.generate_constructor(typedef, member, indent, w)
                    elif isinstance(member, decls.destructor_t):
                        self.generate_destructor(typedef, member, indent, w)
            # finally, close the class
            self.outputFile.write("  " * indent)
            self.outputFile.write("};\n\n\n")

    def generate_process_object_snake_case_functions(self, typedefs):
        self.info("Generating snake case functions")
        process_objects = set()
        for typedef in typedefs:
            class_type = get_type(typedef)
            bases = [base.related_class.name for base in class_type.recursive_bases]
            is_process_object = "ProcessObject" in bases and not class_type.is_abstract
            short_name = class_type.name.split("<")[0]
            if is_process_object or short_name in self.forceSnakeCase:
                process_objects.add((short_name, class_type))
        if len(process_objects) > 0:
            self.outputFile.write("\n\n#ifdef SWIGPYTHON\n")
            self.outputFile.write("%pythoncode %{\n")
            for process_object, class_type in process_objects:
                snake_case = self.camel_case_to_snake_case(process_object)
                if snake_case in self.snakeCaseProcessObjectFunctions:
                    continue
                self.snakeCaseProcessObjectFunctions.add(snake_case)
                decls = pygccxml.declarations
                recursive_bases = class_type.recursive_bases
                bases = [base.related_class.name for base in recursive_bases]

                args_typehint = ""
                if any([b.startswith("ImageTo") for b in bases]):
                    args_typehint = ": itkt.ImageLike"
                elif any([b.startswith("MeshTo") for b in bases]):
                    args_typehint = ": itkt.Mesh"
                elif any([b.startswith("PathTo") for b in bases]):
                    args_typehint = ": itkt.Path"
                elif any([b.startswith("SpatialObjectTo") for b in bases]):
                    args_typehint = ": itkt.SpatialObject"

                kwargs_typehints = ""
                kwargs_of_interest = dict()
                for member in class_type.get_members(access=decls.ACCESS_TYPES.PUBLIC):
                    if isinstance(
                        member, decls.member_function_t
                    ) and self.kwarg_of_interest(member.name):
                        if len(member.argument_types) > 0:
                            arg_type = member.argument_types[0]
                            if member.name in kwargs_of_interest:
                                kwargs_of_interest[member.name].add(arg_type)
                            else:
                                kwargs_of_interest[member.name] = {arg_type}
                base_index = 0
                while recursive_bases[base_index].related_class.name != "ProcessObject":
                    base_class = recursive_bases[base_index].related_class
                    for member in base_class.get_members(
                        access=decls.ACCESS_TYPES.PUBLIC
                    ):
                        if isinstance(
                            member, decls.member_function_t
                        ) and self.kwarg_of_interest(member.name):
                            if len(member.argument_types) > 0:
                                arg_type = member.argument_types[0]
                                if member.name in kwargs_of_interest:
                                    kwargs_of_interest[member.name].add(arg_type)
                                else:
                                    kwargs_of_interest[member.name] = {arg_type}
                    base_index += 1
                    if base_index >= len(recursive_bases):
                        # ImageDuplicator, ...
                        break
                kwarg_snakes = []
                for kwarg, arg_types in kwargs_of_interest.items():
                    kwarg_snake = self.camel_case_to_snake_case(kwarg[3:])
                    kwarg_snakes.append(kwarg_snake)
                    kwargs_typehints += f" {kwarg_snake}"
                    kwarg_types = []
                    for arg_type in arg_types:
                        arg_type = get_arg_type(decls, arg_type)
                        if arg_type is not None:
                            kwarg_types.append(arg_type)
                    if len(kwarg_types) == 0:
                        pass
                    elif len(kwarg_types) == 1:
                        kwargs_typehints += f": {kwarg_types[0]}"
                    else:
                        kwargs_typehints += ": Union["
                        for kt in kwarg_types[:-1]:
                            kwargs_typehints += f"{kt}, "
                        kwargs_typehints += f"{kwarg_types[-1]}]"
                    kwargs_typehints += f"=...,"
                kwarg_dict = ",".join([f"'{k}':{k}" for k in kwarg_snakes])

                return_typehint = ""
                if any([b.startswith("ImageSource") for b in bases]):
                    return_typehint = "-> itkt.ImageSourceReturn"
                elif any([b.startswith("MeshSource") for b in bases]):
                    return_typehint = "-> itkt.MeshSourceReturn"
                elif any([b.startswith("PathSource") for b in bases]):
                    return_typehint = "-> itkt.PathSourceReturn"

                # print(args_typehint, kwargs_typehints, return_typehint)
                self.outputFile.write(
                    f"""from itk.support import helpers
import itk.support.types as itkt
from typing import Sequence, Tuple, Union

@helpers.accept_array_like_xarray_torch
def {snake_case}(*args{args_typehint}, {kwargs_typehints}**kwargs){return_typehint}:
    \"\"\"Functional interface for {process_object}\"\"\"
    import itk

    kwarg_typehints = {{ {kwarg_dict} }}
    specified_kwarg_typehints = {{ k:v for (k,v) in kwarg_typehints.items() if kwarg_typehints[k] is not ... }}
    kwargs.update(specified_kwarg_typehints)

    instance = itk.{process_object}.New(*args, **kwargs)
    return instance.__internal_call__()

def {snake_case}_init_docstring():
    import itk
    from itk.support import template_class

    filter_class = itk.{self.moduleName}.{process_object}
    {snake_case}.process_object = filter_class
    is_template = isinstance(filter_class, template_class.itkTemplate)
    if is_template:
        filter_object = filter_class.values()[0]
    else:
        filter_object = filter_class

    {snake_case}.__doc__ = filter_object.__doc__

"""
                )
            self.outputFile.write("%}\n")
            self.outputFile.write("#endif\n")

    def generate_constructor(self, typedef, constructor, indent, w):
        # iterate over the arguments
        args = []
        for arg in constructor.arguments:
            s = f"{self.get_alias(self.get_declaration_string(arg), w)} {arg.name}"
            if "unknown" in s:
                continue
            # append the default value if it exists
            if arg.default_value:
                s += f" = {arg.default_value}"
            # and add the string to the arg list
            args.append(s)
        self.outputFile.write("  " * indent)
        self.outputFile.write(f"    {typedef.name}({', '.join(args)});\n")

    def generate_destructor(self, typedef, _destructor, indent, _w):
        self.outputFile.write("  " * indent)
        self.outputFile.write(f"    ~{typedef.name}();\n")

    def generate_enum(self, typedef):
        name = typedef.name
        enum = get_type(typedef)
        decl_string = typedef.decl_type.decl_string
        # extract the namespace to put it in c++ code. Without that, the code
        # generated by swig
        # is wrong because it doesn't include the namespace
        ns = "::".join(decl_string.split("::")[:-1])
        self.outputFile.write("%{\n")
        self.outputFile.write(f"using namespace {ns};\n")
        self.outputFile.write("%}\n")
        content = [f" {key}" for key, value in enum.values]
        self.outputFile.write(
            "enum class {}: uint8_t {{ {} }};\n\n".format(name, ", ".join(content))
        )

    def generate_nested_enum(self, _typedef, enum, indent, _w):
        content = [f" {key}" for key, value in enum.values]
        self.outputFile.write("  " * indent)
        self.outputFile.write(
            "    enum class {}: uint8_t {{ {} }};\n\n".format(
                enum.name, ", ".join(content)
            )
        )
        if self.current_class is not None and self.classes[self.current_class].is_enum:
            python_enum_names = [f"{enum.name}_{name.strip()}" for name in content]
            self.classes[self.current_class].enums += python_enum_names

    def generate_method(self, typedef, method, indent, w, is_operator=False):
        self.info(f"Generating interface for method  '{typedef.name}::{method.name}'.")
        # avoid the apply method for the class vnl_c_vector: the signature is
        # quite strange and currently confuse swig :-/

        # Allow call operator to be hinted
        if is_operator and method.name == "operator()":
            is_operator = False

        if "(" in get_type(method.return_type).decl_string:
            self.warn(
                1,
                "ignoring method not supported by swig '%s::%s'."
                % (typedef.name, method.name),
                w,
            )
            return

        names = [
            "rBegin",
            "rEnd",
            "GetSpacingCallback",
            "GetOriginCallback",
            "Begin",
            "End",
        ]

        if (typedef.name.startswith("vnl_") and method.name in ["as_ref"]) or (
            typedef.name.startswith("itk") and method.name in names
        ):
            self.warn(
                3, f"ignoring black listed method '{typedef.name}::{method.name}'.", w
            )
            return

        # iterate over the arguments
        args = []
        method_hints = []
        for argIndex in range(len(method.arguments)):
            arg = method.arguments[argIndex]
            arg_type = self.get_alias(self.get_declaration_string(arg), w)
            s = f"{arg_type} {arg.name}"
            if "unknown" in s:
                continue
            if "(" in s:
                self.warn(
                    1,
                    "ignoring method not supported by swig '%s::%s'."
                    % (typedef.name, method.name),
                    w,
                )
                return

            if (
                self.current_class is not None
                and not is_operator
                and method.name not in self.notWrappedMethods
                and method.access_type == "public"
            ):
                decls = pygccxml.declarations
                python_arg_type = get_arg_type(
                    decls, method.argument_types[argIndex], False
                )
                name = arg.name

                # prepend underscores to invalid keyword names
                if iskeyword(name):
                    name = "_" + name

                if arg.default_value:
                    # unknown value conversions can just be replaced with "..."
                    method_hints.append(
                        (
                            name,
                            python_arg_type,
                            convert_cpp_to_python_value(arg.default_value),
                        )
                    )
                else:
                    method_hints.append((name, python_arg_type, None))

            # append the default value if it exists
            if arg.default_value:
                s += f" = {arg.default_value}"
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

        method_definition = "    {}{} {}({}){};\n".format(
            static,
            self.get_alias(self.get_declaration_string(method.return_type), w),
            method.name,
            ", ".join(args),
            const,
        )
        self.outputFile.write(method_definition)

        if (
            self.current_class is not None
            and not is_operator
            and method.name not in self.notWrappedMethods
            and method.access_type == "public"
        ):
            # declare return type and save header changes to the class
            decls = pygccxml.declarations
            return_type = get_arg_type(decls, method.return_type, False)

            # last element in methods arg list contains special values
            method_hints.append((None, return_type, method.has_static))

            name = "__call__" if "operator()" == method.name else method.name

            if name not in self.classes[self.current_class].python_method_headers:
                self.classes[self.current_class].python_method_headers[name] = [
                    method_hints
                ]
            elif (
                method_hints
                not in self.classes[self.current_class].python_method_headers[name]
            ):
                self.classes[self.current_class].python_method_headers[name].append(
                    method_hints
                )

        # Check the method arguments for std::string passed by reference.
        # In this case, save the name of the argument in the apply_file_names list
        # for further usage.
        for arg in method.arguments:
            dtype = arg.decl_type
            if (
                pygccxml.declarations.is_reference(dtype)
                and pygccxml.declarations.is_const(
                    pygccxml.declarations.remove_reference(dtype)
                )
                is False
                and pygccxml.declarations.is_std_string(dtype)
            ):
                self.apply_file_names.append(arg.name)

    def generate_headerfile(self, idx_file, wrappers_namespace):
        # and begin to write the output
        header_file = StringIO()
        header_file.write("// This file is automatically generated.\n")
        header_file.write("// Do not modify this file manually.\n\n\n")

        langs = [
            # "CHICKEN",
            # "CSHARP",
            # "GUILE",
            # "JAVA",
            # "LUA",
            # "MODULA3",
            # "MZSCHEME",
            # "OCAML",
            # "PERL",
            # "PERL5",
            # "PHP",
            # "PHP4",
            # "PHP5",
            # "PIKE",
            "PYTHON",
            # "R",
            # "RUBY",
            # "SEXP",
            # "TCL",
            # "XML",
        ]

        # first, define the module
        # [1:-1] is there to drop the quotes
        for lang in langs:
            header_file.write(f"#ifdef SWIG{lang}\n")
            if lang == "PYTHON":
                header_file.write(
                    """
%include <pyabc.i>
%pythonbegin %{
import collections

from sys import version_info as _version_info
if _version_info < (3, 7, 0):
    raise RuntimeError("Python 3.7 or later required")

from . import _ITKCommonPython
%}

"""
                )
                # Also, release the GIL
                header_file.write(
                    f'%module(package="itk",threads="1") {self.submodule_name}{lang.title()}\n'
                )
                header_file.write('%feature("nothreadallow");\n')
                header_file.write('%feature("autodoc","2");\n')
            else:
                header_file.write(f"%module {self.submodule_name}{lang.title()}\n")
            header_file.write("#endif\n")
        header_file.write("\n")

        # add the includes
        # use a set to avoid putting many times the same include
        s = set()
        header_file.write("%{\n")
        # the include files passed in option
        include = self.submodule_name + "SwigInterface.h"
        i = f'#include "{include}"'
        if i not in s:
            header_file.write(i + "\n")
            s.add(i)
        header_file.write("%}\n\n\n")

        # load the aliases files
        header_file.write("%{\n")
        self.load_idx(idx_file)
        # and the idx files in the mdx ones
        for f in self.options.mdx:
            self.load_mdx(f)
        # iterate over all the typedefs in the _wrapping_::wrappers namespace
        # to fill the alias dict
        for typedef in wrappers_namespace.typedefs():  # allow_empty=True):
            s = get_type(typedef).decl_string
            # drop the :: prefix - it make swig produce invalid code
            if s.startswith("::"):
                s = s[2:]
            if s not in self.aliases:
                self.warn(
                    2,
                    f"{s} ({typedef.name}) should be already defined in the idx files.",
                )
                self.aliases[s] = typedef.name
                # declare the typedef
                header_file.write(f"typedef {s} {typedef.name};\n")

        header_file.write("%}\n\n\n")

        return header_file

    def generate_importfile(self, used_sources):
        # add the imports
        import_file = StringIO()
        for f in self.options.imports:
            import_file.write("%%import %s\n" % f)
        import_file.write("\n\n")

        for src in used_sources:
            import_file.write("%%import %s.i\n" % src)
        import_file.write("\n\n")
        return import_file

    def generate_includefile(self):
        # add the swig includes
        include_file = StringIO()
        include_file.write("%include itk.i\n")

        for f in self.options.swig_includes:
            include_file.write("%%include %s\n" % f)
        include_file.write("%%include %s\n" % (self.submodule_name + "_ext.i"))
        include_file.write("\n\n")
        return include_file

    def generate_applyfile(self):
        # When a std::string is passed by reference, we need to add the %apply
        # line with the argument name, and the INOUT command.
        # Use a set() to remove duplicates, this will work event if we got
        # multiple functions with the same argument name in the same .i file
        # (swig should take care of it).
        apply_file_names = set(self.apply_file_names)
        # Apply file, for passing std::string as reference in methods
        apply_file = StringIO()
        for name in apply_file_names:
            apply_file.write(
                "%apply (std::string& INOUT) { std::string & " + name + "};\n"
            )
        apply_file.write("\n\n")
        return apply_file

    def create_typedefheader(self, used_sources):
        # create the typedef header
        typedef_file = StringIO()
        typedef_file.write(f"#ifndef {self.submodule_name}SwigInterface_h\n")
        typedef_file.write(f"#define {self.submodule_name}SwigInterface_h\n")
        typedef_input = os.path.join(
            self.options.library_output_dir,
            "castxml_inputs",
            self.submodule_name + "SwigInterface.h.in",
        )
        with open(typedef_input) as f:
            typedef_file.write(f.read() + "\n")
        for src in used_sources:
            typedef_file.write(f'#include "{src}SwigInterface.h"\n')
        typedef_file.write("#endif\n")
        typedef_output = os.path.join(
            self.options.interface_output_dir, self.submodule_name + "SwigInterface.h"
        )
        with open(typedef_output, "w") as f:
            f.write(typedef_file.getvalue())

    def create_interfacefile(self, interface_file, idx_file, wrappers_namespace):
        header_file = self.generate_headerfile(idx_file, wrappers_namespace)

        # iterate over all the typedefs in the _wrapping_::wrappers namespace
        # to build a list of classes with the dependencies
        # classes :: [(name, [dep_name], typedef)]
        classes = []
        for typedef in wrappers_namespace.typedefs():
            # begin a new class
            if isinstance(
                get_type(typedef), pygccxml.declarations.class_declaration.class_t
            ):

                classes.append(
                    (
                        typedef.name,
                        [
                            self.get_alias(super_class.related_class.decl_string)
                            for super_class in get_type(typedef).bases
                        ],
                        typedef,
                    )
                )
            elif isinstance(
                get_type(typedef), pygccxml.declarations.enumeration.enumeration_t
            ):
                # warn( 6, "Enum are currently supported only nested in a
                # class." )
                self.generate_enum(typedef)
            else:
                self.warn(5, f"Unknown type type: {str(typedef.decl_type.declaration)}")

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
            self.current_class, typed = remove_class_type(
                typedef.name.replace("_Superclass", "")
            )
            # Skip wrapping for the following
            if typedef.name.endswith(
                ("_Pointer", "_AutoPointer", "_ConstPointer", "Factory")
            ) or self.current_class in ["stdcomplex", "stdnumeric_limits"]:
                self.current_class = None
            elif self.current_class not in self.classes:
                self.classes[self.current_class] = ITKClass(self.current_class)
                self.classes[self.current_class].typed = typed
                self.classes[self.current_class].submodule_name = self.submodule_name
                if typedef.name.endswith("Enums"):
                    self.classes[self.current_class].is_enum = True
                if typedef.name.endswith("_Superclass"):
                    self.classes[self.current_class].has_superclass = True

            # begin a new class
            self.generate_class(typedef)

            if (
                self.current_class is not None
                and self.classes[self.current_class].parent_class == []
                and (
                    not self.classes[self.current_class].has_superclass
                    or typedef.name.startswith("itk" + self.current_class)
                    and typedef.name.endswith("_Superclass")
                )
            ):

                for base in get_type(typedef).bases:
                    base = base.related_class.decl_string
                    if base.startswith("::"):
                        base = base[2:]
                    if "itk::" in base:
                        base = base[base.find("itk::") + len("itk::") :]

                    if "<" in base:
                        base = base[: base.find("<")]

                    # at this point if a : is in base it is std or some other class we have no reference to
                    # just ignore it
                    if ":" in base:
                        continue

                    if not self.classes[self.current_class].parent_class:
                        self.classes[self.current_class].parent_class.append(
                            f"_{base}Proxy"
                        )

                    else:
                        self.classes[self.current_class].parent_class.append(
                            f"_{base}Proxy"
                        )

            if (
                self.current_class is not None
                and get_type(typedef).is_abstract
                and not self.classes[self.current_class].is_abstract
            ):
                self.classes[self.current_class].is_abstract = True

        self.generate_process_object_snake_case_functions(typedefs)

        if len(self.warnings) > 0 and self.options.warningError:
            sys.exit(1)

        # search the files to import
        used_sources = set()
        for alias in self.usedTypes:
            if alias.rfind("Enums::") != -1:
                alias = alias[: alias.rfind("Enums::") + 5]
            if alias in self.typedefSource:
                idx_name = os.path.basename(self.typedefSource[alias])
                i_name = idx_name[: -len(".idx")]
                used_sources.add(i_name)
        output_file_name = os.path.basename(interface_file)
        if output_file_name in used_sources:
            used_sources.remove(output_file_name)

        import_file = self.generate_importfile(used_sources)
        include_file = self.generate_includefile()
        apply_file = self.generate_applyfile()

        self.create_typedefheader(used_sources)

        # finally, really write the output
        content = (
            header_file.getvalue()
            + import_file.getvalue()
            + include_file.getvalue()
            + apply_file.getvalue()
            + self.outputFile.getvalue()
        )

        if self.options.keep and os.path.exists(interface_file):
            with open(interface_file) as f:
                filecontent = f.read()

        if (
            self.options.keep
            and os.path.exists(interface_file)
            and filecontent == content
        ):
            self.info(f"{interface_file} unchanged.")
        else:
            self.info(f"Writing {interface_file}.")
            with open(interface_file, "w") as f:
                f.write(content)


def get_previous_content(file_path: Path) -> str:
    if file_path.is_file():
        # Avoid writing file if contents are not modified.  This is to prevent
        # unnecessary changing of file timestamps.
        with open(file_path, "r") as index_file:
            previous_file_contents: str = index_file.read()
            return previous_file_contents
    return "INVALID_CONTENT"


def generate_pyi_index_files(submodule: str, file_contents: StringIO, pkl_dir: str):
    # Compare index files with existing, and rewrite if non-existent or different.
    pkl_index_file_name: str = f"{submodule}.index.txt"
    file_path: Path = Path(pkl_dir) / pkl_index_file_name

    previous_file_contents: str = get_previous_content(file_path)
    if file_contents.getvalue() != previous_file_contents:
        with open(file_path, "w") as f:
            f.write(file_contents.getvalue())
    pyi_created_index_list.append(file_path)


def generate_wrapping_namespace(
    submodule_name, library_output_dir, pygccxml_config
) -> object:
    xml_file_path = os.path.join(
        library_output_dir, "castxml_inputs", submodule_name + ".xml"
    )
    pygccxml_reader = pygccxml.parser.source_reader.source_reader_t(pygccxml_config)
    abstract_syntax_tree = pygccxml_reader.read_xml_file(xml_file_path)
    global_namespace = pygccxml.declarations.get_global_namespace(abstract_syntax_tree)
    wrapping_namespace = global_namespace.namespace("_wrapping_")
    return wrapping_namespace.namespace("wrappers")


class GLBSubmoduleNamespaceCache(object):
    # Static variable for the get_submodule_namespace class, used as a cache

    def __init__(self):
        self.wrapping_namespaces_cache: Dict[str, Any] = dict()
        self.cache_hit_count: Dict[str, int] = collections.defaultdict(int)

    def get_submodule_namespace(
        self, submodule_name: str, library_output_dir: str, pygccxml_config
    ):
        if submodule_name in self.wrapping_namespaces_cache:
            wrappers_namespace = self.wrapping_namespaces_cache[submodule_name]
            # cache_hit_count is used to determine if there is any benefit to having this
            # performance cache mechanism.
            self.cache_hit_count[submodule_name] += 1
            if self.cache_hit_count[submodule_name] > 1:
                print(f"Speedup achieved: {self.cache_hit_count[submodule_name]}")
        else:
            wrappers_namespace = generate_wrapping_namespace(
                submodule_name, library_output_dir, pygccxml_config
            )
            max_cache_length: int = 200000  # used to be set to 20
            # Limit the number of cached, parsed abstract syntax trees to avoid very high memory usage
            if len(self.wrapping_namespaces_cache) < max_cache_length:
                self.wrapping_namespaces_cache[submodule_name] = wrappers_namespace
        return wrappers_namespace


global_submodule_cache = GLBSubmoduleNamespaceCache()


def generate_swig_input(
    submodule_name,
    pkl_dir,
    pygccxml_config,
    options,
    snake_case_process_object_functions,
) -> None:
    wrappers_namespace = global_submodule_cache.get_submodule_namespace(
        submodule_name, options.library_output_dir, pygccxml_config
    )

    swig_input_generator = SwigInputGenerator(submodule_name, options)
    idx_file_path: str = os.path.join(
        options.interface_output_dir, submodule_name + ".idx"
    )
    swig_input_file_path: str = os.path.join(
        options.interface_output_dir, submodule_name + ".i"
    )
    swig_input_generator.create_interfacefile(
        swig_input_file_path, idx_file_path, wrappers_namespace
    )
    snake_case_process_object_functions.update(
        swig_input_generator.snakeCaseProcessObjectFunctions
    )

    # Write index list of generated .pkl files
    index_file_contents: StringIO = StringIO()
    all_keys = swig_input_generator.classes.keys()
    if len(all_keys):
        for itk_class in all_keys:
            # Future problem will be that a few files will be empty
            # Can either somehow detect this or accept it
            # pickle class here
            class_name: str = swig_input_generator.classes[itk_class].class_name
            submodule_name: str = swig_input_generator.classes[itk_class].submodule_name
            pickled_filename: str = f"{pkl_dir}/{class_name}.{submodule_name}.pkl"

            # Only write to the pickle file if it does not match what is already saved.
            overwrite: bool = False
            pickle_exists: bool = exists(pickled_filename)
            if pickle_exists:
                with open(pickled_filename, "rb") as pickled_file:
                    existing_itk_class = pickle.load(pickled_file)
                    overwrite = not (
                        existing_itk_class == swig_input_generator.classes[itk_class]
                    )
            if overwrite or not pickle_exists:
                with open(pickled_filename, "wb") as pickled_file:
                    pickle.dump(swig_input_generator.classes[itk_class], pickled_file)

            index_file_contents.write(pickled_filename + "\n")
    else:
        # The following warning is useful for debugging, and eventually we
        # may wish to find a way to remove modules that are not currently part
        # of the build.  For example, currently all *.wrap files are processed and listed
        # as module dependencies. If FFTW is not enabled, that causes empty submodules
        # to be created as dependencies unnecessarily.
        # Changing that behavior will require structural code changes, or alternate
        # mechanisms to be implemented.
        if glb_options.debug_code:
            print(
                f"WARNING: {submodule_name} has no classes identified, but was listed as a dependent submodule."
            )
    generate_pyi_index_files(submodule_name, index_file_contents, pkl_dir)


def main():
    options = glb_options
    if options.pyi_dir == "":
        raise ValueError(f"Required directory missing '{options.pyi_dir}'")

    if options.pkl_dir == "":
        raise ValueError(f"Required directory missing '{options.pkl_dir}'")

    # Ensure that the requested stub file directory exists
    if options.pyi_dir != "":
        Path(options.pyi_dir).mkdir(exist_ok=True, parents=True)
    if options.pkl_dir != "":
        Path(options.pkl_dir).mkdir(exist_ok=True, parents=True)

    # init the pygccxml stuff
    pygccxml.utils.loggers.cxx_parser.setLevel(logging.CRITICAL)
    pygccxml.declarations.scopedef_t.RECURSIVE_DEFAULT = False
    pygccxml.declarations.scopedef_t.ALLOW_EMPTY_MDECL_WRAPPER = True

    pygccxml_config = pygccxml.parser.config.xml_generator_configuration_t(
        xml_generator_path=options.castxml_path, xml_generator="castxml"
    )

    submodule_names_list: List[str] = []
    # The first mdx file is the master index file for this module.
    master_mdx_filename: Path = Path(options.mdx[0])
    with open(master_mdx_filename) as ff:
        for line in ff.readlines():
            stripped = line.strip()
            if line.startswith("%") or line.isspace():
                # exclude the lines which are starting with % - that's not the idx files
                pass
            elif stripped.endswith(".mdx"):
                # exclude the lines which end with .mdx
                pass
            else:
                submodule_name = stripped.rsplit(".")[0]
                if submodule_name.startswith("(const char*)"):
                    submodule_name = submodule_name[len("(const char*)") :]
                submodule_name = submodule_name.strip('"')
                if submodule_name not in submodule_names_list:
                    submodule_names_list.append(submodule_name)

    for submodule_name in submodule_names_list:
        wrappers_namespace: Any = global_submodule_cache.get_submodule_namespace(
            submodule_name, options.library_output_dir, pygccxml_config
        )

        idx_file_path: str = os.path.join(
            options.interface_output_dir, submodule_name + ".idx"
        )
        idx_generator = IdxGenerator(submodule_name)
        idx_generator.create_idxfile(idx_file_path, wrappers_namespace)

    snake_case_process_object_functions = set()

    ordered_submodule_list: List[str] = []
    if options.submodule_order:
        all_submodules = options.submodule_order.split(";") + submodule_names_list
        for submodule_name in all_submodules:
            if submodule_name not in ordered_submodule_list:
                ordered_submodule_list.append(submodule_name)
    del submodule_names_list

    for submodule_name in ordered_submodule_list:
        generate_swig_input(
            submodule_name,
            options.pkl_dir,
            pygccxml_config,
            options,
            snake_case_process_object_functions,
        )

    snake_case_file = options.snake_case_file
    if len(snake_case_file) > 1:
        with open(snake_case_file, "w") as ff:
            ff.write("snake_case_functions = (")
            # Ensure that the functions are sorted alphabetically to ensure consistency
            # in the generated file structure.
            sorted_snake_case_process_object_functions = sorted(
                snake_case_process_object_functions
            )
            for function in sorted_snake_case_process_object_functions:
                ff.write("'" + function + "', ")
            ff.write(")\n")


if __name__ == "__main__":
    main()
    import sys

    if glb_options.debug_code:
        # Sanity Checks for required file generation
        # print("=====Correctly Required and created")
        # for m in pyi_approved_index_list:
        #     if m in pyi_created_index_list:
        #         print(f"GOOD: :{m}: Required and created")
        not_created: List[Path] = []
        for m in pyi_approved_index_list:
            if m not in pyi_created_index_list:
                not_created.append(m)
        if len(not_created) > 0:
            print("=====WRONG Required and *NOT* created")
            print(f"""cmdln: {" ".join(sys.argv)}""")
            [print(f"BAD: :{m}: Required not created") for m in not_created]

        extra_created: List[Path] = []
        for m in pyi_created_index_list:
            if m not in pyi_approved_index_list:
                extra_created.append(m)

        if len(extra_created) > 0:
            print("=====WRONG *NOT * Required and created")
            [print(f"BAD: :{m}: *NOT* Required and created") for m in extra_created]
