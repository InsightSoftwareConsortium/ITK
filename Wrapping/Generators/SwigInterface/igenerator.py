#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import os
import re
from argparse import ArgumentParser
from io import StringIO
from pathlib import Path
from keyword import iskeyword
from typing import List


class ITKClass:
    def __init__(self, class_name):
        self.python_method_headers = {}
        self.has_new_method = False
        self.number_of_typedefs = 0
        self.typed = False
        self.parent_class = ""
        self.is_abstract = False
        self.class_name = class_name
        self.is_enum = False
        self.has_superclass = False
        self.enums = []
        self.submodule_name = ""


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
            "[A-Z_0-9]+(Lanczos|Cosine|Welch|Hamming|Neighborhood)*[A-Z_0-9]*$",
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


def generate_class_pyi_def(
    outputPYIHeaderFile, outputPYIMethodFile, itk_class: ITKClass
):
    class_name = itk_class.class_name

    if itk_class.is_enum:
        outputPYIHeaderFile.write(f"itk.{class_name} =  _{class_name}Proxy\n\n\n")
    elif not itk_class.is_abstract:
        outputPYIHeaderFile.write(
            generate_class_pyi_header(
                class_name, itk_class.has_new_method, itk_class.typed
            )
        )

    outputPYIMethodFile.write(
        f"class _{class_name}Proxy({itk_class.parent_class}):\n"  # if
    )

    if itk_class.is_enum and len(itk_class.enums) > 0:
        for enum in itk_class.enums:
            outputPYIMethodFile.write(f"\t{enum} = ...\n")
        outputPYIMethodFile.write(f"\n\n")
    else:
        if len(itk_class.python_method_headers.keys()) == 0:
            outputPYIMethodFile.write(f"\t...\n\n")

        for method in itk_class.python_method_headers.keys():
            is_overloaded = len(itk_class.python_method_headers[method]) > 1
            for method_variation in itk_class.python_method_headers[method]:
                method_name = method
                attributes = method_variation
                params = ""
                return_type = attributes[len(attributes) - 1][1]

                is_a_static_method: bool = attributes[len(attributes) - 1][2]

                # remove special case containing the return type from the end of the attributes list
                attributes = attributes[:-1]
                if len(attributes) > 0:
                    params += ", "

                for attribute in attributes:
                    # name of the argument
                    params += attribute[0]

                    # type of the argument
                    if attribute[1] is None:
                        pass
                    else:
                        params += f": {attribute[1]}"

                    # default value of the argument (if any)
                    if attribute[2] is not None:
                        params += f" = {attribute[2]}, "
                    else:
                        params += ", "

                params = params[:-2]  # remove extra comma

                self_str = "self"
                if is_a_static_method:
                    outputPYIMethodFile.write("\t@staticmethod\n")
                    self_str = ""
                    params = params[2:]  # remove comma from beginning

                if is_overloaded:
                    outputPYIMethodFile.write("\t@overload\n")

                outputPYIMethodFile.write(f"\tdef {method_name}({self_str}{params})")
                if return_type is not None:
                    outputPYIMethodFile.write(f" -> {return_type}")
                outputPYIMethodFile.write(f":\n" f'\t\t""""""\n' f"\t\t...\n\n")


not_callable = [
    "ImageToImageMetricv4",
    "NarrowBandLevelSetImageFilter",
    "ShapePriorSegmentationLevelSetImageFilter",
    "ImageToImageFilter",
    "ImageSource",
    "NarrowBandImageFilterBase",
    "NoiseBaseImageFilter",
    "ImageToMeshFilter",
    "ImageToPathFilter",
    "GenerateImageSource",
    "DenseFiniteDifferenceImageFilter",
    "InPlaceImageFilter",
    "AnisotropicDiffusionImageFilter",  # you must use snake_case method to properly use this class
    "ConvolutionImageFilterBase",
    "SparseFieldFourthOrderLevelSetImageFilter",
    "SegmentationLevelSetImageFilter",
    "TemporalProcessObject",
    "ProcessObject",
    "SegmentationLevelSetFunction",
    "LBFGSOptimizerBasev4",
]


def generate_class_pyi_header(class_name: str, has_new_method: bool, typed: bool):
    """Return a string containing the definition of a pyi class header.
    Supports both typed and non-typed classes."""
    if not typed:
        if has_new_method:
            class_header = (
                f"class _{class_name}Template(_itkTemplate):\n"
                f'    """Interface for instantiating itk::{class_name}'
                f"\n        Create a new {class_name} Object:\n"
                f"            'itk.{class_name}.New(**kwargs)"
                f'"""\n\n'
                f"    @staticmethod\n"
                f"    def New(**kwargs) -> _{class_name}Proxy:\n"
                f'        """Instantiate itk::{class_name}"""\n'
                f"        ...\n"
                f"\n"
                f"{class_name} = _{class_name}Template\n"
                f"\n"
                f"\n"
            )
        elif class_name not in not_callable:
            class_header = (
                f"class _{class_name}Template(_itkTemplate):\n"
                f'    """Interface for instantiating itk::{class_name}'
                f'"""\n\n'
                f"    def __new__(cls, *args: Any) -> _{class_name}Proxy:\n"
                f'        """Instantiate itk::{class_name}"""\n'
                f"        ...\n\n"
                f"    def __call__(self, *args: Any) -> _{class_name}Proxy:\n"
                f'        """Instantiate itk::{class_name}"""\n'
                f"        ...\n"
                f"\n"
                f"{class_name} = _{class_name}Template"
                f"\n"
                f"\n"
            )
        else:
            class_header = (
                f"class _{class_name}Template(_itkTemplate):\n"
                f'    """Interface for instantiating itk::{class_name}"""\n'
                f"    ...\n"
                f"\n"
                f"{class_name} = _{class_name}Template"
                f"\n"
                f"\n"
            )
        return class_header

    types = "INSERT_TYPE_NAMES_HERE"
    if has_new_method:
        class_header = (
            f"class _{class_name}TemplateGetter():\n"
            f"    def __getitem__(self, parameters) -> _{class_name}Template:\n"
            f'        """Specify class type with:\n'
            f"            \t[{types}]\n"
            f"            :return: {class_name}Template\n"
            f'            """\n'
            f"        ...\n"
            f"\n"
            f"\n"
            f"class _{class_name}Template(_itkTemplate, metaclass=_{class_name}TemplateGetter):\n"
            f'    """Interface for instantiating itk::{class_name}< {types} >\n'
            f"        Create a new {class_name} Object (of default type):\n"
            f"            'itk.{class_name}.New(**kwargs)\n"
            f"        Supports type specification through dictionary access:\n"
            f'            \'itk.{class_name}[{types}].New(**kwargs)"""\n'
            f"\n"
            f"    @staticmethod\n"
            f"    def New(**kwargs) -> _{class_name}Proxy:\n"
            f'        """Instantiate itk::{class_name}< {types} >"""\n'
            f"        ...\n"
            f"\n"
            f"{class_name} = _{class_name}Template"
            f"\n"
            f"\n"
        )
    elif class_name not in not_callable:
        class_header = (
            f"class _{class_name}TemplateGetter():\n"
            f"    def __getitem__(self, parameters) -> _{class_name}Template:\n"
            f'        """Specify class type with:\n'
            f"            \t[{types}]\n"
            f"            :return: {class_name}Template\n"
            f'            """\n'
            f"        ...\n"
            f"\n"
            f"\n"
            f"class _{class_name}Template(_itkTemplate, metaclass=_{class_name}TemplateGetter):\n"
            f'    """Interface for instantiating itk::{class_name}< {types} >\n'
            f"        Supports type specification through dictionary access:\n"
            f'            \'itk.{class_name}[{types}]()"""\n'
            f"\n"
            f"    def __new__(cls, *args: Any) -> _{class_name}Proxy:\n"
            f'        """Instantiate itk::{class_name}< {types} >"""\n'
            f"        ...\n\n"
            f"    def __call__(self, *args: Any) -> _{class_name}Proxy:\n"
            f'        """Instantiate itk::{class_name}< {types} >"""\n'
            f"        ...\n"
            f"\n"
            f"{class_name} = _{class_name}Template"
            f"\n"
            f"\n"
        )
    else:
        class_header = (
            f"class _{class_name}TemplateGetter():\n"
            f"    def __getitem__(self, parameters) -> _{class_name}Template:\n"
            f'        """Specify class type with:\n'
            f"            \t[{types}]\n"
            f"            :return: {class_name}Template\n"
            f'            """\n'
            f"        ...\n"
            f"\n"
            f"\n"
            f"class _{class_name}Template(_itkTemplate, metaclass=_{class_name}TemplateGetter):\n"
            f'    """Interface for instantiating itk::{class_name}< {types} >"""\n'
            f"    ...\n"
            f"\n"
            f"{class_name} = _{class_name}Template"
            f"\n"
            f"\n"
        )
    return class_header


def getType(v):
    if hasattr(v, "decl_type"):
        return getType(v.decl_type)
    if hasattr(v, "declaration"):
        return getType(v.declaration)
    return v


class IdxGenerator(object):
    """Generates a the .idx file for an ITK wrapping submodule (which usually
    corresponds to a class)."""

    def __init__(self, submoduleName):
        self.submoduleName = submoduleName
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
            self.outputFile.write("{%s} {%s} {%s}\n" % (s, n, self.submoduleName))

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

    def __init__(self, submoduleName, options, classes):
        self.submoduleName = submoduleName
        # The first mdx file is the master index file for this module.
        self.moduleName = Path(options.mdx[0]).stem
        self.options = options

        self.outputFile = StringIO()
        self.applyFileNames = []

        # A dict of sets containing the .pyi python equivalent for all class methods and params
        self.classes = classes
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

    def warn(self, identifier, msg, doWarn=True):
        if not doWarn:
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
                        f"{self.submoduleName}: error({str(identifier)}): {msg}",
                        file=sys.stderr,
                    )
                else:
                    print(
                        f"{self.submoduleName}: warning({str(identifier)}): {msg}",
                        file=sys.stderr,
                    )

    def info(self, msg):
        if self.verbose:
            print(f"info: {msg}", file=sys.stderr)

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
        if s.startswith("std::") and pygccxml.declarations.templates.is_instantiation(
            s
        ):
            args = []
            for arg in pygccxml.declarations.templates.args(s):
                t, d = SwigInputGenerator.type_and_decorators(arg)
                args.append(self.renameTypesInSTL(self.get_alias(t)) + d)
            return (
                pygccxml.declarations.templates.join(
                    pygccxml.declarations.templates.name(s), args
                )
                + SwigInputGenerator.type_and_decorators(s)[1]
            )
        return s

    @staticmethod
    def removeStdAllocator(s):
        if pygccxml.declarations.templates.is_instantiation(s):
            args = []
            for arg in pygccxml.declarations.templates.args(s):
                if not arg.startswith("std::allocator"):
                    t, d = SwigInputGenerator.type_and_decorators(arg)
                    args.append(SwigInputGenerator.removeStdAllocator(t) + d)
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
        needToContinue = True
        while needToContinue:
            needToContinue = False
            for e in ends:
                if s.endswith(e):
                    end = e + end
                    s = s[: -len(e)]
                    needToContinue = True
        return (s, end)

    _firstCapRE = re.compile(r"(.)([A-Z][a-z]+)")
    _allCapRE = re.compile("([a-z0-9])([A-Z])")

    @staticmethod
    def camel_case_to_snake_case(camelCase):
        substitution = SwigInputGenerator._firstCapRE.sub(r"\1_\2", camelCase)
        return (
            SwigInputGenerator._allCapRE.sub(r"\1_\2", substitution)
            .lower()
            .replace("__", "_")
        )

    @staticmethod
    def kwarg_of_interest(member_name):
        """
        This function accepts a member function name and returns whether we
        want to list it explicitly in the ProcessObject functional interface
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
        s = self.renameTypesInSTL(s)

        # drop the allocator part of the type, because it is not supported by the
        # %template directive with some generators (like tcl)
        s = SwigInputGenerator.removeStdAllocator(s)

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
        with open(file_name, "r") as f:
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
                    # If the full_name key alreay exists, do not overwrite the
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
        with open(file_name, "r") as f:
            lines = f.readlines()
        for line in lines:
            line_stripped = line.strip()
            if line.startswith("%") or line.isspace():
                # exclude the lines which are starting with % - that's not the idx
                # files
                pass
            elif line_stripped.endswith(".mdx"):
                self.load_mdx(os.path.dirname(file_name) + os.sep + line_stripped)
            elif line_stripped[:-4] == self.submoduleName:
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
            for member in getType(typedef).get_members(
                access=decls.ACCESS_TYPES.PUBLIC
            ):
                if (
                    isinstance(member, decls.member_function_t)
                    and member.name == "New"
                    and not typedef.name == "itkLightObject"
                ):
                    if self.current_class is not None:
                        self.classes[self.current_class].has_new_method = True
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
            for super_class in getType(typedef).bases:
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
            self.outputFile.write("class %s%s {\n" % (typedef.name, s))

            # iterate over access
            for access in decls.ACCESS_TYPES.ALL:

                # the access type
                self.outputFile.write("  " * indent)
                self.outputFile.write(f"  {access}:\n")

                # warnings or no warning?
                w = access not in self.options.access_warnings

                # iterate over the members
                for member in getType(typedef).get_members(access=access):
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
            self.outputFile.write("class %s%s {\n" % (typedef.name, s))

            # iterate over access
            for access in pygccxml.declarations.ACCESS_TYPES.ALL:

                # the access type
                self.outputFile.write("  " * indent)
                self.outputFile.write(f"  {access}:\n")

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

    def generate_process_object_snake_case_functions(self, typedefs):
        self.info("Generating snake case functions")
        processObjects = set()
        for typedef in typedefs:
            classType = getType(typedef)
            bases = [base.related_class.name for base in classType.recursive_bases]
            isProcessObject = "ProcessObject" in bases and not classType.is_abstract
            short_name = classType.name.split("<")[0]
            if isProcessObject or short_name in self.forceSnakeCase:
                processObjects.add((short_name, classType))
        if len(processObjects) > 0:
            self.outputFile.write("\n\n#ifdef SWIGPYTHON\n")
            self.outputFile.write("%pythoncode %{\n")
            for processObject, classType in processObjects:
                snakeCase = self.camel_case_to_snake_case(processObject)
                if snakeCase in self.snakeCaseProcessObjectFunctions:
                    continue
                self.snakeCaseProcessObjectFunctions.add(snakeCase)
                decls = pygccxml.declarations
                recursive_bases = classType.recursive_bases
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
                for member in classType.get_members(access=decls.ACCESS_TYPES.PUBLIC):
                    if isinstance(
                        member, decls.member_function_t
                    ) and self.kwarg_of_interest(member.name):
                        if len(member.argument_types) > 0:
                            arg_type = member.argument_types[0]
                            if member.name in kwargs_of_interest:
                                kwargs_of_interest[member.name].add(arg_type)
                            else:
                                kwargs_of_interest[member.name] = set([arg_type])
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
                                    kwargs_of_interest[member.name] = set([arg_type])
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
def {snakeCase}(*args{args_typehint}, {kwargs_typehints}**kwargs){return_typehint}:
    \"\"\"Functional interface for {processObject}\"\"\"
    import itk

    kwarg_typehints = {{ {kwarg_dict} }}
    specified_kwarg_typehints = {{ k:v for (k,v) in kwarg_typehints.items() if kwarg_typehints[k] is not ... }}
    kwargs.update(specified_kwarg_typehints)

    instance = itk.{processObject}.New(*args, **kwargs)
    return instance.__internal_call__()

def {snakeCase}_init_docstring():
    import itk
    from itk.support import template_class

    filter_class = itk.{self.moduleName}.{processObject}
    {snakeCase}.process_object = filter_class
    is_template = isinstance(filter_class, template_class.itkTemplate)
    if is_template:
        filter_object = filter_class.values()[0]
    else:
        filter_object = filter_class

    {snakeCase}.__doc__ = filter_object.__doc__

"""
                )
            self.outputFile.write("%}\n")
            self.outputFile.write("#endif\n")

    def generate_constructor(self, typedef, constructor, indent, w):
        # iterate over the arguments
        args = []
        for arg in constructor.arguments:
            s = f"{self.get_alias(self.getDeclarationString(arg), w)} {arg.name}"
            if "unknown" in s:
                continue
            # append the default value if it exists
            if arg.default_value:
                s += f" = {arg.default_value}"
            # and add the string to the arg list
            args.append(s)
        self.outputFile.write("  " * indent)
        self.outputFile.write(f"    {typedef.name}({', '.join(args)});\n")

    def generate_destructor(self, typedef, destructor, indent, w):
        self.outputFile.write("  " * indent)
        self.outputFile.write(f"    ~{typedef.name}();\n")

    def generate_enum(self, typedef):
        name = typedef.name
        enum = getType(typedef)
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
            "enum class %s: uint8_t { %s };\n\n" % (name, ", ".join(content))
        )

    def generate_nested_enum(self, typedef, enum, indent, w):
        content = [f" {key}" for key, value in enum.values]
        self.outputFile.write("  " * indent)
        self.outputFile.write(
            "    enum class %s: uint8_t { %s };\n\n" % (enum.name, ", ".join(content))
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

        if "(" in getType(method.return_type).decl_string:
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
            arg_type = self.get_alias(self.getDeclarationString(arg), w)
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

        method_definition = "    %s%s %s(%s)%s;\n" % (
            static,
            self.get_alias(self.getDeclarationString(method.return_type), w),
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
        # In this case, save the name of the argument in the applyFileNames list
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
                self.applyFileNames.append(arg.name)

    def generate_headerfile(self, idxFile, wrappersNamespace):
        # and begin to write the output
        headerFile = StringIO()
        headerFile.write("// This file is automatically generated.\n")
        headerFile.write("// Do not modify this file manually.\n\n\n")

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
            headerFile.write(f"#ifdef SWIG{lang}\n")
            if lang == "PYTHON":
                headerFile.write(
                    """
%include <pyabc.i>
%pythonbegin %{
import collections

from sys import version_info as _version_info
if _version_info < (3, 7, 0):
    raise RuntimeError("Python 3.7 or later required")
%}
"""
                )
                # Also, release the GIL
                headerFile.write(
                    f'%module(package="itk",threads="1") {self.submoduleName}{lang.title()}\n'
                )
                headerFile.write('%feature("nothreadallow");\n')
                headerFile.write('%feature("autodoc","2");\n')
            else:
                headerFile.write(f"%module {self.submoduleName}{lang.title()}\n")
            headerFile.write("#endif\n")
        headerFile.write("\n")

        # add the includes
        # use a set to avoid putting many times the same include
        s = set()
        headerFile.write("%{\n")
        # the include files passed in option
        include = self.submoduleName + "SwigInterface.h"
        i = f'#include "{include}"'
        if i not in s:
            headerFile.write(i + "\n")
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
                    2,
                    f"{s} ({typedef.name}) should be already defined in the idx files.",
                )
                self.aliases[s] = typedef.name
                # declare the typedef
                headerFile.write(f"typedef {s} {typedef.name};\n")

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
        importFile.write("\n\n")
        return importFile

    def generate_includefile(self):
        # add the swig includes
        includeFile = StringIO()
        includeFile.write("%include itk.i\n")
        for f in options.swig_includes:
            includeFile.write("%%include %s\n" % f)
        includeFile.write("%%include %s\n" % (self.submoduleName + "_ext.i"))
        includeFile.write("\n\n")
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
                "%apply (std::string& INOUT) { std::string & " + name + "};\n"
            )
        applyFile.write("\n\n")
        return applyFile

    def create_typedefheader(self, usedSources):
        # create the typedef header
        typedefFile = StringIO()
        typedefFile.write(f"#ifndef {self.submoduleName}SwigInterface_h\n")
        typedefFile.write(f"#define {self.submoduleName}SwigInterface_h\n")
        typedefInput = os.path.join(
            options.library_output_dir, self.submoduleName + "SwigInterface.h.in"
        )
        with open(typedefInput, "r") as f:
            typedefFile.write(f.read() + "\n")
        for src in usedSources:
            typedefFile.write(f'#include "{src}SwigInterface.h"\n')
        typedefFile.write("#endif\n")
        typedefOutput = os.path.join(
            options.interface_output_dir, self.submoduleName + "SwigInterface.h"
        )
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
                getType(typedef), pygccxml.declarations.class_declaration.class_t
            ):

                classes.append(
                    (
                        typedef.name,
                        [
                            self.get_alias(super_class.related_class.decl_string)
                            for super_class in getType(typedef).bases
                        ],
                        typedef,
                    )
                )
            elif isinstance(
                getType(typedef), pygccxml.declarations.enumeration.enumeration_t
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
            if (
                typedef.name.endswith(
                    (
                        "_Pointer",
                        "_AutoPointer",
                        "_ConstPointer",
                        "Factory",
                    )
                )
                or self.current_class in ["stdcomplex", "stdnumeric_limits"]
            ):
                self.current_class = None
            elif self.current_class not in self.classes:
                self.classes[self.current_class] = ITKClass(self.current_class)
                self.classes[self.current_class].typed = typed
                self.classes[self.current_class].submodule_name = self.submoduleName
                if typedef.name.endswith("Enums"):
                    self.classes[self.current_class].is_enum = True
                if typedef.name.endswith("_Superclass"):
                    self.classes[self.current_class].has_superclass = True

            # begin a new class
            self.generate_class(typedef)

            if (
                self.current_class is not None
                and self.classes[self.current_class].parent_class == ""
                and (
                    not self.classes[self.current_class].has_superclass
                    or typedef.name.startswith("itk" + self.current_class)
                    and typedef.name.endswith("_Superclass")
                )
            ):

                for base in getType(typedef).bases:
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

                    if self.classes[self.current_class].parent_class == "":
                        self.classes[self.current_class].parent_class = f"_{base}Proxy"

                    else:
                        self.classes[
                            self.current_class
                        ].parent_class += f", _{base}Proxy"

            if (
                self.current_class is not None
                and getType(typedef).is_abstract
                and not self.classes[self.current_class].is_abstract
            ):
                self.classes[self.current_class].is_abstract = True

        self.generate_process_object_snake_case_functions(typedefs)

        if len(self.warnings) > 0 and self.options.warningError:
            sys.exit(1)

        # search the files to import
        usedSources = set()
        for alias in self.usedTypes:
            if alias.rfind("Enums::") != -1:
                alias = alias[: alias.rfind("Enums::") + 5]
            if alias in self.typedefSource:
                idxName = os.path.basename(self.typedefSource[alias])
                iName = idxName[: -len(".idx")]
                usedSources.add(iName)
        outputFileName = os.path.basename(interfaceFile)
        if outputFileName in usedSources:
            usedSources.remove(outputFileName)

        importFile = self.generate_importfile(usedSources)
        includeFile = self.generate_includefile()
        applyFile = self.generate_applyfile()

        self.create_typedefheader(usedSources)

        # finally, really write the output
        content = (
            headerFile.getvalue()
            + importFile.getvalue()
            + includeFile.getvalue()
            + applyFile.getvalue()
            + self.outputFile.getvalue()
        )

        if self.options.keep and os.path.exists(interfaceFile):
            with open(interfaceFile, "r") as f:
                filecontent = f.read()

        if (
            self.options.keep
            and os.path.exists(interfaceFile)
            and filecontent == content
        ):
            self.info(f"{interfaceFile} unchanged.")
        else:
            self.info(f"Writing {interfaceFile}.")
            with open(interfaceFile, "w") as f:
                f.write(content)


def init_submodule_pyi_file(pyiFile: Path, submodule_name: str) -> None:
    with open(pyiFile, "w") as pyiFile:
        pyiFile.write(
            f"""# Interface and Interface methods for submodule: {submodule_name}
from typing import Union, Any
# additional imports
from .support.template_class import itkTemplate as _itkTemplate

\n"""
        )


def write_class_pyi(
    pyiFile: Path, class_name: str, header_code: str, interfaces_code: str
) -> None:
    # Write interface files to the stub directory to support editor autocompletions
    with open(pyiFile, "a+") as pyiFile:
        pyiFile.write(f"# Interface for class: {class_name}\n")
        pyiFile.write(header_code)
        pyiFile.write(f"# Interface methods for class: {class_name}\n")
        pyiFile.write(interfaces_code)


def write_common_init_interface_file(interface_dir_path: Path) -> None:
    # This code may still be problematic due to possible parallel runs overwriting
    # the file each time.  As long as the last write is successful, this code
    # works.  Initial testing demonstrated that igenerator.py was never run
    # in parallel, so that also makes this OK.
    with open(interface_dir_path / "__init__.pyi", "w") as pyiIndexFile:
        # This is a bit of a hack to re-write this file every time
        # a new .pyi file is written.
        # Keeping track of these generated files from CMake is
        # very convoluted, and the cmake custom command dependencies
        # caused circular dependencies.
        #
        # This __init__.pyi file needs a small preamble
        # and then import all the auto-generated .pyi files
        # statically.
        preamble = """
# Create/Clear .pyi file and add boilerplate import content
# Python Header Interface File
# imports as they appear in __init__.py
from typing import Union, Any
from itkConfig import ITK_GLOBAL_VERSION_STRING as __version__Ï

## Must import using relative paths so that symbols are imported successfully
from .support.extras import *
from .support.types import *

### All items below are written out statically during

"""
        pyiIndexFile.write(preamble)

        # If we could keep track of the interface files in CMake, we could avoid
        # rewriting the __init__.pyi file every time a new version is created.
        for interface_file in Path(f"{interface_dir_path}").glob(f"i*.pyi"):
            pyiIndexFile.write(
                f"from .{interface_file.name.replace('.pyi', '')} import *\n"
            )


if __name__ == "__main__":
    argParser = ArgumentParser()
    argParser.add_argument(
        "--mdx",
        action="append",
        dest="mdx",
        default=[],
        metavar="FILE",
        help="master idx file to be used.",
    )
    argParser.add_argument(
        "--import",
        action="append",
        dest="imports",
        default=[],
        metavar="FILE",
        help="File to be imported in the generated interface file.",
    )
    argParser.add_argument(
        "--swig-include",
        action="append",
        dest="swig_includes",
        default=[],
        metavar="FILE",
        help=(
            "File to be included by swig (%include) in the generated " "interface file."
        ),
    )
    argParser.add_argument(
        "-w",
        "--disable-warning",
        action="append",
        dest="warnings",
        default=[],
        metavar="WARNING",
        help="Warning to be disabled.",
    )
    argParser.add_argument(
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
    argParser.add_argument(
        "-W",
        "--warning-error",
        action="store_true",
        dest="warningError",
        help="Treat warnings as errors.",
    )
    argParser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        dest="verbose",
        help="Log what is currently done.",
    )
    argParser.add_argument(
        "-k",
        "--keep",
        action="store_true",
        dest="keep",
        help="Don't rewrite the output file if the content is unchanged.",
    )
    argParser.add_argument(
        "-p",
        "--pygccxml-path",
        action="store",
        dest="pygccxml_path",
        help="Path to pygccxml",
    )
    argParser.add_argument(
        "-g",
        "--castxml-path",
        action="store",
        dest="castxml_path",
        help="Path to castxml",
    )
    argParser.add_argument(
        "-o",
        "--interface-output-dir",
        action="store",
        dest="interface_output_dir",
        help="Directory to write the Swig input files",
    )
    argParser.add_argument(
        "-l",
        "--library-output-dir",
        action="store",
        dest="library_output_dir",
        help="Directory to read the xml abstract syntax tree input files",
    )
    argParser.add_argument(
        "-s",
        "--submodule-order",
        action="store",
        dest="submodule_order",
        help="List of submodules that must be wrapped in the given order",
    )
    argParser.add_argument(
        "-a",
        "--snake-case-file",
        action="store",
        dest="snake_case_file",
        help="The configuration file to be appended to if snake_case_functions are found",
    )

    argParser.add_argument(
        "--pyi_dir",
        action="store",
        dest="pyi_dir",
        default="",
        type=str,
        help="The directory for .pyi files to be generated",
    )

    options = argParser.parse_args()

    # Ensure that the requested stub file directory exists
    if options.pyi_dir != "":
        Path(options.pyi_dir).mkdir(exist_ok=True, parents=True)

    sys.path.insert(1, options.pygccxml_path)
    import pygccxml
    import logging

    # init the pygccxml stuff
    pygccxml.utils.loggers.cxx_parser.setLevel(logging.CRITICAL)
    pygccxml.declarations.scopedef_t.RECURSIVE_DEFAULT = False
    pygccxml.declarations.scopedef_t.ALLOW_EMPTY_MDECL_WRAPPER = True

    pygccxml_config = pygccxml.parser.config.xml_generator_configuration_t(
        xml_generator_path=options.castxml_path, xml_generator="castxml"
    )

    submoduleNames = []
    # The first mdx file is the master index file for this module.
    with open(options.mdx[0], "r") as ff:
        for line in ff.readlines():
            stripped = line.strip()
            if line.startswith("%") or line.isspace():
                # exclude the lines which are starting with % - that's not the idx
                # files
                pass
            elif stripped.endswith(".mdx"):
                pass
            else:
                submoduleName = stripped.rsplit(".")[0]
                if submoduleName.startswith("(const char*)"):
                    submoduleName = submoduleName[len("(const char*)") :]
                submoduleName = submoduleName.strip('"')
                submoduleNames.append(submoduleName)

    def generate_wrapping_namespace(submoduleName):
        xmlFilePath = os.path.join(options.library_output_dir, submoduleName + ".xml")
        pygccxml_reader = pygccxml.parser.source_reader.source_reader_t(pygccxml_config)
        abstractSyntaxTree = pygccxml_reader.read_xml_file(xmlFilePath)
        globalNamespace = pygccxml.declarations.get_global_namespace(abstractSyntaxTree)
        wrappingNamespace = globalNamespace.namespace("_wrapping_")
        return wrappingNamespace.namespace("wrappers")

    wrappingNamespaces = dict()
    # Limit the number of cached, parsed abstract syntax trees to avoid very
    # high memory usage
    wrappingCacheLength = min(len(submoduleNames), 20)
    for ii in range(wrappingCacheLength):
        submoduleName = submoduleNames[ii]
        wrappingNamespace = generate_wrapping_namespace(submoduleName)
        wrappingNamespaces[submoduleName] = wrappingNamespace

    for submoduleName in submoduleNames:
        if submoduleName in wrappingNamespaces:
            wrappersNamespace = wrappingNamespaces[submoduleName]
        else:
            wrappersNamespace = generate_wrapping_namespace(submoduleName)

        idxFilePath = os.path.join(options.interface_output_dir, submoduleName + ".idx")
        idx_generator = IdxGenerator(submoduleName)
        idx_generator.create_idxfile(idxFilePath, wrappersNamespace)

    snake_case_process_object_functions = set()

    def generate_swig_input(submoduleName, classes):
        if submoduleName in wrappingNamespaces:
            wrappersNamespace = wrappingNamespaces[submoduleName]
        else:
            wrappersNamespace = generate_wrapping_namespace(submoduleName)

        idxFilePath = os.path.join(options.interface_output_dir, submoduleName + ".idx")
        swigInputFilePath = os.path.join(
            options.interface_output_dir, submoduleName + ".i"
        )

        swig_input_generator = SwigInputGenerator(submoduleName, options, classes)
        swig_input_generator.create_interfacefile(
            swigInputFilePath, idxFilePath, wrappersNamespace
        )
        snake_case_process_object_functions.update(
            swig_input_generator.snakeCaseProcessObjectFunctions
        )

    classes = {}

    ordered_submodule_list: List[str] = []
    if options.submodule_order:
        for submoduleName in options.submodule_order.split(";"):
            if submoduleName not in ordered_submodule_list:
                ordered_submodule_list.append(submoduleName)
        for submoduleName in submoduleNames:
            if submoduleName not in ordered_submodule_list:
                ordered_submodule_list.append(submoduleName)
    del submoduleNames

    for submoduleName in ordered_submodule_list:
        generate_swig_input(submoduleName, classes)
        if options.pyi_dir != "":
          init_submodule_pyi_file(
            Path(f"{options.pyi_dir}/{submoduleName}.pyi"), submoduleName
          )

    if options.pyi_dir != "":
      for itk_class in classes.keys():
        outputPYIHeaderFile = StringIO()
        outputPYIMethodFile = StringIO()
        generate_class_pyi_def(
            outputPYIHeaderFile, outputPYIMethodFile, classes[itk_class]
        )

        write_class_pyi(
            Path(f"{options.pyi_dir}/{classes[itk_class].submodule_name}.pyi"),
            itk_class,
            outputPYIHeaderFile.getvalue(),
            outputPYIMethodFile.getvalue(),
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
