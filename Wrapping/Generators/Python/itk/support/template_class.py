# ==========================================================================
#
#   Copyright NumFOCUS
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          https://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
# ==========================================================================*/

import inspect
import os
import re
import sys
import types
import collections
import warnings
from typing import Dict, Any, List, Callable, Union

import itkConfig

from itk.support import base
from itk.support.extras import output
from itk.support.types import itkCType
import math
from collections.abc import Mapping

# Needed to avoid problem with aliasing of itk.set (itkTemplate)
# inside the itk namespace.  We need to explicitly specify the
# use of the builtin set
from builtins import set as _builtin_set

# A valid type for holding swig classes and functions
_SWIG_CALLABLE_TYPE = Callable[..., Any]


class itkTemplateBase:
    """This itkTemplateBase class has only static and class methods used
    to manage the singleton instances of template objects.
    """

    #
    # 'itk::FixedArray<unsignedint,2>' = {type} <class 'itk.itkFixedArrayPython.itkFixedArrayUI2'>
    #          thisown = {property} <property object at 0x7ff800995710>
    __template_instantiations_name_to_object__: Dict[
        str, _SWIG_CALLABLE_TYPE
    ] = collections.OrderedDict()

    #
    # __template_instantiations_name_to_object__ = {dict}
    #          <class 'itk.itkFixedArrayPython.itkFixedArrayF2'> = {tuple}
    #               0 = {itkTemplate} <itkTemplate itk::FixedArray>
    #               1 = {tuple} (<itkCType float>, 2)
    __template_instantiations_object_to_name__: Dict[
        _SWIG_CALLABLE_TYPE, "itkTemplate"
    ] = {}

    # __named_template_registry__ = {dict}
    #    'std::list' = {itkTemplate}
    #                 B = {type} <class 'itk.pyBasePython.listB'>
    #                 D = {type} <class 'itk.pyBasePython.listD'>
    #                 F = {type} <class 'itk.pyBasePython.listF'>
    #                      ...
    #    'itk::Image' = {itkTemplate} <itkTemplate itk::Image>
    #                B2 = {type} <class 'itk.itkImagePython.itkImageB2'>
    #             CVD22 = {type} <class 'itk.itkImagePython.itkImageCVD22'>
    #             CVD23 = {type} <class 'itk.itkImagePython.itkImageCVD23'>
    #                      ...
    #     ...
    __named_template_registry__: Dict[str, "itkTemplate"] = {}
    # NOT IMPLEMENTED: __doxygen_root__ = itkConfig.doxygen_root


class itkTemplate(Mapping):
    """This class manages access to available template arguments of a C++ class.

    This class is generic and does not give help on the methods available in
    the instantiated class. To get help on a specific ITK class, instantiate an
    object of that class.

    e.g.: median = itk.MedianImageFilter[ImageType, ImageType].New()
          help(median)

    There are two ways to access types:

    1. With a dict interface. The user can manipulate template parameters
    similarly to C++, with the exception that the available parameters sets are
    chosen at compile time. It is also possible, with the dict interface, to
    explore the available parameters sets.
    2. With object attributes. The user can easily find the available parameters
    sets by pressing tab in interpreter like ipython
    """

    @staticmethod
    def normalizeName(name: str) -> str:
        """Normalize the class name to remove ambiguity

        This function removes the white spaces in the name, and also
        remove the pointer declaration "*" (it have no sense in python)"""
        name = name.strip()
        name = name.replace(" ", "")
        name = name.replace("*", "")

        return name

    @staticmethod
    def registerNoTpl(name: str, cl: "itkTemplate") -> None:
        """Register a class without template

        It can seem not useful to register classes without template (and it wasn't
        useful until the SmartPointer template was generated), but those classes
        can be used as template argument of classes with template.
        """
        itkTemplateBase.__template_instantiations_name_to_object__[
            itkTemplate.normalizeName(name)
        ] = cl

    @staticmethod
    def _NewImageReader(
        TemplateReaderType,
        increase_dimension: bool,
        primaryInputMethod,
        *args,
        **kwargs,
    ):
        def firstIfList(arg):
            if type(arg) in [list, tuple]:
                return arg[0]
            else:
                return arg

        inputFileName = ""
        if len(args) != 0:
            # try to find a type suitable for the primary input provided
            inputFileName = firstIfList(args[0])
        elif primaryInputMethod in kwargs:
            inputFileName = firstIfList(kwargs[primaryInputMethod])
        if not inputFileName:
            raise RuntimeError("No FileName specified.")
        import itk

        if "ImageIO" in kwargs:
            imageIO = kwargs["ImageIO"]
        else:
            imageIO = itk.ImageIOFactory.CreateImageIO(
                inputFileName, itk.CommonEnums.IOFileMode_ReadMode
            )
        if not imageIO:
            msg = ""
            if not os.path.isfile(inputFileName):
                msg += "\nThe file doesn't exist. \n" + f"Filename = {inputFileName}"
            raise RuntimeError(
                f"Could not create IO object for reading file {inputFileName}" + msg
            )
        # Read the metadata from the image file.
        imageIO.SetFileName(inputFileName)
        imageIO.ReadImageInformation()
        dimension: int = imageIO.GetNumberOfDimensions()
        # For image series, increase dimension if last dimension is not of size one.
        if increase_dimension and imageIO.GetDimensions(dimension - 1) != 1:
            dimension += 1
        componentAsString = imageIO.GetComponentTypeAsString(imageIO.GetComponentType())
        _io_component_type_dict: Dict[str, itkCType] = {
            "float": itk.F,
            "double": itk.D,
            "unsigned_char": itk.UC,
            "unsigned_short": itk.US,
            "unsigned_int": itk.UI,
            "unsigned_long": itk.UL,
            "unsigned_long_long": itk.ULL,
            "char": itk.SC,
            "short": itk.SS,
            "int": itk.SI,
            "long": itk.SL,
            "long_long": itk.SLL,
        }
        component: itkCType = _io_component_type_dict[componentAsString]
        pixel = imageIO.GetPixelTypeAsString(imageIO.GetPixelType())
        numberOfComponents: int = imageIO.GetNumberOfComponents()
        PixelType: itkCType = itkTemplate._pixelTypeFromIO(
            pixel, component, numberOfComponents
        )
        ImageType = itk.Image[PixelType, dimension]
        ReaderType = TemplateReaderType[ImageType]
        return ReaderType.New(*args, **kwargs)

    @staticmethod
    def _NewMeshReader(TemplateReaderType, *args, **kwargs):
        def firstIfList(arg):
            if type(arg) in [list, tuple]:
                return arg[0]
            else:
                return arg

        inputFileName: str = ""
        if len(args) != 0:
            # try to find a type suitable for the primary input provided
            inputFileName = firstIfList(args[0])
        elif "FileName" in kwargs:
            inputFileName = firstIfList(kwargs["FileName"])
        if not inputFileName:
            raise RuntimeError("No FileName specified.")
        import itk

        if "MeshIO" in kwargs:
            meshIO = kwargs["MeshIO"]
        else:
            meshIO = itk.MeshIOFactory.CreateMeshIO(
                inputFileName, itk.CommonEnums.IOFileMode_ReadMode
            )
        if not meshIO:
            msg = ""
            if not os.path.isfile(inputFileName):
                msg += "\nThe file doesn't exist. \n" + f"Filename = {inputFileName}"
            raise RuntimeError(
                f"Could not create IO object for reading file {inputFileName}" + msg
            )
        # Read the metadata from the mesh file.
        meshIO.SetFileName(inputFileName)
        meshIO.ReadMeshInformation()
        dimension: int = meshIO.GetPointDimension()
        componentAsString = meshIO.GetComponentTypeAsString(
            meshIO.GetPointPixelComponentType()
        )
        # For meshes with unknown pixel type, a common case, we assign the pixel
        # type to be float, which is well supported in the wrapping and handles
        # most use cases.
        _io_component_type_dict: Dict[str, itkCType] = {
            "unknown": itk.F,
            "float": itk.F,
            "double": itk.D,
            "unsigned_char": itk.UC,
            "unsigned_short": itk.US,
            "unsigned_int": itk.UI,
            "unsigned_long": itk.UL,
            "unsigned_long_long": itk.ULL,
            "char": itk.SC,
            "short": itk.SS,
            "int": itk.SI,
            "long": itk.SL,
            "long_long": itk.SLL,
        }
        component: itkCType = _io_component_type_dict[componentAsString]
        pixel = meshIO.GetPixelTypeAsString(meshIO.GetPointPixelType())
        numberOfComponents: int = meshIO.GetNumberOfPointPixelComponents()
        PixelType = itkTemplate._pixelTypeFromIO(pixel, component, numberOfComponents)
        MeshType = itk.Mesh[PixelType, dimension]
        ReaderType = TemplateReaderType[MeshType]
        return ReaderType.New(*args, **kwargs)

    @staticmethod
    def _pixelTypeFromIO(pixel: str, component, numberOfComponents: int) -> Any:
        import itk

        if pixel == "scalar":
            PixelType = component
        elif pixel == "rgb":
            PixelType = itk.RGBPixel[component]
        elif pixel == "rgba":
            PixelType = itk.RGBAPixel[component]
        elif pixel == "offset":
            PixelType = itk.Offset[numberOfComponents]
        elif pixel == "vector":
            PixelType = itk.Vector[component, numberOfComponents]
        elif pixel == "point":
            PixelType = itk.Point[component, numberOfComponents]
        elif pixel == "covariant_vector":
            PixelType = itk.CovariantVector[component, numberOfComponents]
        elif pixel == "symmetric_second_rank_tensor":
            PixelType = itk.SymmetricSecondRankTensor[component, numberOfComponents]
        elif pixel == "diffusion_tensor_3D":
            PixelType = itk.DiffusionTensor3D[component]
        elif pixel == "complex":
            PixelType = itk.complex[component]
        elif pixel == "fixed_array":
            PixelType = itk.FixedArray[component, numberOfComponents]
        elif pixel == "matrix":
            PixelType = itk.Matrix[
                component,
                int(math.sqrt(numberOfComponents)),
                int(math.sqrt(numberOfComponents)),
            ]
        else:
            raise RuntimeError(f"Unknown pixel type: {pixel}.")
        return PixelType

    def __local__init__(self, new_object_name: str) -> None:
        """
        Can not have a __init__ because we must use __new__
        so that the singleton takes preference.
        Use this to define the class member elements
        """
        self.__template__: Dict[
            str, Union[str, Callable[..., Any]]
        ] = collections.OrderedDict()
        self.__name__: str = new_object_name

    def __new__(cls, new_object_name: str) -> "itkTemplate":
        # Singleton pattern: we only make a single instance of any itkTemplate of
        # a given name. If we have already made the instance, just return it
        # as-is.
        if new_object_name not in itkTemplateBase.__named_template_registry__:
            # Create an raw itkTemplate object without calling the __init__
            # New object of type itkTemplate
            itkTemplateBase.__named_template_registry__[
                new_object_name
            ] = object.__new__(cls)
            # Must explicitly initialize the raw object.
            itkTemplateBase.__named_template_registry__[
                new_object_name
            ].__local__init__(new_object_name)
        return itkTemplateBase.__named_template_registry__[new_object_name]

    def __getnewargs_ex__(self):
        """Return arguments for __new__.
        Required by the Pickle protocol.
        """
        return (self.__name__,), {}

    def __add__(self, paramSetString: str, cl: Callable[..., Any]) -> None:
        """Add a new argument set and the resulting class to the template.

        paramSetString is the C++ string which defines the parameters set.
        cl is the class which corresponds to the couple template-argument set.
        """
        # recreate the full name and normalize it to avoid ambiguity
        normFullName: str = itkTemplate.normalizeName(
            f"{self.__name__}<{paramSetString}>"
        )

        # the full class should not be already registered. If it is, there is a
        # problem somewhere so warn the user so he can fix the problem
        if normFullName in itkTemplateBase.__template_instantiations_name_to_object__:
            message = (
                f"Template {normFullName}\n already defined as {itkTemplateBase.__template_instantiations_name_to_object__[normFullName]}\n is redefined "
                "as {cl}"
            )
            warnings.warn(message)
        # register the class
        itkTemplateBase.__template_instantiations_name_to_object__[normFullName] = cl

        # __find_param__ will parse the paramSetString and produce a list of
        # the same parameters transformed in corresponding python classes.
        # we transform this list in tuple to make it usable as key of the dict
        param: tuple = tuple(self.__find_param__(paramSetString))

        # once again, warn the user if the tuple of parameter is already
        # defined so he can fix the problem
        if param in self.__template__:
            message = f"Warning: template already defined '{normFullName}'"
            warnings.warn(message)
        # and register the parameter tuple
        self.__template__[param] = cl

        # add in __template_instantiations_object_to_name__ dictionary
        itkTemplateBase.__template_instantiations_object_to_name__[cl] = (self, param)

        # now populate the template
        # 2 cases:
        # - the template is a SmartPointer. In that case, the attribute name
        # will be the full real name of the class without the itk prefix and
        # _Pointer suffix
        # - the template is not a SmartPointer. In that case, we keep only the
        # end of the real class name which is a short string describing the
        # template arguments (for example IUC2)
        if cl.__name__.startswith("itk"):
            if cl.__name__.endswith("_Pointer"):
                # it's a SmartPointer
                attributeName = cl.__name__[len("itk") : -len("_Pointer")]
            else:
                # it's not a SmartPointer
                # we need to now the size of the name to keep only the suffix
                # short name does not contain :: and nested namespace
                # itk::Numerics::Sample -> itkSample
                shortNameSize = len(re.sub(r":.*:", "", self.__name__))
                attributeName = cl.__name__[shortNameSize:]
        else:
            short_name: str = re.sub(r":.*:", "", self.__name__)

            if not cl.__name__.startswith(short_name):
                short_name = re.sub(r".*::", "", self.__name__)

            attributeName = cl.__name__[len(short_name) :]

        if attributeName[0].isdigit():
            # the attribute name can't start with a number
            # add a single x before it to build a valid name.
            # Adding an underscore would hide the attributeName in IPython
            attributeName = "x" + attributeName

        # add the attribute to this object
        self.__dict__[attributeName] = cl

    def __instancecheck__(self, instance) -> bool:
        """Overloads `isinstance()` when called on an `itkTemplate` object.

        This function allows to compare an object to a filter without
        specifying the actual template arguments of the class. It will
        test all available template parameters that have been wrapped
        and return `True` if one that corresponds to the object is found.
        """
        for k in self.keys():
            if isinstance(instance, self.__getitem__(k)):
                return True
        return False

    def __hash__(self):
        """Overloads `hash()` when called on an `itkTemplate` object.

        Identify with the __name__, e.g. `itk.Image.__name__` is `itk::Image`.
        Used by frozenset construction in typing._GenericAlias
        """
        return hash(self.__name__)

    def __find_param__(self, paramSetString) -> List[Any]:
        """Find the parameters of the template.

        paramSetString is the C++ string which defines the parameters set.

        __find_param__ returns a list of itk classes, itkCType, and/or numbers
        which correspond to the parameters described in paramSetString.
        The parameters MUST have been registered before calling this method,
        or __find_param__ will return a string and not the wanted object, and
        will display a warning. Registration order is important.

        This method is not static only to be able to display the template name
        in the warning.
        """
        # split the string in a list of parameters
        paramStrings: List[str] = []
        num_open_inner_classes: int = 0
        part: str = paramSetString.split(",")
        for elt in part:
            if num_open_inner_classes == 0:
                paramStrings.append(elt)
            else:
                paramStrings[-1] += f",{elt}"
            #                         (num open)     - (num closed)
            num_open_inner_classes += elt.count("<") - elt.count(">")

        # convert all string parameters into classes (if possible)
        parameters: List[Any] = []
        for curr_param in paramStrings:
            # the parameter need to be normalized several time below
            # do it once here
            param_stripped = curr_param.strip()
            paramNorm = itkTemplate.normalizeName(param_stripped)

            if paramNorm in itkTemplateBase.__template_instantiations_name_to_object__:
                # the parameter is registered.
                # just get the real class form the dictionary
                param = itkTemplateBase.__template_instantiations_name_to_object__[
                    paramNorm
                ]

            elif itkCType.GetCType(param_stripped):
                # the parameter is a c type
                # just get the itkCtype instance
                param = itkCType.GetCType(param_stripped)

            elif paramNorm.isdigit():
                # the parameter is a number
                # convert the string to a number !
                param = int(param_stripped)

            elif paramNorm == "true":
                param = True
            elif paramNorm == "false":
                param = False

            else:
                # unable to convert the parameter
                # use it without changes, but display a warning message, to
                # incite developer to fix the problem
                message = "Warning: Unknown parameter '%s' in " "template '%s'" % (
                    paramNorm,
                    self.__name__,
                )
                warnings.warn(message)
                param = None
            if param:
                parameters.append(param)
                del param

        return parameters

    def __getitem__(self, parameters) -> _SWIG_CALLABLE_TYPE:
        """Return the class which corresponds to the given template parameters.

        parameters can be:
            - a single parameter (Ex: itk.Index[2])
            - a list of elements (Ex: itk.Image[itk.UC, 2])
        """

        parameters_type = type(parameters)
        if (parameters_type is not tuple) and (parameters_type is not list):
            # parameters is a single element.
            # include it in a list to manage the 2 cases in the same way
            parameters = [parameters]

        cleanParameters = []
        for param in parameters:
            # In the case of itk class instance, get the class
            name: str = param.__class__.__name__
            isclass: bool = inspect.isclass(param)
            if not isclass and name.startswith("itk") and name != "itkCType":
                param = param.__class__

            # append the parameter to the list. If it's not a supported type,
            # it is not in the dictionary and we will raise an exception below
            cleanParameters.append(param)

        key = tuple(cleanParameters)
        if self.__template__.get(key, None) is None:
            self._LoadModules()
        try:
            this_item = self.__template__[key]
        except KeyError:
            import itk

            raise itk.TemplateTypeError(self, key)
        return this_item

    def __repr__(self) -> str:
        return f"<itkTemplate {self.__name__}>"

    def __getattr__(self, attr):
        """Support for lazy loading."""
        self._LoadModules()
        return object.__getattribute__(self, attr)

    def _LoadModules(self) -> None:
        """Loads all the module that may have not been loaded by the lazy loading system.

        If multiple modules use the same object, the lazy loading system is only going to
        load the module in which the object belongs. The other modules will be loaded only when necessary.
        """
        name = self.__name__.split("::")[-1]  # Remove 'itk::' or 'itk::Function::'
        modules = base.itk_base_global_lazy_attributes[name]
        for module in modules:
            # find the module's name in sys.modules, or create a new module so named
            swig_module_name = "itk." + module + "Python"
            this_module = sys.modules.setdefault(
                swig_module_name, base.create_itk_module(module)
            )
            namespace = {}
            if not hasattr(this_module, "__templates_loaded"):
                base.itk_load_swig_module(module, namespace)
                base.load_module_needed_factories(module)

    def __dir__(self):
        """Returns the list of the attributes available in the current template.

        This loads all the modules that might be required by this template first,
        and then returns the list of attributes. It is used when dir() is called
        or when it tries to autocomplete attribute names.
        """
        self._LoadModules()

        def get_attrs(obj):
            if not hasattr(obj, "__dict__"):
                return []  # slots only
            dict_types = (dict, types.MappingProxyType)
            if not isinstance(obj.__dict__, dict_types):
                raise TypeError(f"{obj.__name__}.__dict__ is not a dictionary")
            return obj.__dict__.keys()

        def dir2(obj):
            attrs = _builtin_set()
            if not hasattr(obj, "__bases__"):
                # obj is an instance
                if not hasattr(obj, "__class__"):
                    # slots
                    return sorted(get_attrs(obj))
                klass = obj.__class__
                attrs.update(get_attrs(klass))
            else:
                # obj is a class
                klass = obj

            for cls in klass.__bases__:
                attrs.update(get_attrs(cls))
                attrs.update(dir2(cls))
            attrs.update(get_attrs(obj))
            return list(attrs)

        return dir2(self)

    def __call__(self, *args, **kwargs):
        """Deprecated procedural interface function.

        Use snake case function instead. This function is now
        merely a wrapper around the snake case function (more
        specifically around `__internal_call__()` to avoid
        creating a new instance twice).

        Create a process object, update with the inputs and
        attributes, and return the result.

        The syntax is the same as the one used in New().

        UpdateLargestPossibleRegion() is execute and the current output,
        or tuple of outputs if there is more than one, is returned.

        For example,

          outputImage = itk.MedianImageFilter(inputImage, Radius=(1,2))
        """
        from . import helpers

        short_name: str = re.sub(r".*::", "", self.__name__)
        snake = helpers.camel_to_snake_case(short_name)

        warnings.warn(
            "WrapITK warning: itk.%s() is deprecated for procedural"
            " interface. Use snake case function itk.%s() instead."
            % (short_name, snake),
            DeprecationWarning,
        )

        filter = self.New(*args, **kwargs)
        return filter.__internal_call__()

    def New(self, *args, **kwargs):
        """Instantiate the template with a type implied from its input.

        Template type specification can be avoided by assuming that the type's
        first template argument should have the same type as its primary input.
        This is generally true. If it is not true, then specify the types
        explicitly.

        For example, instead of the explicit type specification::

          median = itk.MedianImageFilter[ImageType, ImageType].New()
          median.SetInput(reader.GetOutput())

        call::

          median = itk.MedianImageFilter.New(Input=reader.GetOutput())

        or, the shortened::

          median = itk.MedianImageFilter.New(reader.GetOutput())

        or:

          median = itk.MedianImageFilter.New(reader)"""
        import itk

        keys = self.keys()
        cur = itk.auto_pipeline.current
        if self.__name__ == "itk::ImageFileReader":
            return self._NewImageReader(
                itk.ImageFileReader, False, "FileName", *args, **kwargs
            )
        elif self.__name__ == "itk::ImageSeriesReader":
            # Only support `FileNames`, not `FileName`, to simplify the logic and avoid having
            # to deal with checking if both keyword arguments are given.
            return self._NewImageReader(
                itk.ImageSeriesReader, True, "FileNames", *args, **kwargs
            )
        elif self.__name__ == "itk::MeshFileReader":
            return self._NewMeshReader(itk.MeshFileReader, *args, **kwargs)
        primary_input_methods = ("Input", "InputImage", "Input1")

        def ttype_for_input_type(keys_l, input_type_l):
            keys_first = list(filter(lambda k: k[0] == input_type_l, keys_l))
            # If there is more than one match, prefer the filter where the
            # second template argument, typically the second input or the
            # output, has the same type as the input
            keys_second = list(
                filter(lambda k: len(k) > 1 and k[1] == input_type_l, keys_first)
            )
            if len(keys_second):
                return keys_second
            return keys_first

        input_type = None
        if "ttype" in kwargs and keys:
            # Convert `ttype` argument to `tuple` as filter keys are tuples.
            # Note that the function `itk.template()` which returns the template
            # arguments of an object returns tuples and its returned value
            # should be usable in this context.
            # However, it is easy for a user to pass a list (e.g. [ImageType, ImageType]) or
            # a type (e.g., ImageType), and these need to work too.
            ttype = kwargs.pop("ttype")
            if not isinstance(ttype, tuple):
                if isinstance(ttype, list):
                    ttype = tuple(ttype)
                else:
                    ttype = (ttype,)
            # If there is not the correct number of template parameters, throw an error.
            if len(ttype) != len(list(keys)[0]):
                raise RuntimeError(
                    "Expected %d template parameters. %d parameters given."
                    % (len(list(keys)[0]), len(ttype))
                )
            keys = [k for k in keys if k == ttype]
        elif len(args) != 0:
            # try to find a type suitable for the primary input provided
            input_type = output(args[0]).__class__
            keys = ttype_for_input_type(keys, input_type)
        elif set(primary_input_methods).intersection(kwargs.keys()):
            for method in primary_input_methods:
                if method in kwargs:
                    input_type = output(kwargs[method]).__class__
                    keys = ttype_for_input_type(keys, input_type)
                    break
        elif cur is not None and len(cur) != 0:
            # try to find a type suitable for the input provided
            input_type = output(cur).__class__
            keys = ttype_for_input_type(keys, input_type)

        if len(keys) == 0:
            if not input_type:
                raise RuntimeError(
                    """No suitable template parameter can be found.

Please specify an input via the first argument, the 'ttype' keyword parameter,
or via one of the following keyword arguments: %s"""
                    % ", ".join(primary_input_methods)
                )
            else:
                import itk

                raise itk.TemplateTypeError(self, input_type)
        return self[list(keys)[0]].New(*args, **kwargs)

    def keys(self):
        return self.__template__.keys()

    def values(self):
        return list(self.__template__.values())

    def items(self):
        return list(self.__template__.items())

    # everything after this comment is for dict interface
    # and is a copy/paste from DictMixin
    # only methods to edit dictionary are not there
    def __iter__(self):
        yield from self.keys()

    def __contains__(self, key):
        return key in self

    def get(self, key, default=None):
        try:
            return self[key]
        except KeyError:
            return default

    def __len__(self):
        return len(self.keys())

    def GetTypes(self):
        """Helper method which prints out the available template parameters."""

        print(f"<itkTemplate {self.__name__}>")
        print("Options:")
        for tp in self.GetTypesAsList():
            print("  " + str(tp).replace("(", "[").replace(")", "]"))

    def GetTypesAsList(self):
        """Helper method which returns the available template parameters."""

        # Make a list of allowed types, and sort them
        ctypes = []
        classes = []
        others = []

        for key_tuple in self.__template__:
            key = str(key_tuple)
            if "itkCType" in key:
                ctypes.append(key)
            elif "class" in key:
                classes.append(key)
            else:
                others.append(key)
        # Sort the lists
        ctypes = sorted(ctypes)
        classes = sorted(classes)
        others = sorted(others)

        return ctypes + classes + others


# create a new New function which accepts parameters
def New(self, *args, **kargs):
    import itk

    itk.set_inputs(self, args, kargs)

    # now, try to add observer to display progress
    if "auto_progress" in kargs.keys():
        if kargs["auto_progress"] in [True, 1]:
            callback = itk.terminal_progress_callback
        elif kargs["auto_progress"] == 2:
            callback = itk.simple_progress_callback
        else:
            callback = None
    elif itkConfig.ProgressCallback:
        callback = itkConfig.ProgressCallback
    else:
        callback = None

    if callback and not issubclass(self.__class__, itk.Command):
        try:
            name = self.__class__.__name__

            def progress():
                # self and callback are kept referenced with a closure
                callback(name, self.GetProgress())

            self.AddObserver(itk.ProgressEvent(), progress)
        except AttributeError:
            # as this feature is designed for prototyping, it's not really a
            # problem if an object doesn't have progress reporter, so adding
            # reporter can silently fail
            pass
        except Exception:
            # it seems that something else has gone wrong...
            # silently fail to maintain backward compatibility
            pass

    if itkConfig.NotInPlace and "SetInPlace" in dir(self):
        self.SetInPlace(False)

    if itk.auto_pipeline.current is not None:
        itk.auto_pipeline.current.connect(self)

    return self
