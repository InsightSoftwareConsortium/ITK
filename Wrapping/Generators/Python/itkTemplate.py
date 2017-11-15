#==========================================================================
#
#   Copyright Insight Software Consortium
#
#   Licensed under the Apache License, Version 2.0 (the "License");
#   you may not use this file except in compliance with the License.
#   You may obtain a copy of the License at
#
#          http://www.apache.org/licenses/LICENSE-2.0.txt
#
#   Unless required by applicable law or agreed to in writing, software
#   distributed under the License is distributed on an "AS IS" BASIS,
#   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#   See the License for the specific language governing permissions and
#   limitations under the License.
#
#==========================================================================*/

from __future__ import print_function

import inspect
import os
import re
import sys
import types
import collections
if sys.version_info >= (3, 4):
    import importlib
else:
    import imp
import warnings
import itkConfig
import itkBase
import itkLazy
from itkTypes import itkCType


def registerNoTpl(name, cl):
    """Register a class without template

    It can seem not useful to register classes without template (and it wasn't
    useful until the SmartPointer template was generated), but those classes
    can be used as template argument of classes with template.
    """
    itkTemplate.__templates__[normalizeName(name)] = cl


def normalizeName(name):
    """Normalize the class name to remove ambiguity

    This function removes the white spaces in the name, and also
    remove the pointer declaration "*" (it have no sense in python) """

    name = name.replace(" ", "")
    name = name.replace("*", "")

    return name


class itkTemplate(object):

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
    sets by pressing tab in interperter like ipython
    """
    __templates__ = collections.OrderedDict()
    __class_to_template__ = {}
    __named_templates__ = {}
    __doxygen_root__ = itkConfig.doxygen_root

    def __new__(cls, name):
        # Singleton pattern: we only make a single instance of any Template of
        # a given name. If we have already made the instance, just return it
        # as-is.
        if name not in cls.__named_templates__:
                new_instance = object.__new__(cls)
                new_instance.__name__ = name
                new_instance.__template__ = collections.OrderedDict()
                cls.__named_templates__[name] = new_instance
        return cls.__named_templates__[name]

    def __add__(self, paramSetString, cl):
        """Add a new argument set and the resulting class to the template.

        paramSetString is the C++ string which defines the parameters set.
        cl is the class which corresponds to the couple template-argument set.
        """
        # recreate the full name and normalize it to avoid ambiguity
        normFullName = normalizeName(
            self.__name__ + "<" + paramSetString + ">")

        # the full class should not be already registered. If it is, there is a
        # problem somewhere so warn the user so he can fix the problem
        if normFullName in itkTemplate.__templates__:
            message = (
                "Template %s\n already defined as %s\n is redefined "
                "as %s") % (normFullName, self.__templates__[normFullName], cl)
            warnings.warn(message)
        # register the class
        itkTemplate.__templates__[normFullName] = cl

        # __find_param__ will parse the paramSetString and produce a list of
        # the same parameters transformed in corresponding python classes.
        # we transform this list in tuple to make it usable as key of the dict
        param = tuple(self.__find_param__(paramSetString))

        # once again, warn the user if the tuple of parameter is already
        # defined so he can fix the problem
        if param in self.__template__:
            message = "Warning: template already defined '%s'" % normFullName
            warnings.warn(message)
        # and register the parameter tuple
        self.__template__[param] = cl

        # add in __class_to_template__ dictionary
        itkTemplate.__class_to_template__[cl] = (self, param)

        # now populate the template
        # 2 cases:
        # - the template is a SmartPointer. In that case, the attribute name
        # will be the full real name of the class without the itk prefix and
        # _Pointer suffix
        # - the template is not a SmartPointer. In that case, we keep only the
        # end of the real class name which is a short string discribing the
        # template arguments (for example IUC2)
        if cl.__name__.startswith("itk"):
            if cl.__name__.endswith("_Pointer"):
                # it's a SmartPointer
                attributeName = cl.__name__[len("itk"):-len("_Pointer")]
            else:
                # it's not a SmartPointer
                # we need to now the size of the name to keep only the suffix
                # short name does not contain :: and nested namespace
                # itk::Numerics::Sample -> itkSample
                shortNameSize = len(re.sub(r':.*:', '', self.__name__))
                attributeName = cl.__name__[shortNameSize:]
        elif cl.__name__.startswith("vcl_complex"):
            raise AttributeError('former vcl_complex, handling ' + str(cl.__name__))
            # C++ name is likely to be std::complex here, instead of the
            # expected vcl_complex
            attributeName = cl.__name__[len("vcl_complex"):]
        else:
            shortName = re.sub(r':.*:', '', self.__name__)

            if not cl.__name__.startswith(shortName):
                shortName = re.sub(r'.*::', '', self.__name__)

            attributeName = cl.__name__[len(shortName):]

        if attributeName[0].isdigit():
            # the attribute name can't start with a number
            # add a single x before it to build a valid name.
            # Adding an underscore would hide the attributeName in IPython
            attributeName = "x" + attributeName

        # add the attribute to this object
        self.__dict__[attributeName] = cl

    def __find_param__(self, paramSetString):
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
        paramStrings = []
        inner = 0
        part = paramSetString.split(",")
        for elt in part:
            if inner == 0:
                paramStrings.append(elt)
            else:
                paramStrings[-1] += "," + elt
            inner += elt.count("<") - elt.count(">")

        # convert all string parameters into classes (if possible)
        parameters = []
        for param in paramStrings:
            # the parameter need to be normalized several time below
            # do it once here
            param = param.strip()
            paramNorm = normalizeName(param)

            if paramNorm in itkTemplate.__templates__:
                # the parameter is registered.
                # just get the really class form the dictionary
                param = itkTemplate.__templates__[paramNorm]

            elif itkCType.GetCType(param):
                # the parameter is a c type
                # just get the itkCtype instance
                param = itkCType.GetCType(param)

            elif paramNorm.isdigit():
                # the parameter is a number
                # convert the string to a number !
                param = int(param)

            elif paramNorm == "true":
                param = True
            elif paramNorm == "false":
                param = False

            else:
                # unable to convert the parameter
                # use it without changes, but display a warning message, to
                # incite developer to fix the problem
                message = (
                    "Warning: Unknown parameter '%s' in "
                    "template '%s'" % (param, self.__name__))
                warnings.warn(message)

            parameters.append(param)

        return parameters

    def __getitem__(self, parameters):
        """Return the class which corresponds to the given template parameters.

        parameters can be:
            - a single parameter (Ex: itk.Index[2])
            - a list of elements (Ex: itk.Image[itk.UC, 2])
        """

        parameters_type = type(parameters)
        if not parameters_type is tuple and not parameters_type is list:
            # parameters is a single element.
            # include it in a list to manage the 2 cases in the same way
            parameters = [parameters]

        cleanParameters = []
        for param in parameters:
            # In the case of itk class instance, get the class
            name = param.__class__.__name__
            isclass = inspect.isclass(param)
            if not isclass and name[:3] == 'itk' and name != "itkCType":
                param = param.__class__

            # append the parameter to the list. If it's not a supported type,
            # it is not in the dictionary and we will raise an exception below
            cleanParameters.append(param)

        try:
            return(self.__template__[tuple(cleanParameters)])
        except:
            self._LoadModules()
            try:
                return(self.__template__[tuple(cleanParameters)])
            except:
                raise KeyError(
                    'itkTemplate : No template %s for the %s class' %
                    (str(parameters), self.__name__))

    def __repr__(self):
        return '<itkTemplate %s>' % self.__name__

    def __getattr__(self, attr):
        """Support for lazy loading."""
        self._LoadModules()
        return object.__getattribute__(self, attr)


    def _LoadModules(self):
        """Loads all the module that may have not been loaded by the lazy loading system.

        If multiple modules use the same object, the lazy loading system is only going to
        load the module in which the object belongs. The other modules will be loaded only when necessary.
        """
        name=self.__name__.split('::')[-1] # Remove 'itk::' or 'itk::Function::'
        modules = itkBase.lazy_attributes[name]
        for module in modules:
            # find the module's name in sys.modules, or create a new module so named
            if sys.version_info >= (3, 4):
                this_module = sys.modules.setdefault(module, types.ModuleType(module))
            else:
                this_module = sys.modules.setdefault(module, imp.new_module(module))
            namespace = {}
            if not hasattr(this_module, '__templates_loaded'):
                itkBase.LoadModule(module, namespace)

    def __dir__(self):
        """Returns the list of the attributes available in the current template.

        This loads all the modules that might be required by this template first,
        and then returns the list of attributes. It is used when dir() is called
        or when it tries to autocomplete attribute names.
        """
        self._LoadModules()

        def get_attrs(obj):
            if not hasattr(obj, '__dict__'):
                return []  # slots only
            if sys.version_info >= (3, 0):
              dict_types = (dict, types.MappingProxyType)
            else:
              dict_types = (dict, types.DictProxyType)
            if not isinstance(obj.__dict__, dict_types):
                raise TypeError("%s.__dict__ is not a dictionary"
                                "" % obj.__name__)
            return obj.__dict__.keys()

        def dir2(obj):
            attrs = set()
            if not hasattr(obj, '__bases__'):
                # obj is an instance
                if not hasattr(obj, '__class__'):
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
        filt = self.New(*args, **kwargs)
        try:
            filt.UpdateLargestPossibleRegion()
            img = filt.GetOutput()
        except AttributeError:
            img = filt
        return img

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
            return self._NewImageFileReader(*args, **kwargs)
        primary_input_methods = ('Input', 'InputImage', 'Input1')
        if len(args) != 0:
            # try to find a type suitable for the primary input provided
            input_type = output(args[0]).__class__
            keys = [k for k in keys if k[0] == input_type]
        elif set(primary_input_methods).intersection(kwargs.keys()):
            for method in primary_input_methods:
                if method in kwargs:
                    input_type = output(kwargs[method]).__class__
                    keys = [k for k in keys if k[0] == input_type]
                    break
        elif cur is not None and len(cur) != 0:
            # try to find a type suitable for the input provided
            input_type = output(cur).__class__
            keys = [k for k in keys if k[0] == input_type]

        if len(keys) == 0:
            raise RuntimeError("No suitable template parameter can be found.")
        return self[list(keys)[0]].New(*args, **kwargs)

    def _NewImageFileReader(self, *args, **kwargs):
        primaryInputMethods = ('FileName',)
        inputFileName = ''
        if len(args) != 0:
            # try to find a type suitable for the primary input provided
            inputFileName = args[0]
        elif set(primaryInputMethods).intersection(kwargs.keys()):
            for method in primaryInputMethods:
                if method in kwargs:
                    inputFileName = kwargs[method]
                    break
        if not inputFileName:
            raise RuntimeError("No FileName specified.")
        import itk
        imageIO = itk.ImageIOFactory.CreateImageIO( inputFileName, itk.ImageIOFactory.ReadMode )
        if not imageIO:
            raise RuntimeError("No ImageIO is registered to handle the given file.")
        componentTypeDic= {"float": itk.F, "double": itk.D,
        "unsigned_char": itk.UC, "unsigned_short": itk.US, "unsigned_int": itk.UI,
        "unsigned_long": itk.UL, "char": itk.SC, "short": itk.SS,
        "int": itk.SI, "long": itk.SL}
        # Read the metadata from the image file.
        imageIO.SetFileName( inputFileName )
        imageIO.ReadImageInformation()
        dimension = imageIO.GetNumberOfDimensions()
        componentAsString = imageIO.GetComponentTypeAsString(imageIO.GetComponentType())
        component = componentTypeDic[componentAsString]
        pixel = imageIO.GetPixelTypeAsString(imageIO.GetPixelType())
        PixelType = itkTemplate._pixelTypeFromIO(pixel, component, dimension)
        ImageType = itk.Image[PixelType, dimension]
        ReaderType = itk.ImageFileReader[ImageType]
        return ReaderType.New(*args, **kwargs)

    @staticmethod
    def _pixelTypeFromIO(pixel, component, dimension):
        import itk
        if pixel == 'scalar':
            PixelType = component
        elif pixel == 'rgb':
            PixelType = itk.RGBPixel[component]
        elif pixel == 'rgba':
            PixelType = itk.RGBAPixel[component]
        elif pixel == 'offset':
            PixelType = itk.Offset[dimension]
        elif pixel == 'vector':
            PixelType = itk.Vector[component, dimension]
        elif pixel == 'point':
            PixelType = itk.Point[component, dimension]
        elif pixel == 'covariant_vector':
            PixelType = itk.CovariantVector[component, dimension]
        elif pixel == 'symmetric_second_rank_tensor':
            PixelType = itk.SymmetricSecondRankTensor[component, dimension]
        elif pixel == 'diffusion_tensor_3D':
            PixelType = itk.DiffusionTensor3D[component]
        elif pixel == 'complex':
            PixelType = itk.complex[component]
        elif pixel == 'fixed_array':
            PixelType = itk.FixedArray[component, dimension]
        elif pixel == 'matrix':
            PixelType = itk.Matrix[component, dimension, dimension]
        else:
            raise RuntimeError("Unknown pixel type: %s." % pixel)
        return PixelType

    def keys(self):
        return self.__template__.keys()

    # everything after this comment is for dict interface
    # and is a copy/paste from DictMixin
    # only methods to edit dictionary are not there
    def __iter__(self):
        for k in self.keys():
            yield k

    def __contains__(self, key):
        return key in self

    # third level takes advantage of second level definitions
    def iteritems(self):
        for k in self:
            yield (k, self[k])

    def iterkeys(self):
        return self.__iter__()

    # fourth level uses definitions from lower levels
    def itervalues(self):
        for _, v in self.iteritems():
            yield v

    def values(self):
        return [v for _, v in self.iteritems()]

    def items(self):
        return list(self.iteritems())

    def get(self, key, default=None):
        try:
            return self[key]
        except KeyError:
            return default

    def __len__(self):
        return len(self.keys())

    def GetTypes(self):
        """Helper method which prints out the available template parameters."""

        print("<itkTemplate %s>" % self.__name__)
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
        except:
            # it seems that something goes wrong...
            # as this feature is designed for prototyping, it's not really a
            # problem if an object doesn't have progress reporter, so adding
            # reporter can silently fail
            pass

    if itkConfig.NotInPlace and "SetInPlace" in dir(self):
        self.SetInPlace(False)

    if itk.auto_pipeline.current is not None:
        itk.auto_pipeline.current.connect(self)

    return self


def output(input):
    try:
        img = input.GetOutput()
    except AttributeError:
        img = input
    return img


def image(input):
    warnings.warn("WrapITK warning: itk.image() is deprecated. "
            "Use itk.output() instead.")
    return output(input)
