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

# The following line defines an ascii string used for dynamically refreshing
# the import and progress callbacks on the same terminal line.
# See http://www.termsys.demon.co.uk/vtansi.htm
# \033 is the C-style octal code for an escape character
# [2000D moves the cursor back 2000 columns, this is a brute force way of
# getting to the start of the line.
# [K erases the end of the line
clrLine = "\033[2000D\033[K"


def auto_not_in_place(v=True):
    """Force it to not run in place
    """
    import itkConfig
    itkConfig.NotInPlace = v


def auto_progress(progressType=1):
    """Set up auto progress report

    progressType:
        1 or True -> auto progress be used in a terminal
        2 -> simple auto progress (without special characters)
        0 or False -> disable auto progress
    """
    import itkConfig

    if progressType is True or progressType == 1:
        itkConfig.ImportCallback = terminal_import_callback
        itkConfig.ProgressCallback = terminal_progress_callback

    elif progressType == 2:
        itkConfig.ImportCallback = simple_import_callback
        itkConfig.ProgressCallback = simple_progress_callback

    elif progressType is False or progressType == 0:
        itkConfig.ImportCallback = None
        itkConfig.ProgressCallback = None

    else:
        raise ValueError("Invalid auto progress type: " + repr(progressType))


def terminal_progress_callback(name, p):
    """Display the progress of an object and clean the display once complete

    This function can be used with itkConfig.ProgressCallback
    """
    import sys
    print(clrLine + "%s: %f" % (name, p), file=sys.stderr, end="")
    if p == 1:
        print(clrLine, file=sys.stderr, end="")


def terminal_import_callback(name, p):
    """Display the loading of a module and clean the display once complete

    This function can be used with itkConfig.ImportCallback
    """
    import sys
    print(clrLine + "Loading %s... " % name, file=sys.stderr, end="")
    if p == 1:
        print(clrLine, file=sys.stderr, end="")


def simple_import_callback(name, p):
    """Print a message when a module is loading

    This function can be used with itkConfig.ImportCallback
    """
    import sys
    if p == 0:
        print("Loading %s... " % name, file=sys.stderr, end="")
    elif p == 1:
        print("done", file=sys.stderr)


def simple_progress_callback(name, p):
    """Print a message when an object is running

    This function can be used with itkConfig.ProgressCallback
    """
    import sys
    if p == 0:
        print("Running %s... " % name, file=sys.stderr, end="")
    elif p == 1:
        print("done", file=sys.stderr)


def force_load():
    """force itk to load all the submodules"""
    import itk
    for k in dir(itk):
        getattr(itk, k)


import sys


def echo(object, f=sys.stderr):
    """Print an object is f

    If the object has a method Print(), this method is used.
    repr(object) is used otherwise
    """
    print(f, object)
del sys


def size(imageOrFilter):
    """Return the size of an image, or of the output image of a filter

    This method take care of updating the needed informations
    """
    # we don't need the entire output, only its size
    imageOrFilter.UpdateOutputInformation()
    img = output(imageOrFilter)
    return img.GetLargestPossibleRegion().GetSize()


def physical_size(imageOrFilter):
    """Return the physical size of an image, or of the output image of a filter

    This method take care of updating the needed informations
    """
    # required because range is overloaded in this module
    import sys
    if sys.version_info >= (3, 0):
      from builtins import range
    else:
      from __builtin__ import range
    spacing_ = spacing(imageOrFilter)
    size_ = size(imageOrFilter)
    result = []
    for i in range(0, spacing_.Size()):
        result.append(spacing_.GetElement(i) * size_.GetElement(i))
    return result


def spacing(imageOrFilter):
    """Return the spacing of an image, or of the output image of a filter

    This method take care of updating the needed informations
    """
    # we don't need the entire output, only its size
    imageOrFilter.UpdateOutputInformation()
    img = output(imageOrFilter)
    return img.GetSpacing()


def origin(imageOrFilter):
    """Return the origin of an image, or of the output image of a filter

    This method take care of updating the needed informations
    """
    # we don't need the entire output, only its size
    imageOrFilter.UpdateOutputInformation()
    img = output(imageOrFilter)
    return img.GetOrigin()


def index(imageOrFilter):
    """Return the index of an image, or of the output image of a filter

    This method take care of updating the needed informations
    """
    # we don't need the entire output, only its size
    imageOrFilter.UpdateOutputInformation()
    img = output(imageOrFilter)
    return img.GetLargestPossibleRegion().GetIndex()


def region(imageOrFilter):
    """Return the region of an image, or of the output image of a filter

    This method take care of updating the needed informations
    """
    # we don't need the entire output, only its size
    imageOrFilter.UpdateOutputInformation()
    img = output(imageOrFilter)
    return img.GetLargestPossibleRegion()

HAVE_NUMPY = True
try:
    import numpy
except ImportError:
    HAVE_NUMPY = False

def _get_itk_pixelid(numpy_array_type):
    """Returns a ITK PixelID given a numpy array."""

    if not HAVE_NUMPY:
        raise ImportError('Numpy not available.')
    import itk
    # This is a Mapping from numpy array types to itk pixel types.
    _np_itk = {numpy.uint8:itk.UC,
                numpy.uint16:itk.US,
                numpy.uint32:itk.UI,
                numpy.uint64:itk.UL,
                numpy.int8:itk.SC,
                numpy.int16:itk.SS,
                numpy.int32:itk.SI,
                numpy.int64:itk.SL,
                numpy.float32:itk.F,
                numpy.float64:itk.D,
                numpy.complex64:itk.complex[itk.F],
                numpy.complex128:itk.complex[itk.D]
                }
    try:
        return _np_itk[numpy_array_type.dtype.type]
    except KeyError as e:
        for key in _np_itk:
            if numpy.issubdtype(numpy_array_type.dtype.type, key):
                return _np_itk[key]
            raise e

def _GetArrayFromImage(imageOrFilter, function):
    """Get an Array with the content of the image buffer
    """
    # Check for numpy
    if not HAVE_NUMPY:
        raise ImportError('Numpy not available.')
    # Finds the image type
    import itk
    keys = [k for k in itk.PyBuffer.keys() if k[0] == output(imageOrFilter).__class__]
    if len(keys ) == 0:
        raise RuntimeError("No suitable template parameter can be found.")
    ImageType = keys[0]
    # Create a numpy array of the type of the input image
    templatedFunction = getattr(itk.PyBuffer[keys[0]], function)
    return templatedFunction(output(imageOrFilter))

def GetArrayFromImage(imageOrFilter):
    """Get an array with the content of the image buffer
    """
    return _GetArrayFromImage(imageOrFilter, "GetArrayFromImage")

def GetArrayViewFromImage(imageOrFilter):
    """Get an array view with the content of the image buffer
    """
    return _GetArrayFromImage(imageOrFilter, "GetArrayViewFromImage")

def _GetImageFromArray(arr, function):
    """Get an ITK image from a Python array.
    """
    if not HAVE_NUMPY:
        raise ImportError('Numpy not available.')
    import itk
    PixelType = _get_itk_pixelid(arr)
    ImageType = itk.Image[PixelType,arr.ndim]
    templatedFunction = getattr(itk.PyBuffer[ImageType], function)
    return templatedFunction(arr)

def GetImageFromArray(arr):
    """Get an ITK image from a Python array.
    """
    return _GetImageFromArray(arr, "GetImageFromArray")

def GetImageViewFromArray(arr):
    """Get an ITK image view from a Python array.
    """
    return _GetImageFromArray(arr, "GetImageViewFromArray")

def _GetArrayFromVnlObject(vnlObject, function):
    """Get an array with the content of vnlObject
    """
    # Check for numpy
    if not HAVE_NUMPY:
        raise ImportError('Numpy not available.')
    # Finds the vnl object type
    import itk
    PixelType = itk.template(vnlObject)[1][0]
    keys = [k for k in itk.PyVnl.keys() if k[0] == PixelType]
    if len(keys ) == 0:
        raise RuntimeError("No suitable template parameter can be found.")
    # Create a numpy array of the type of the vnl object
    templatedFunction = getattr(itk.PyVnl[keys[0]], function)
    return templatedFunction(vnlObject)

def GetArrayFromVnlVector(vnlVector):
    """Get an array with the content of vnlVector
    """
    return _GetArrayFromVnlObject(vnlVector, "GetArrayFromVnlVector")

def GetArrayViewFromVnlVector(vnlVector):
    """Get an array view of vnlVector
    """
    return _GetArrayFromVnlObject(vnlVector, "GetArrayViewFromVnlVector")

def GetArrayFromVnlMatrix(vnlMatrix):
    """Get an array with the content of vnlMatrix
    """
    return _GetArrayFromVnlObject(vnlMatrix, "GetArrayFromVnlMatrix")

def GetArrayViewFromVnlMatrix(vnlMatrix):
    """Get an array view of vnlMatrix
    """
    return _GetArrayFromVnlObject(vnlMatrix, "GetArrayViewFromVnlMatrix")

def _GetVnlObjectFromArray(arr, function):
    """Get a vnl object from a Python array.
    """
    if not HAVE_NUMPY:
        raise ImportError('Numpy not available.')
    import itk
    PixelType = _get_itk_pixelid(arr)
    templatedFunction = getattr(itk.PyVnl[PixelType], function)
    return templatedFunction(arr)

def GetVnlVectorFromArray(arr):
    """Get a vnl vector from a Python array.
    """
    return _GetVnlObjectFromArray(arr, "GetVnlVectorFromArray")

def GetVnlMatrixFromArray(arr):
    """Get a vnl matrix from a Python array.
    """
    return _GetVnlObjectFromArray(arr, "GetVnlMatrixFromArray")

# return an image
from itkTemplate import image, output


def template(cl):
    """Return the template of a class (or of the class of an object) and
    its parameters

    template() returns a tuple with 2 elements:
        - the first one is the itkTemplate object
        - the second is a tuple containing the template parameters
    """
    from itkTemplate import itkTemplate
    return itkTemplate.__class_to_template__[class_(cl)]


def ctype(s):
    """Return the c type corresponding to the string passed in parameter

    The string can contain some extra spaces.
    see also itkCType
    """
    from itkTypes import itkCType

    ret = itkCType.GetCType(" ".join(s.split()))
    if ret is None:
        raise KeyError("Unrecognized C type '%s'" % s)
    return ret


def class_(obj):
    """Return a class from an object

    Often in itk, the __class__ is not what the user is expecting.
    class_() should do a better job
    """
    import inspect
    if inspect.isclass(obj):
        # obj is already a class !
        return obj
    else:
        return obj.__class__


def range(imageOrFilter):
    """Return the range of values in a image of in the output image of a filter

    The minimum and maximum values are returned in a tuple: (min, max)
    range() take care of updating the pipeline
    """
    import itk
    img = output(imageOrFilter)
    img.UpdateOutputInformation()
    img.Update()
    # don't put that calculator in the automatic pipeline
    tmp_auto_pipeline = auto_pipeline.current
    auto_pipeline.current = None
    comp = itk.MinimumMaximumImageCalculator[img].New(Image=img)
    auto_pipeline.current = tmp_auto_pipeline
    comp.Compute()
    return (comp.GetMinimum(), comp.GetMaximum())


def imwrite(imageOrFilter, fileName, compression=False):
    """Write a image or the output image of a filter to a file.

    The writer is instantiated with the image type of the image in
    parameter (or, again, with the output image of the filter in parameter).
    """
    import itk
    img = output(imageOrFilter)
    img.UpdateOutputInformation()
    # don't put that writer in the automatic pipeline
    tmp_auto_pipeline = auto_pipeline.current
    auto_pipeline.current = None
    writer = itk.ImageFileWriter[img].New(
        Input=img,
        FileName=fileName,
        UseCompression=compression)
    auto_pipeline.current = tmp_auto_pipeline
    writer.Update()

# For backwards compatibility
def write(*args, **kwargs):
    import warnings
    warnings.warn("WrapITK warning: itk.write() is deprecated. "
            "Use itk.imwrite() instead.")
    imwrite(*args, **kwargs)

def imread(fileName, pixelType=None):
    """Read an image from a file and return an itk.Image.

    The reader is instantiated with the image type of the image file.
    """
    import itk
    if pixelType:
        imageIO = itk.ImageIOFactory.CreateImageIO( fileName, itk.ImageIOFactory.ReadMode )
        if not imageIO:
            raise RuntimeError("No ImageIO is registered to handle the given file.")
        imageIO.SetFileName( fileName )
        imageIO.ReadImageInformation()
        dimension = imageIO.GetNumberOfDimensions()
        ImageType=itk.Image[pixelType,dimension]
        reader = itk.ImageFileReader[ImageType].New(FileName=fileName)
    else:
        reader = itk.ImageFileReader.New(FileName=fileName)
    reader.Update()
    return reader.GetOutput()

def search(s, case_sensitive=False):  # , fuzzy=True):
    """Search for a class name in the itk module.
    """
    s = s.replace(" ", "")
    if not case_sensitive:
        s = s.lower()
    import itk
    names = sorted(dir(itk))
    # exact match first
    if case_sensitive:
        res = [n for n in names if s == n]
    else:
        res = [n for n in names if s == n.lower()]
    # then exact match inside the name
    if case_sensitive:
        res += [n for n in names if s in n and s != n]
    else:
        res += [n for n in names if s in n.lower() and s != n.lower()]
#     if fuzzy:
#         try:
# everything now requires editdist
#             import editdist
#             if case_sensitive:
#                 res.sort(key=lambda x: editdist.distance(x, s))
#             else:
#                 res.sort(key=lambda x: (editdist.distance(x.lower(), s), x))
#         except:
#             pass
    return res


def set_inputs(newItkObject, args=[], kargs={}):
    """Set the inputs of the given objects, according to the non named or the
    named parameters in args and kargs

    This function tries to assign all the non named parameters in the input of
    the newItkObject
    - the first non named parameter in the first input, etc.

    The named parameters are used by calling the method with the same name
    prefixed by 'Set'.
    set_inputs( obj, kargs={'Threshold': 10} ) calls obj.SetThreshold(10)

    This is the function use in the enhanced New() method to manage the inputs.
    It can be used to produce a similar behavior:

    def SetInputs(self, *args, **kargs):
        import itk
        itk.set_inputs(self, *args, **kargs)
    """
    # try to get the images from the filters in args
    args = [output(arg) for arg in args]

    # args without name are filter used to set input image
    #
    # count SetInput calls to call SetInput, SetInput2, SetInput3, ...
    # usefull with filter which take 2 input (or more) like SubstractImageFiler
    # Ex: substract image2.png to image1.png and save the result in result.png
    # r1 = itk.ImageFileReader.US2.New(FileName='image1.png')
    # r2 = itk.ImageFileReader.US2.New(FileName='image2.png')
    # s = itk.SubtractImageFilter.US2US2US2.New(r1, r2)
    # itk.ImageFileWriter.US2.New(s, FileName='result.png').Update()
    try:
        for setInputNb, arg in enumerate(args):
            methodName = 'SetInput%i' % (setInputNb + 1)
            if methodName in dir(newItkObject):
                # first try to use methods called SetInput1, SetInput2, ...
                # those method should have more chances to work in case of
                # multiple input types
                getattr(newItkObject, methodName)(arg)
            else:
                # no method called SetInput?
                # try with the standard SetInput(nb, input)
                newItkObject.SetInput(setInputNb, arg)
    except TypeError as e:
        # the exception have (at least) to possible reasons:
        # + the filter don't take the input number as first argument
        # + arg is an object of wrong type
        #
        # if it's not the first input, re-raise the exception
        if setInputNb != 0:
            raise e
        # it's the first input, try to use the SetInput() method without input
        # number
        newItkObject.SetInput(args[0])
        # but raise an exception if there is more than 1 argument
        if len(args) > 1:
            raise TypeError('Object accept only 1 input.')
    except AttributeError:
        # There is no SetInput() method, try SetImage
        # but before, check the number of inputs
        if len(args) > 1:
            raise TypeError('Object accept only 1 input.')
        methodList = ['SetImage', 'SetInputImage']
        methodName = None
        for m in methodList:
            if m in dir(newItkObject):
                methodName = m
        if methodName:
            getattr(newItkObject, methodName)(args[0])
        else:
            raise AttributeError('No method found to set the input.')

    # named args : name is the function name, value is argument(s)
    for attribName, value in kargs.items():
        # use Set as prefix. It allow to use a shorter and more intuitive
        # call (Ex: itk.ImageFileReader.UC2.New(FileName='image.png')) than
        # with the full name
        # (Ex: itk.ImageFileReader.UC2.New(SetFileName='image.png'))
        if attribName not in ["auto_progress", "template_parameters"]:
            attrib = getattr(newItkObject, 'Set' + attribName)
            attrib(output(value))


def show(input, **kargs):
    """display an image
    """
    import itk
    img = output(input)
    if img.GetImageDimension() == 3 and "show3D" in dir(itk):
        return itk.show3D(input, **kargs)
    else:
        # print("2D not supported yet, use the 3D viewer.")
        return show2D(input, **kargs)


class show2D:

    """Display a 2D image
    """

    def __init__(self, imageOrFilter, Label=False, Title=None):
        import tempfile
        import itk
        import os
        import platform
        # get some data from the environment
        command = os.environ.get("WRAPITK_SHOW2D_COMMAND")
        if command is None:
            if platform.system() == "Darwin":
                command = (
                    "open -a ImageJ -n --args -eval 'open(\"%(image)s\"); "
                    "run (\"View 100%%\"); rename(\"%(title)s\");'")
            else:
                command = (
                    "imagej %(image)s -run 'View 100%%' -eval "
                    "'rename(\"%(title)s\")' &")

        label_command = os.environ.get("WRAPITK_SHOW2D_LABEL_COMMAND")
        if label_command is None:
            if platform.system() == "Darwin":
                label_command = (
                    "open -a ImageJ -n --args -eval 'open(\"%(image)s\"); "
                    "run (\"View 100%%\"); rename(\"%(title)s\"); "
                    "run(\"3-3-2 RGB\");'")
            else:
                label_command = (
                    "imagej %(image)s -run 'View 100%%' -eval "
                    "'rename(\"%(title)s\")' -run '3-3-2 RGB' &")

        compress = os.environ.get(
            "WRAPITK_SHOW2D_COMPRESS",
            "true").lower() in ["on", "true", "yes", "1"]
        extension = os.environ.get("WRAPITK_SHOW2D_EXTENSION", ".tif")

        # use the tempfile module to get a non used file name and to put
        # the file at the rignt place
        self.__tmpFile__ = tempfile.NamedTemporaryFile(suffix=extension)
        # get an updated image
        img = output(imageOrFilter)
        img.UpdateOutputInformation()
        img.Update()
        if Title is None:
            # try to generate a title
            s = img.GetSource()
            if s:
                s = itk.down_cast(s)
                if hasattr(img, "GetSourceOutputIndex"):
                    o = '[%s]' % img.GetSourceOutputIndex()
                elif hasattr(img, "GetSourceOutputName"):
                    o = '[%s]' % img.GetSourceOutputName()
                else:
                    o = ""
                Title = "%s%s" % (s.__class__.__name__, o)
            else:
                Title = img.__class__.__name__
            try:
                import IPython
                ip = IPython.get_ipython()
                if ip is not None:
                    names = []
                    ref = imageOrFilter
                    if s:
                        ref = s
                    for n, v in ip.user_ns.iteritems():
                        if isinstance(v, itk.LightObject) and v == ref:
                            names.append(n)
                    if names != []:
                        Title = ", ".join(names) + " - " + Title
            except ImportError:
                # just do nothing
                pass
        # change the LabelMaps to an Image, so we can look at them easily
        if 'LabelMap' in dir(itk) and img.GetNameOfClass() == 'LabelMap':
            # retreive the biggest label in the label map
            maxLabel = img.GetNthLabelObject(
                img.GetNumberOfLabelObjects() - 1).GetLabel()
            # search for a filter to convert the label map
            lab = itk.LabelMapToLabelImageFilter.keys()
            maxVal = itk.NumericTraits[itk.template(params[1])[1][0]].max()
            cond = params[0] == class_(img) and maxVal >= maxLabel
            label_image_type = sorted([params[1] for params in lab if cond])[0]
            convert = itk.LabelMapToLabelImageFilter[
                img, label_image_type].New(img)
            convert.Update()
            img = convert.GetOutput()
            # this is a label image - force the parameter
            Label = True
        write(img, self.__tmpFile__.name, compress)
        # now run imview
        import os
        if Label:
            os.system(
                label_command %
                {"image": self.__tmpFile__.name, "title": Title})
        else:
            os.system(
                command %
                {"image": self.__tmpFile__.name, "title": Title})


class templated_class:

    """This class is used to mimic the behavior of the templated C++ classes.

    It is used this way:

    class CustomClass:
        # class definition here
    CustomClass = templated_class(CustomClass)

    customObject = CustomClass[template, parameters].New()

    The template parameters are passed to the custom class constructor as a
    named parameter 'template_parameters' in a tuple.

    The custom class may implement a static method
    check_template_parameters(parameters) which should raise an exception if
    the template parameters provided are not suitable to instantiate the custom
    class.
    """

    def __init__(self, cls):
        """cls is the custom class
        """
        self.__cls__ = cls
        self.__templates__ = {}

    def New(self, *args, **kargs):
        """Use the parameters to infer the types of the template parameters.
        """
        # extract the types from the arguments to instantiate the class
        import itk
        types = tuple(itk.class_(o) for o in args)
        return self[types].New(*args, **kargs)

    def __getitem__(self, template_parameters):
        """Return a pair class-template parameters ready to be instantiated.

        The template parameters may be validated if the custom class provide
        the static method check_template_parameters(parameters).
        """
        if not isinstance(template_parameters, tuple):
            template_parameters = (template_parameters,)
        return (
            templated_class.__templated_class_and_parameters__(
                self,
                template_parameters)
        )

    def check_template_parameters(self, template_parameters):
        """Check the template parameters passed in parameter.
        """
        # this method is there mainly to make possible to reuse it in the
        # custom class constructor after having used templated_class().
        # Without that, the following example doesn't work:
        #
        # class CustomClass:
        #     def __init__(self, *args, **kargs):
        #         template_parameters = kargs["template_parameters"]
        #         CustomClass.check_template_parameters(template_parameters)
        # other init stuff
        #     def check_template_parameters(template_parameters):
        # check, really
        #         pass
        #    CustomClass = templated_class(CustomClass)
        #
        self.__cls__.check_template_parameters(template_parameters)

    def add_template(self, name, params):
        if not isinstance(params, list) and not isinstance(params, tuple):
            params = (params,)
        params = tuple(params)
        val = self[params]
        self.__templates__[params] = val
        setattr(self, name, val)

    def add_image_templates(self, *args):
        import itk
        if args == []:
            return
        combinations = [[t] for t in args[0]]
        for types in args[1:]:
            temp = []
            for t in types:
                for c in combinations:
                    temp.append(c + [t])
            combinations = temp
        for d in itk.DIMS:
            for c in combinations:
                parameters = []
                name = ""
                for t in c:
                    parameters.append(itk.Image[t, d])
                    name += "I" + t.short_name + str(d)
                self.add_template(name, tuple(parameters))

    class __templated_class_and_parameters__:

        """Inner class used to store the pair class-template parameters ready
        to instantiate.
        """

        def __init__(self, templated_class, template_parameters):
            self.__templated_class__ = templated_class
            self.__template_parameters__ = template_parameters
            if "check_template_parameters" in dir(templated_class.__cls__):
                templated_class.__cls__.check_template_parameters(
                    template_parameters)

        def New(self, *args, **kargs):
            """A New() method to mimic the ITK default behavior, even if the
            class doesn't provide any New() method.
            """
            kargs["template_parameters"] = self.__template_parameters__
            if "New" in dir(self.__templated_class__.__cls__):
                obj = self.__templated_class__.__cls__.New(*args, **kargs)
            else:
                obj = self.__templated_class__.__cls__(*args, **kargs)
            setattr(
                obj,
                "__template_parameters__",
                self.__template_parameters__)
            setattr(obj, "__templated_class__", self.__templated_class__)
            return obj

        def __call__(self, *args, **kargs):
            return self.New(*args, **kargs)

    def keys(self):
        return self.__templates__.keys()

    # everything after this comment is for dict interface
    # and is a copy/paste from DictMixin
    # only methods to edit dictionary are not there
    def __iter__(self):
        for k in self.keys():
            yield k

    def has_key(self, key):
        try:
            value = self[key]
        except KeyError:
            return False
        return True

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


class pipeline:

    """A convenient class to store the reference to the filters of a pipeline

    With this class, a method can create a pipeline of several filters and
    return it without losing the references to the filters in this pipeline.
    The pipeline object act almost like a filter (it has a GetOutput() method)
    and thus can be simply integrated in another pipeline.
    """

    def __init__(self, *args, **kargs):
        self.clear()
        self.input = None
        set_inputs(self, args, kargs)

    def connect(self, filter):
        """Connect a new filter to the pipeline

        The output of the first filter will be used as the input of this
        one and the filter passed as parameter will be added to the list
        """
        if self.GetOutput() is not None:
            set_inputs(filter, [self.GetOutput()])
        self.append(filter)

    def append(self, filter):
        """Add a new filter to the pipeline

        The new filter will not be connected. The user must connect it.
        """
        self.filters.append(filter)

    def clear(self):
        """Clear the filter list
        """
        self.filters = []

    def GetOutput(self, index=0):
        """Return the output of the pipeline

        If another output is needed, use
        pipeline.filters[-1].GetAnotherOutput() instead of this method,
        subclass pipeline to implement another GetOutput() method, or use
        expose()
        """
        if len(self.filters) == 0:
            return self.GetInput()
        else:
            filter = self.filters[-1]
            if hasattr(filter, "__getitem__"):
                return filter[index]
            try:
                return filter.GetOutput(index)
            except:
                if index == 0:
                    return filter.GetOutput()
                else:
                    raise ValueError("Index can only be 0 on that object")

    def SetInput(self, input):
        """Set the input of the pipeline
        """
        if len(self.filters) != 0:
            set_inputs(self.filters[0], [input])
        self.input = input

    def GetInput(self):
        """Get the input of the pipeline
        """
        return self.input

    def Update(self):
        """Update the pipeline
        """
        if len(self.filters) > 0:
            return self.filters[-1].Update()

    def UpdateLargestPossibleRegion(self):
        """Update the pipeline
        """
        if len(self.filters) > 0:
            return self.filters[-1].UpdateLargestPossibleRegion()

    def UpdateOutputInformation(self):
        if "UpdateOutputInformation" in dir(self.filters[-1]):
            self.filters[-1].UpdateOutputInformation()
        else:
            self.Update()

    def __len__(self):
        if len(self.filters) == 0:
            return 1
        else:
            return self.filters[-1].GetNumberOfOutputs()

    def __getitem__(self, item):
        return self.GetOutput(item)

    def __call__(self, *args, **kargs):
        set_inputs(self, args, kargs)
        self.UpdateLargestPossibleRegion()
        return self

    def expose(self, name, new_name=None, position=-1):
        """Expose an attribute from a filter of the minipeline.

        Once called, the pipeline instance has a new Set/Get set of methods to
        access directly the corresponding method of one of the filter of the
        pipeline.
        Ex: p.expose( "Radius" )
                p.SetRadius( 5 )
                p.GetRadius( 5 )
        By default, the attribute usable on the pipeline instance has the same
        name than the one of the filter, but it can be changed by providing a
        value to new_name.
        The last filter of the pipeline is used by default, but another one may
        be used by giving its position.
        Ex: p.expose("Radius", "SmoothingNeighborhood", 2)
            p.GetSmoothingNeighborhood()
        """
        if new_name is None:
            new_name = name
        src = self.filters[position]
        ok = False
        set_name = "Set" + name
        if set_name in dir(src):
            setattr(self, "Set" + new_name, getattr(src, set_name))
            ok = True
        get_name = "Get" + name
        if get_name in dir(src):
            setattr(self, "Get" + new_name, getattr(src, get_name))
            ok = True
        if not ok:
            raise RuntimeError(
                "No attribute %s at position %s." %
                (name, position))


class auto_pipeline(pipeline):
    current = None

    def __init__(self, *args, **kargs):
        pipeline.__init__(self, *args, **kargs)
        self.Start()

    def Start(self):
        auto_pipeline.current = self

    def Stop(self):
        auto_pipeline.current = None


def down_cast(obj):
    """Down cast an itkLightObject (or a object of a subclass) to its most
    specialized type.
    """
    import itk
    import itkTemplate
    className = obj.GetNameOfClass()
    t = getattr(itk, className)
    if isinstance(t, itkTemplate.itkTemplate):
        for c in t.values():
            try:
                return c.cast(obj)
            except:
                # fail silently for now
                pass
        raise RuntimeError(
            "Can't downcast to a specialization of %s" %
            className)
    else:
        return t.cast(obj)


def attribute_list(i, name):
    """Returns a list of the specified attributes for the objects in the image.

    i: the input LabelImage
    name: the attribute name
    """
    import itk
    i = itk.output(i)
    relabel = itk.StatisticsRelabelLabelMapFilter[i].New(
        i,
        Attribute=name,
        ReverseOrdering=True,
        InPlace=False)
    relabel.UpdateLargestPossibleRegion()
    r = relabel.GetOutput()
    l = []
    for i in range(1, r.GetNumberOfLabelObjects() + 1):
        l.append(r.GetLabelObject(i).__getattribute__("Get" + name)())
    return l


def attributes_list(i, names):
    """Returns a list of the specified attributes for the objects in the image.

    i: the input LabelImage
    name: the attribute name
    """
    import itk
    i = itk.output(i)
    relabel = itk.StatisticsRelabelLabelMapFilter[i].New(
        i,
        Attribute=names[0],
        ReverseOrdering=True,
        InPlace=False)
    relabel.UpdateLargestPossibleRegion()
    r = relabel.GetOutput()
    l = []
    for i in range(1, r.GetNumberOfLabelObjects() + 1):
        attrs = []
        for name in names:
            attrs.append(r.GetLabelObject(i).__getattribute__("Get" + name)())
        l.append(tuple(attrs))
    return l


def attribute_dict(i, name):
    """Returns a dict with the attribute values in keys and a list of the
    corresponding objects in value

    i: the input LabelImage
    name: the name of the attribute
    """
    import itk
    i = itk.output(i)
    relabel = itk.StatisticsRelabelLabelMapFilter[i].New(
        i,
        Attribute=name,
        ReverseOrdering=True,
        InPlace=False)
    relabel.UpdateLargestPossibleRegion()
    r = relabel.GetOutput()
    d = {}
    for i in range(1, r.GetNumberOfLabelObjects() + 1):
        lo = r.GetLabelObject(i)
        v = lo.__getattribute__("Get" + name)()
        l = d.get(v, [])
        l.append(lo)
        d[v] = l
    return d


def number_of_objects(i):
    """Returns the number of objets in the image.

    i: the input LabelImage
    """
    import itk
    i.UpdateLargestPossibleRegion()
    i = itk.output(i)
    return i.GetNumberOfLabelObjects()


def ipython_kw_matches(text):
    """Match named ITK object's named parameters"""
    import IPython
    import itk
    import re
    import inspect
    import itkTemplate
    regexp = re.compile(r'''
                    '.*?' |  # single quoted strings or
                    ".*?" |  # double quoted strings or
                    \w+     |  # identifier
                    \S  # other characters
                    ''', re.VERBOSE | re.DOTALL)
    ip = IPython.get_ipython()
    if "." in text:  # a parameter cannot be dotted
        return []
    # 1. Find the nearest identifier that comes before an unclosed
    # parenthesis e.g. for "foo (1+bar(x), pa", the candidate is "foo".
    if ip.Completer.readline:
        textUntilCursor = ip.Completer.readline.get_line_buffer()[:ip.Completer.readline.get_endidx()]
    else:
        # IPython >= 5.0.0, which is based on the Python Prompt Toolkit
        textUntilCursor = ip.Completer.text_until_cursor

    tokens = regexp.findall(textUntilCursor)
    tokens.reverse()
    iterTokens = iter(tokens)
    openPar = 0
    for token in iterTokens:
        if token == ')':
            openPar -= 1
        elif token == '(':
            openPar += 1
            if openPar > 0:
                # found the last unclosed parenthesis
                break
    else:
        return []
    # 2. Concatenate dotted names ("foo.bar" for "foo.bar(x, pa" )
    ids = []
    isId = re.compile(r'\w+$').match
    while True:
        try:
            ids.append(iterTokens.next())
            if not isId(ids[-1]):
                ids.pop()
                break
            if not iterTokens.next() == '.':
                break
        except StopIteration:
            break
    # lookup the candidate callable matches either using global_matches
    # or attr_matches for dotted names
    if len(ids) == 1:
        callableMatches = ip.Completer.global_matches(ids[0])
    else:
        callableMatches = ip.Completer.attr_matches('.'.join(ids[::-1]))
    argMatches = []
    for callableMatch in callableMatches:
        # drop the .New at this end, so we can search in the class members
        if callableMatch.endswith(".New"):
            callableMatch = callableMatch[:-4]
        try:
            object = eval(callableMatch, ip.Completer.namespace)
            if isinstance(object, itkTemplate.itkTemplate):
                # this is a template - lets grab the first entry to search for
                # the methods
                object = object.values()[0]
            namedArgs = []
            isin = isinstance(object, itk.LightObject)
            if inspect.isclass(object):
                issub = issubclass(object, itk.LightObject)
            if isin or (inspect.isclass(object) and issub):
                namedArgs = [n[3:] for n in dir(object) if n.startswith("Set")]
        except Exception as e:
            print(e)
            continue
        for namedArg in namedArgs:
            if namedArg.startswith(text):
                argMatches.append(u"%s=" % namedArg)
    return argMatches

# install progress callback and custom completer if we are in ipython
# interpreter
try:
    import itkConfig
    import IPython
    if IPython.get_ipython():
        IPython.get_ipython().Completer.matchers.insert(0, ipython_kw_matches)
    # some cleanup
    del itkConfig, IPython
except (ImportError, AttributeError):
    # fail silently
    pass
