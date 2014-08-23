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

import types
import inspect
import os
import warnings
import itkConfig
from itkTypes import itkCType


def itkFormatWarning(msg, *a):
    """"Format the warnings issued by itk to display only the message.

    This will ignore the filename and the linenumber where the warning was
    triggered. The message is returned to the warnings module.
    """

    return str(msg) + '\n'

# Redefine the format of the warnings
warnings.formatwarning = itkFormatWarning


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

    """This class manage access to avaible template arguments of C++ class

    There is 2 ways to access types:
    - with a dict interface. The user can manipulate template parameters nearly
    as it do in c++, excepted that the available parameters sets are choosed at
    build time. It is also possible, with the dict interface, to explore the
    available parameters sets.
    - with object attributes. The user can easily find the available parameters
    sets by pressing tab in interperter like ipython
    """
    __templates__ = {}
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
                new_instance.__template__ = {}
                cls.__named_templates__[name] = new_instance
        return cls.__named_templates__[name]

    def __add__(self, paramSetString, cl):
        """add a new argument set and the resulting class to the template

        paramSetString is the c++ string which define the parameters set
        cl is the class which correspond to the couple template-argument set
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
                import re
                shortNameSize = len(re.sub(r':.*:', '', self.__name__))
                attributeName = cl.__name__[shortNameSize:]
        elif cl.__name__.startswith("vcl_complex"):
            # C++ name is likely to be std::complex here, instead of the
            # expected vcl_complex
            attributeName = cl.__name__[len("vcl_complex"):]
        else:
            import re
            shortNameSize = len(re.sub(r'.*::', '', self.__name__))
            attributeName = cl.__name__[shortNameSize:]

        if attributeName.isdigit():
            # the attribute name can't be a number
            # add a single undescore before it to build a valid name
            attributeName = "_" + attributeName

        # add the attribute to this object
        self.__dict__[attributeName] = cl

    def __find_param__(self, paramSetString):
        """find the parameters of the template

        paramSetString is the c++ string which define the parameters set

        __find_param__ returns a list of itk classes, itkCType, and/or numbers
        which correspond to the parameters described in paramSetString.
        The parameters MUST have been registered before calling this method,
        or __find_param__ will return a string and not the wanted object, and
        will display a warning. Registration order is important.

        This method is not static only to be able to display the template name
        in the warning
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
        """return the class which correspond to the given template parameters

        parameters can be:
            - a single parameter (Ex: itk.Index[2])
            - a list of element (Ex: itk.Image[itk.UC, 2])
        """

        isin = isinstance(parameters, types.TupleType)
        if not isin and not isinstance(parameters, types.ListType):
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
            raise KeyError(
                'itkTemplate : No template %s for the %s class' %
                (str(parameters), self.__name__))

    def __repr__(self):
        return '<itkTemplate %s>' % self.__name__

    def __getattribute__(self, attr):
        """Support for reading doxygen man pages to produce __doc__ strings"""
        root = itkTemplate.__doxygen_root__
        indoc = (attr == '__doc__')
        if indoc and root != "" and self.__name__.startswith('itk'):
            try:
                import commands
                doxyname = self.__name__.replace("::", "_")
                man_path = "%s/man3/%s.3" % (root, doxyname)
                bzman_path = "%s/man3/%s.3.bz2" % (root, doxyname)
                if os.path.exists(bzman_path):
                        return (
                            commands.getoutput(
                                "bunzip2 --stdout '" + bzman_path +
                                "' | groff -mandoc -Tascii -c"))
                elif os.path.exists(man_path):
                    # Use groff here instead of man because man dies when it is
                    # passed paths with spaces (!) groff does not.
                    return (
                        commands.getoutput(
                            "groff -mandoc -Tascii -c '" +
                            man_path + "'"))
                else:
                    return (
                        "Cannot find man page for %s: %s"
                        % (self.__name__, man_path + "[.bz2]"))
            except Exception as e:
                return (
                    "Cannot display man page for %s due to exception: %s."
                    % (self.__name__, e))
        else:
            return object.__getattribute__(self, attr)

    def New(self, *args, **kargs):
        """TODO: some doc! Don't call it __call__ as it break the __doc__
        attribute feature in ipython"""
        import itk
        keys = self.keys()
        if len(args) != 0:
            # try to find a type suitable for the input provided
            input_types = [output(f).__class__ for f in args]
            keys = [k for k in self.keys() if k[0] == input_types[0]]
        cur = itk.auto_pipeline.current
        if cur is not None and len(cur) != 0:
            # try to find a type suitable for the input provided
            input_type = output(cur).__class__
            keys = [k for k in self.keys() if k[0] == input_type]
        if len(keys) == 0:
            raise RuntimeError("No suitable template parameter can be found.")
        return self[keys[0]].New(*args, **kargs)

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
        for key_tuple in self.__template__:
            key = str(key_tuple)
            if "itkCType" in key:
                ctypes.append(key)
            elif "class" in key:
                classes.append(key)

        # Sort the lists
        ctypes = sorted(ctypes)
        classes = sorted(classes)

        return ctypes + classes


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

    if callback:
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
    import sys
    print(
        ("WrapITK warning: itk.image() is deprecated. "
            "Use itk.output() instead."), file=sys.stderr)
    return output(input)
