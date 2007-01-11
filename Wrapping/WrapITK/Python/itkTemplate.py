import types
import inspect
import sys
import os
import itkConfig
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
  
  name = name.replace(" ","")
  name = name.replace("*","")
  
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
    # Singleton pattern: we only make a single instance of any Template of a
    # given name. If we have already made the instance, just return it as-is.
    if not cls.__named_templates__.has_key(name):
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
    normalizedFullName = normalizeName(self.__name__+"<"+paramSetString+">")
    
    # the full class should not be already registered. If it is, there is a problem
    # somewhere so warn the user so he can fix the problem
    if itkTemplate.__templates__.has_key( normalizedFullName ) :
      print >>sys.stderr, "Warning: templated class already defined '%s'" % normalizedFullName
    # register the class
    itkTemplate.__templates__[normalizedFullName] = cl

    # __find_param__ will parse the paramSetString and produce a list of the same
    # parameters transformed in corresponding python classes.
    # we transform this list in tuple to make it usable as key of the dict
    param = tuple( self.__find_param__( paramSetString ) )
    
    # once again, warn the user if the tuple of parameter is already defined
    # so he can fix the problem
    if self.__template__.has_key( param ) :
      print >>sys.stderr,"Warning: template %s\n  already defined as %s\n  is redefined as    %s" % (normalizedFullName, self.__template__[param], cl)
    # and register the parameter tuple
    self.__template__[param] = cl

    # add in __class_to_template__ dictionary
    itkTemplate.__class_to_template__[cl] = (self, param)
    
    # now populate the template
    # 2 cases:
    # - the template is a SmartPointer. In that case, the attribute name will be the
    #   full real name of the class without the itk prefix and _Pointer suffix
    # - the template is not a SmartPointer. In that case, we keep only the end of the
    #   real class name which is a short string discribing the template arguments
    #   (for example IUC2)
    if cl.__name__.endswith("_Pointer") :
      # it's a SmartPointer
      attributeName = cl.__name__[len("itk"):-len("_Pointer")]
    else :
      # it's not a SmartPointer
      # we need to now the size of the name to keep only the suffix
      # short name does not contain :: and nested namespace
      # itk::Numerics::Sample -> itkSample
      import re
      shortNameSize = len(re.sub(r':.*:', '', self.__name__))
      attributeName = cl.__name__[shortNameSize:]
      
    if attributeName.isdigit() :
      # the attribute name can't be a number
      # add a single undescore before it to build a valid name
      attributeName = "_" + attributeName
      
    # add the attribute to this object
    self.__dict__[attributeName] = cl
    
    # now replace New method by a custom one
    if hasattr(cl, 'New') :
      # the new method needs to call the old one, so keep it with another (hidden) name
      cl.__New_orig__ = cl.New
      cl.New = types.MethodType(New, cl)    

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
    for elt in part :
      if inner == 0 :
        paramStrings.append( elt )
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

      if itkTemplate.__templates__.has_key( paramNorm ) :
        # the parameter is registered.
        # just get the really class form the dictionary
        param = itkTemplate.__templates__[paramNorm]
        
      elif itkCType.GetCType( param ) :
        # the parameter is a c type
        # just get the itkCtype instance
        param = itkCType.GetCType( param )
        
      elif paramNorm.isdigit() :
        # the parameter is a number
        # convert the string to a number !
        param = int(param)
        
      else :
        # unable to convert the parameter
        # use it without changes, but display a warning message, to incite
        # developer to fix the problem
        print >>sys.stderr,"Warning: Unknown parameter '%s' in template '%s'" % (param, self.__name__)
      
      parameters.append( param )

    return parameters
    

  def __getitem__(self, parameters):
    """return the class which correspond to the given template parameters
    
    parameters can be:
      - a single parameter (Ex: itk.Index[2])
      - a list of element (Ex: itk.Image[itk.UC, 2])
    """
    
    if type(parameters) != types.TupleType and type(parameters) != types.ListType :
      # parameters is a single element.
      # include it in a list to manage the 2 cases in the same way
      parameters = [parameters]
        
    cleanParameters = []
    for param in parameters:
      # In the case of itk SmartPointer, get the pointed object class
      try: param = param.GetPointer()
      except:pass
      
      # In the case where elt is a pointer (<className>Ptr), the real class
      # can be found in the pointer class dictionary
      try: param = param.__dict__[ param.__class__ ]
      except: pass

      # In the case of itk class instance, get the class
      if not inspect.isclass( param ) and param.__class__.__name__[:3] == 'itk' and param.__class__.__name__!= "itkCType" :
        param = param.__class__

      # append the parameter to the list. If it's not a supported type, it is
      # not in the dictionary and we will raise an exception below
      cleanParameters.append( param )

    try:
      return(self.__template__[tuple(cleanParameters)])
    except:
      raise KeyError, 'itkTemplate : No template %s for the %s class' % (str(parameters), self.__name__)


  def __repr__(self):
    return '<itkTemplate %s>' % self.__name__

  # support for reading doxygen man pages to produce __doc__ strings
  def __getattribute__(self, attr):
    if attr == '__doc__' and itkTemplate.__doxygen_root__ != "" and self.__name__.startswith('itk'):
      try:
        import commands
        doxyname = self.__name__.replace("::", "_")
        man_path = "%s/man3/%s.3" %(itkTemplate.__doxygen_root__, doxyname)
        bzman_path = "%s/man3/%s.3.bz2" %(itkTemplate.__doxygen_root__, doxyname)
        if os.path.exists(bzman_path):
	    return commands.getoutput("bunzip2 --stdout '"+bzman_path+"' | groff -mandoc -Tascii -c")
        elif os.path.exists(man_path):
          # Use groff here instead of man because man dies when it is passed paths with spaces (!)
          # groff does not.
          return commands.getoutput("groff -mandoc -Tascii -c '" + man_path +"'")
        else:
          return "Cannot find man page for %s in %s." %(self.__name__, itkTemplate.__doxygen_root__)
      except Exception, e:
        return "Cannot display man page for %s due to exception: %s." %(self.__name__, e)
    else:
      return object.__getattribute__(self, attr)

  def keys(self):
    return self.__template__.keys()

  # everything after this comment is for dict interface
  # and is a copy/paste from DictMixin
  # only methods to edit dictionary are not there
  def __iter__(self):
    for k in self.keys():
      yield k

  def has_key(self,key):
    try:
      value=self[key]
    except KeyError:
      return False
    return True

  def __contains__(self,key):
    return self.has_key(key)

  # third level takes advantage of second level definitions
  def iteritems(self):
    for k in self:
      yield (k,self[k])

  def iterkeys(self):
    return self.__iter__()

  # fourth level uses definitions from lower levels
  def itervalues(self):
    for _,v in self.iteritems():
      yield v

  def values(self):
    return [v for _,v in self.iteritems()]

  def items(self):
    return list(self.iteritems())

  def get(self,key,default=None):
    try:
      return self[key]
    except KeyError:
      return default

  def __len__(self):
    return len(self.keys())



# create a new New function which accepts parameters
def New(self, *args, **kargs) :
  import sys
  
  newItkObject = self.__New_orig__()
  
  # try to get the images from the filters in args
  args = [image(arg) for arg in args]
  
  # args without name are filter used to set input image
  #
  # count SetInput calls to call SetInput, SetInput2, SetInput3, ...
  # usefull with filter which take 2 input (or more) like SubstractImageFiler
  # Ex: substract image2.png to image1.png and save the result in result.png
  # r1 = itk.ImageFileReader.US2.New(FileName='image1.png')
  # r2 = itk.ImageFileReader.US2.New(FileName='image2.png')
  # s = itk.SubtractImageFilter.US2US2US2.New(r1, r2)
  # itk.ImageFileWriter.US2.New(s, FileName='result.png').Update()
  try :
    for setInputNb, arg  in enumerate(args) :
      methodName = 'SetInput%i' % (setInputNb+1)
      if methodName in dir(newItkObject) :
        # first try to use methods called SetInput1, SetInput2, ...
        # those method should have more chances to work in case of multiple
        # input types
        getattr(newItkObject, methodName)(arg)
      else :
        # no method called SetInput?
        # try with the standard SetInput(nb, input)
        newItkObject.SetInput(setInputNb, arg)
  except TypeError, e :
    # the exception have (at least) to possible reasons:
    # + the filter don't take the input number as first argument
    # + arg is an object of wrong type
    # 
    # if it's not the first input, re-raise the exception
    if setInputNb != 0 :
      raise e
    # it's the first input, try to use the SetInput() method without input number
    newItkObject.SetInput(args[0])
    # but raise an exception if there is more than 1 argument
    if len(args) > 1 :
      raise TypeError('Object accept only 1 input.')
  except AttributeError :
    # There is no SetInput() method, try SetImage
    # but before, check the number of inputs
    if len(args) > 1 :
      raise TypeError('Object accept only 1 input.')
    methodList = ['SetImage', 'SetInputImage']
    methodName = None
    for m in methodList:
      if m in dir(newItkObject):
        methodName = m
    if methodName :
      getattr(newItkObject, methodName)(args[0])
    else:
      raise AttributeError('No method found to set the input.')
    
  # named args : name is the function name, value is argument(s)
  for attribName, value in kargs.iteritems() :
    # use Set as prefix. It allow to use a shorter and more intuitive
    # call (Ex: itk.ImageFileReader.UC2.New(FileName='image.png')) than with the
    # full name (Ex: itk.ImageFileReader.UC2.New(SetFileName='image.png'))
    attrib = getattr(newItkObject, 'Set' + attribName)
    attrib(value)

  # now, try to add observer to display progress
  if itkConfig.ProgressCallback :
    import itk
    # copy the callback so it can be reset to None in itkConfig
    # without pb
    callback = itkConfig.ProgressCallback
    try :
      def progress() :
        # newItkObject and callback are kept referenced with a closure
        callback(self.__name__, newItkObject.GetProgress())

      command = itk.PyCommand.New()
      command.SetCommandCallable(progress)
      newItkObject.AddObserver(itk.ProgressEvent(), command.GetPointer())
    except :
      # it seems that something goes wrong...
      # as this feature is designed for prototyping, it's not really a problem
      # if an object  don't have progress reporter, so adding reporter can silently fail
      pass

  return newItkObject


def image(input) :
    try :
	img = input.GetOutput()
    except AttributeError :
	img = input
    try :
	img = img.GetPointer()
    except AttributeError :
	pass
    return img
