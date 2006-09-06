import os, os.path, sys, imp, inspect, itkConfig, itkTemplate

def LoadModule(name, namespace = None):
  """This function causes a SWIG module to be loaded into memory after its dependencies
  are satisfied. Information about the templates defined therein is looked up from 
  a config file, and PyTemplate instances for each are created. These template 
  instances are placed in a module with the given name that is either looked up 
  from sys.modules or created and placed there if it does not already exist.
  Optionally, a 'namespace' parameter can be provided. If it is provided, this
  namespace will be updated with the new template instantiations.
  The raw classes loaded from the named module's SWIG interface are placed in a 
  'swig' sub-module. If the namespace parameter is provided, this information will
  be placed in a sub-module named 'swig' therein as well. This latter submodule
  will be created if it does not already exist."""
  
  # find the module's name in sys.modules, or create a new module so named
  this_module = sys.modules.setdefault(name, imp.new_module(name))
 
  # if this library and it's template instantiations have already been loaded
  # into sys.modules, bail out after loading the defined symbols into 'namespace'
  if hasattr(this_module, '__templates_loaded'):
    if namespace is not None:
        swig = namespace.setdefault('swig', imp.new_module('swig'))
        swig.__dict__.update(this_module.swig.__dict__)

        # don't worry about overwriting the symbols in namespace -- any common
        # symbols should be of type itkTemplate, which is a singleton type. That
        # is, they are all identical, so replacing one with the other isn't a
        # problem.
        for k, v in this_module.__dict__.items():
          if not (k.startswith('_') or k == 'swig'): namespace[k] = v
    return
  
  # We're definitely going to load the templates. We set templates_loaded here
  # instead of at the end of the file to protect against cyclical dependencies
  # that could kill the recursive lookup below.
  this_module.__templates_loaded = True 
  
  # For external projects :
  # If this_module name (variable name) is in the module_data dictionnary, then
  # this_module is an installed module (or a previously loaded module).
  # Otherwise, it may come from an external project. In this case, we must
  # search the Configuration/<name>Config.py file of this project.
  try:
    module_data[name]
  except:
    file = inspect.getfile(this_module)
    path = os.path.dirname(file)

    data = {}
    try:
      # for a linux tree
      execfile(os.path.join(path, 'Configuration', name + 'Config.py'), data)
    except:
      try:
        # for a windows tree
        execfile(os.path.join(path, '..', 'Configuration', name + 'Config.py'), data)
      except:
        data=None
    if(data):
      module_data[name] = data

  # Now, we we definitely need to load the template instantiations from the
  # named module, and possibly also load the underlying SWIG module. Before we
  # can load the template instantiations of this module, we need to load those
  # of the modules on which this one depends. Ditto for the SWIG modules.
  # So, we recursively satisfy the dependencies of named module and create the
  # template instantiations.
  # Dependencies are looked up from the auto-generated configuration files, via 
  # the module_data instance defined at the bottom of this file, which knows how
  # to find those configuration files.
  data = module_data[name]
  if data:
    deps = list(data['depends'])
    deps.sort()
    for dep in deps:
      LoadModule(dep, namespace)
  
  if itkConfig.ImportCallback: itkConfig.ImportCallback(name, 0)
  
  # SWIG-generated modules have 'Python' appended. Only load the SWIG module if
  # we haven't already.
  swigModuleName = name + "Python"
  loader = LibraryLoader()
  if not swigModuleName in sys.modules: module = loader.load(swigModuleName)
  
  # OK, now the modules on which this one depends are loaded and template-instantiated,
  # and the SWIG module for this one is also loaded.
  # We're going to put the things we load and create in two places: the optional 
  # 'namespace' parameter, and the this_module variable's namespace.
  
  # make a new 'swig' sub-module for this_module. Also look up or create a 
  # different 'swig' module for 'namespace'. Since 'namespace' may be used to 
  # collect symbols from multiple different ITK modules, we don't want to 
  # stomp on an existing 'swig' module, nor do we want to share 'swig' modules
  # between this_module and namespace.
  
  this_module.swig = imp.new_module('swig')
  if namespace is not None: swig = namespace.setdefault('swig', imp.new_module('swig'))
  for k, v in module.__dict__.items():
    if not k.startswith('__'): setattr(this_module.swig, k, v)
    if namespace is not None and not k.startswith('__'): setattr(swig, k, v)

  data = module_data[name]
  if data:
    for template in data['templates']:
      if len(template) == 4: 
        # this is a template description      
        pyClassName, cppClassName, swigClassName, templateParams = template
        # It doesn't matter if an itkTemplate for this class name already exists
        # since every instance of itkTemplate with the same name shares the same
        # state. So we just make a new instance and add the new templates.
        templateContainer = itkTemplate.itkTemplate(cppClassName)
        try: templateContainer.__add__(templateParams, getattr(module, swigClassName))
        except Exception, e: DebugPrintError("%s not loaded from module %s because of exception:\n %s" %(swigClassName, name, e))
        setattr(this_module, pyClassName, templateContainer)
        if namespace is not None:
          current_value = namespace.get(pyClassName)
          if current_value != None and current_value != templateContainer:
            DebugPrintError("Namespace already has a value for %s, which is not an itkTemplate instance for class %s. Overwriting old value." %(pyClassName, cppClassName))
          namespace[pyClassName] = templateContainer
        
      else:
        # this is a description of a non-templated class
        pyClassName, cppClassName, swigClassName = template
        try: swigClass = getattr(module, swigClassName)
        except Exception, e: DebugPrintError("%s not found in module %s because of exception:\n %s" %(swigClassName, name, e))
        itkTemplate.registerNoTpl(cppClassName, swigClass)
        setattr(this_module, pyClassName, swigClass)
        if namespace is not None:
          current_value = namespace.get(pyClassName)
          if current_value != None and current_value != swigClass:
            DebugPrintError("Namespace already has a value for %s, which is not class %s. Overwriting old value." %(pyClassName, cppClassName))
          namespace[pyClassName] = swigClass
  
  if itkConfig.ImportCallback: itkConfig.ImportCallback(name, 1)
  
def DebugPrintError(error):
  if itkConfig.DebugLevel == itkConfig.WARN:
    print >> sys.stderr, error
  elif itkConfig.DebugLevel == itkConfig.ERROR:
    raise RuntimeError(error)

class LibraryLoader(object):
  """Do all the work to set up the environment so that a SWIG-generated library
  can be properly loaded. This invloves setting paths, etc., defined in itkConfig."""  
  
  # To share symbols across extension modules, we must set
  #     sys.setdlopenflags(dl.RTLD_NOW|dl.RTLD_GLOBAL)
  #
  # Since RTLD_NOW==0x002 and RTLD_GLOBAL==0x100 very commonly
  # we will just guess that the proper flags are 0x102 when there
  # is no dl module. On darwin (where there is not yet a dl module)
  # we need different flags because RTLD_GLOBAL = 0x008.
  darwin_dlopenflags  = 0xA
  generic_dlopenflags = 0x102
    
  if sys.platform.startswith('darwin'):
    dlopenflags = darwin_dlopenflags
  elif sys.platform.startswith('win'):
    dlopenflags = None
  else:
    dlopenflags = generic_dlopenflags
  # now try to refine the dlopenflags if we have a dl module.
  try:
    import dl
    dlopenflags = dl.RTLD_NOW|dl.RTLD_GLOBAL
  except:
    pass
    
  
  def setup(self):
    self.old_cwd = os.getcwd()
    try:
      os.chdir(itkConfig.swig_lib)
    except OSError:
      # silently pass to avoid the case where the dir is not there
      pass
    self.old_path = sys.path
    sys.path = [itkConfig.swig_lib, itkConfig.swig_py] + sys.path
    try:
      self.old_dlopenflags = sys.getdlopenflags()
      sys.setdlopenflags(self.dlopenflags)
    except:
      self.old_dlopenflags = None
  
  def load(self, name):
    self.setup()
    try:
      fp = None # needed in case next line raises exception, so that finally block works
      fp, pathname, description = imp.find_module(name)
      return imp.load_module(name, fp, pathname, description)
    finally:
      # Since we may exit via an exception, close fp explicitly.
      if fp: fp.close()
      self.cleanup()
  
  def cleanup(self):
    os.chdir(self.old_cwd)
    sys.path = self.old_path
    if self.old_dlopenflags:
      try: sys.setdlopenflags(self.old_dlopenflags)
      except: pass


# Make a list of all know modules (described in *Config.py files in the 
# config_py directory) and load the information described in those Config.py
# files.
dirs = [p for p in itkConfig.path if os.path.isdir(p)]
module_data = {}
for d in dirs:
  known_modules = [f[:-9] for f in os.listdir(d+os.sep+"Configuration") if f.endswith('Config.py')]
  known_modules.sort()
  sys.path.append(d)
  sys.path.append(d+os.sep+".."+os.sep+"lib")
  
  for module in known_modules:
    data = {}
    execfile(os.path.join(d+os.sep+"Configuration", module + 'Config.py'), data)
    module_data[module] = data
