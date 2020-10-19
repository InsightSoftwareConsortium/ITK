# ==========================================================================
#
#   Copyright NumFOCUS
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
# ==========================================================================*/

import os
import sys
from sys import stderr as system_error_stream

# Required to work around weird import error with xarray
import pkg_resources

import itkConfig
import itkTemplate


def create_itk_module(name):
    from importlib.util import module_from_spec as ilu_module_from_spec
    from importlib.util import spec_from_file_location as ilu_spec_from_file_location

    swig_module_name = "itk." + name + "Python"
    spec = ilu_spec_from_file_location(
        swig_module_name,
        os.path.join(os.path.dirname(__file__), "itk", name + "Python.py"),
    )
    l_module = ilu_module_from_spec(spec)
    return l_module


def itk_load_swig_module(name, namespace=None):
    """This function causes a SWIG module to be loaded into memory after its
    dependencies are satisfied. Information about the templates defined therein
    is looked up from a config file, and PyTemplate instances for each are
    created. These template instances are placed in a module with the given
    name that is either looked up from sys.modules or created and placed there
    if it does not already exist.

    Optionally, a 'namespace' parameter can be provided. If it is provided,
    this namespace will be updated with the new template instantiations.

    The raw classes loaded from the named module's SWIG interface are placed in
    a 'swig' sub-module. If the namespace parameter is provided, this
    information will be placed in a sub-module named 'swig' therein as well.
    This later submodule will be created if it does not already exist."""

    swig_module_name = "itk." + name + "Python"
    # find the module's name in sys.modules, or create a new module so named
    this_module = sys.modules.setdefault(swig_module_name, create_itk_module(name))

    # if this library and it's template instantiations have already been loaded
    # into sys.modules, bail out after loading the defined symbols into
    # 'namespace'
    if hasattr(this_module, "__templates_loaded"):
        if namespace is not None:
            swig = namespace.setdefault("swig", {})
            assert hasattr(this_module, "swig")
            swig.update(this_module.swig)

            # don't worry about overwriting the symbols in namespace -- any
            # common symbols should be of type itkTemplate, which is a
            # singleton type. That is, they are all identical, so replacing one
            # with the other isn't a problem.
            for k, v in this_module.__dict__.items():
                if not (k.startswith("_") or k.startswith("itk") or k == "swig"):
                    namespace[k] = v
        return

    # We're definitely going to load the templates. We set templates_loaded
    # here instead of at the end of the file to protect against cyclical
    # dependencies that could kill the recursive lookup below.
    this_module.__templates_loaded = True

    # Now, we definitely need to load the template instantiations from the
    # named module, and possibly also load the underlying SWIG module. Before
    # we can load the template instantiations of this module, we need to load
    # those of the modules on which this one depends. Ditto for the SWIG
    # modules.
    # So, we recursively satisfy the dependencies of named module and create
    # the template instantiations.
    # Dependencies are looked up from the auto-generated configuration files,
    # via the itk_base_global_module_data instance defined at the bottom of this file, which
    # knows how to find those configuration files.
    l_data = itk_base_global_module_data[name]
    if l_data:
        deps = sorted(l_data["depends"])
        for dep in deps:
            itk_load_swig_module(dep, namespace)

    if itkConfig.ImportCallback:
        itkConfig.ImportCallback(name, 0)

    # SWIG-generated modules have 'Python' appended. Only load the SWIG module
    # if we haven't already.
    loader = LibraryLoader()
    l_module = loader.load(swig_module_name)

    # OK, now the modules on which this one depends are loaded and
    # template-instantiated, and the SWIG module for this one is also loaded.
    # We're going to put the things we load and create in two places: the
    # optional 'namespace' parameter, and the this_module variable's namespace.

    # Populate the 'swig' sub-module namespace for this_module. Also look up or create a
    # different 'swig' namespace for 'namespace'. Since 'namespace' may be used to
    # collect symbols from multiple different ITK modules, we don't want to
    # stomp on an existing 'swig' namespace, nor do we want to share 'swig'
    # namespaces between this_module and namespace.

    if namespace is None:
        for k, v in l_module.__dict__.items():
            if not (k.startswith("__") or k.startswith("itk")):
                this_module.swig[k] = v
    else:
        swig = None
        if namespace is not None:
            swig = namespace.setdefault("swig", {})
        assert swig is not None
        for k, v in l_module.__dict__.items():
            if not (k.startswith("__") or k.startswith("itk")):
                this_module.swig[k] = v
                swig[k] = v

    l_data = itk_base_global_module_data[name]
    if l_data:
        for template in l_data["templates"]:
            if len(template) == 5:
                # This is a template description
                (
                    py_class_name,
                    cpp_class_name,
                    swig_class_name,
                    class_in_module,
                    template_params,
                ) = template
                # It doesn't matter if an itkTemplate for this class name
                # already exists since every instance of itkTemplate with the
                # same name shares the same state. So we just make a new
                # instance and add the new templates.
                template_container = itkTemplate.itkTemplate(cpp_class_name)
                try:
                    template_container.__add__(
                        template_params, getattr(l_module, swig_class_name)
                    )
                    setattr(this_module, py_class_name, template_container)
                    if namespace is not None:
                        current_value = namespace.get(py_class_name)
                        if (
                            current_value is not None
                            and current_value != template_container
                        ):
                            DebugPrintError(
                                "Namespace already has a value for"
                                " %s, which is not an itkTemplate"
                                "instance for class %s. "
                                "Overwriting old value."
                                % (py_class_name, cpp_class_name)
                            )
                        namespace[py_class_name] = template_container
                except Exception as e:
                    DebugPrintError(
                        "%s not loaded from module %s because of "
                        "exception:\n %s" % (swig_class_name, name, e)
                    )

            else:
                # this is a description of a non-templated class
                # It may have 3 or 4 arguments, the last one can be a boolean value
                if len(template) == 4:
                    (
                        py_class_name,
                        cpp_class_name,
                        swig_class_name,
                        class_in_module,
                    ) = template
                else:
                    py_class_name, cpp_class_name, swig_class_name = template
                try:
                    swigClass = getattr(l_module, swig_class_name)
                    itkTemplate.registerNoTpl(cpp_class_name, swigClass)
                    setattr(this_module, py_class_name, swigClass)
                    if namespace is not None:
                        current_value = namespace.get(py_class_name)
                        if current_value is not None and current_value != swigClass:
                            DebugPrintError(
                                "Namespace already has a value for"
                                " %s, which is not class %s. "
                                "Overwriting old value."
                                % (py_class_name, cpp_class_name)
                            )
                        namespace[py_class_name] = swigClass
                except Exception as e:
                    DebugPrintError(
                        "%s not found in module %s because of "
                        "exception:\n %s" % (swig_class_name, name, e)
                    )
        if "snake_case_functions" in l_data:
            for snakeCaseFunction in l_data["snake_case_functions"]:
                namespace[snakeCaseFunction] = getattr(l_module, snakeCaseFunction)
                init_name = snakeCaseFunction + "_init_docstring"
                init_function = getattr(l_module, init_name)
                try:
                    init_function()
                except AttributeError:
                    pass

    if itkConfig.ImportCallback:
        itkConfig.ImportCallback(name, 1)


def DebugPrintError(error):
    if itkConfig.DebugLevel == itkConfig.WARN:
        print(error, file=system_error_stream)
    elif itkConfig.DebugLevel == itkConfig.ERROR:
        raise RuntimeError(error)


class LibraryLoader(object):

    """Do all the work to set up the environment so that a SWIG-generated
    library can be properly loaded. This involves setting paths defined in
    itkConfig."""

    def __init__(self):
        self.old_path = sys.path
        self.old_cwd = os.getcwd()

    def setup(self):
        self.old_cwd = os.getcwd()
        try:
            os.chdir(itkConfig.swig_lib)
        except OSError:
            # silently pass to avoid the case where the dir is not there
            pass
        self.old_path = sys.path
        sys.path = [itkConfig.swig_lib, itkConfig.swig_py, itkConfig.path] + sys.path

    def load(self, name):
        self.setup()
        try:
            import importlib

            l_module = importlib.import_module(name)
            # since version 3.4: Use importlib.util.find_spec() instead.
            l_spec = importlib.util.find_spec(name)
            l_spec.loader.exec_module(l_module)
            return l_module
        finally:
            self.cleanup()

    def cleanup(self):
        os.chdir(self.old_cwd)
        sys.path = self.old_path


def _initialize(l_module_data):
    # Make a list of all know modules (described in *Config.py files in the
    # config_py directory) and load the information described in those Config.py
    # files.
    dirs = [p for p in itkConfig.path if os.path.isdir(p)]

    for d in dirs:
        files = os.listdir(d + os.sep + "Configuration")
        known_modules = sorted([f[:-9] for f in files if f.endswith("Config.py")])
        sys.path.append(d)
        sys.path.append(d + os.sep + ".." + os.sep + "lib")

        for module in known_modules:
            data = {}
            conf = module + "Config.py"
            path = os.path.join(d + os.sep + "Configuration", conf)
            with open(path, "rb") as module_file:
                exec(module_file.read(), data)
            snake_data = {}
            snake_conf = module + "_snake_case.py"
            snake_path = os.path.join(d + os.sep + "Configuration", snake_conf)
            if os.path.exists(snake_path):
                with open(snake_path, "rb") as snake_module_file:
                    exec(snake_module_file.read(), snake_data)
            data.update(snake_data)
            l_module_data[module] = data


itk_base_global_lazy_attributes = {}
itk_base_global_known_modules = []
itk_base_global_module_data = {}
_initialize(itk_base_global_module_data)
del _initialize
