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

import os
import sys
from sys import stderr as system_error_stream

# Required to work around weird import error with xarray
from typing import Dict, Any, List, Optional, Sequence, Union

import pkg_resources

import itkConfig
from itkConfig import DefaultFactoryLoading as _DefaultFactoryLoading
from itk.support.template_class import itkTemplate


def create_itk_module(name: str):
    from importlib.util import module_from_spec as ilu_module_from_spec
    from importlib.util import spec_from_file_location as ilu_spec_from_file_location

    swig_module_name: str = f"itk.{name}Python"
    spec = ilu_spec_from_file_location(
        swig_module_name,
        os.path.join(os.path.dirname(__file__), "..", f"{name}Python.py"),
    )
    l_module = ilu_module_from_spec(spec)
    return l_module


def itk_load_swig_module(name: str, namespace=None):
    """This function causes a SWIG module to be loaded into memory after its
    dependencies are satisfied. Information about the templates defined therein
    is looked up from a config file, and PyTemplate instances for each are
    created. These template_feature instances are placed in a module with the given
    name that is either looked up from sys.modules or created and placed there
    if it does not already exist.

    Optionally, a 'namespace' parameter can be provided. If it is provided,
    this namespace will be updated with the new template_feature instantiations.

    The raw classes loaded from the named module's SWIG interface are placed in
    a 'swig' sub-module. If the namespace parameter is provided, this
    information will be placed in a sub-module named 'swig' therein as well.
    This later submodule will be created if it does not already exist."""

    swig_module_name: str = f"itk.{name}Python"
    # find the module's name in sys.modules, or create a new module so named
    this_module = sys.modules.setdefault(swig_module_name, create_itk_module(name))

    # If this library and its template_feature instantiations have already been loaded
    # into sys.modules, bail out after loading the defined symbols into
    # 'namespace'
    if hasattr(this_module, "__templates_loaded"):
        if namespace is not None:
            swig = namespace.setdefault("swig", {})
            if hasattr(this_module, "swig"):
                swig.update(this_module.swig)

            # don't worry about overwriting the symbols in namespace -- any
            # common symbols should be of type itkTemplate, which is a
            # singleton type. That is, they are all identical, so replacing one
            # with the other isn't a problem.
            for k, v in this_module.__dict__.items():
                if not (k.startswith("_") or k.startswith("itk") or k == "swig"):
                    namespace[k] = v
        return

    # Before we can load the template_feature instantiations of this module,
    # we need to load those of the modules on which this one depends. Ditto
    # for the SWIG modules.
    # So, we recursively satisfy the dependencies of named module and create
    # the dependency template_feature instantiations.
    # Dependencies are looked up from the auto-generated configuration files,
    # via the itk_base_global_module_data instance defined at the bottom of this file, which
    # knows how to find those configuration files.
    l_data = itk_base_global_module_data[name]
    if l_data:
        deps = l_data.get_module_dependencies()
        for dep in deps:
            itk_load_swig_module(dep, namespace)

    # It is possible that template_feature instantiations from this module
    # were loaded as a side effect of dependency loading. For instance,
    # if this module defines a factory override for a base class in a
    # dependency module, that module will load and then immediately load
    # this module to ensure overrides are available.
    # We check whether this module and its template_feature instantiations have
    # already been loaded into sys.modules. If so we can copy into 'namespace'
    # and then exit early with success.
    this_module = sys.modules.setdefault(swig_module_name, create_itk_module(name))
    if hasattr(this_module, "__templates_loaded"):
        if namespace is not None:
            swig = namespace.setdefault("swig", {})
            if hasattr(this_module, "swig"):
                swig.update(this_module.swig)

            # don't worry about overwriting the symbols in namespace -- any
            # common symbols should be of type itkTemplate, which is a
            # singleton type. That is, they are all identical, so replacing one
            # with the other isn't a problem.
            for k, v in this_module.__dict__.items():
                if not (k.startswith("_") or k.startswith("itk") or k == "swig"):
                    namespace[k] = v
        return

    # All actions after this point should execute exactly once to properly load in
    # templates and factories.

    # Indicate that we are proceeding with loading templates for this module
    if itkConfig.ImportCallback:
        itkConfig.ImportCallback(name, 0)

    # SWIG-generated modules have 'Python' appended. Only load the SWIG module
    # if we haven't already.
    loader = LibraryLoader()
    l_module = loader.load(swig_module_name)

    # OK, now the modules on which this one depends are loaded and
    # template_feature-instantiated, and the SWIG module for this one is also loaded.
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
        swig = namespace.setdefault("swig", {})
        for k, v in l_module.__dict__.items():
            if not (k.startswith("__") or k.startswith("itk")):
                this_module.swig[k] = v
                swig[k] = v

    l_data: ITKModuleInfo = itk_base_global_module_data[name]
    for template_feature in l_data.get_all_template_features():
        if template_feature.is_itk_class():
            # Get the attribute associated with the class name if it exists,
            # otherwise make a new templated class
            # template_container =  this_module.'py_class_name'
            template_container = getattr(
                this_module,
                template_feature.get_python_class_name(),
                # Create a new template_container if not already found
                itkTemplate(template_feature.get_cpp_class_name()),
            )

            try:
                template_container.__add__(
                    template_feature.get_template_parameters(),
                    getattr(l_module, template_feature.get_swig_class_name()),
                )
                # Now set the updated template_container to this_module
                setattr(
                    this_module,
                    template_feature.get_python_class_name(),
                    template_container,
                )
                if namespace is not None:
                    current_value = namespace.get(
                        template_feature.get_python_class_name()
                    )
                    if (
                        current_value is not None
                        and current_value != template_container
                    ):
                        debug_print_error(
                            f"Namespace already has a value for "
                            f"{template_feature.get_python_class_name()}, which is not an itkTemplate "
                            f"instance for class {template_feature.get_cpp_class_name()}. "
                            f"Overwriting old value."
                        )
                    namespace[
                        template_feature.get_python_class_name()
                    ] = template_container
            except Exception as e:
                debug_print_error(
                    f"{template_feature.get_swig_class_name()} not loaded from module {name} because of "
                    f"exception:\n {e}"
                )
                pass

        else:
            # this is a description of a non-templated class
            try:
                swig_class = getattr(l_module, template_feature.get_swig_class_name())
                itkTemplate.registerNoTpl(
                    template_feature.get_cpp_class_name(), swig_class
                )
                setattr(
                    this_module, template_feature.get_python_class_name(), swig_class
                )
                if namespace is not None:
                    current_value = namespace.get(
                        template_feature.get_python_class_name()
                    )
                    if current_value is not None and current_value != swig_class:
                        debug_print_error(
                            f"Namespace already has a value for"
                            f" {template_feature.get_python_class_name()}, which is not class {template_feature.get_cpp_class_name()}. "
                            f"Overwriting old value."
                        )
                    namespace[template_feature.get_python_class_name()] = swig_class
            except Exception as e:
                debug_print_error(
                    f"{template_feature.get_swig_class_name()} not found in module {name} because of "
                    f"exception:\n {e}"
                )

    # Indicate that templates have been fully loaded from the module.
    # Any subsequent attempts to load the module will bail out early
    # if this flag is set.
    this_module.__templates_loaded = True

    # Load any modules that have been marked as defining overrides
    # for some base class(es) defined in the current module.
    # For instance, ITKImageIOBase will load any modules defining
    # ImageIO objects for reading different image file types.
    if _DefaultFactoryLoading:
        load_module_needed_factories(name)

    for snakeCaseFunction in l_data.get_snake_case_functions():
        namespace[snakeCaseFunction] = getattr(l_module, snakeCaseFunction)
        init_name = snakeCaseFunction + "_init_docstring"
        init_function = getattr(l_module, init_name)
        try:
            init_function()
        except AttributeError:
            pass

    if itkConfig.ImportCallback:
        itkConfig.ImportCallback(name, 1)


def debug_print_error(error):
    if itkConfig.DebugLevel == itkConfig.WARN:
        print(error, file=system_error_stream)
    elif itkConfig.DebugLevel == itkConfig.ERROR:
        raise RuntimeError(error)


class LibraryLoader:

    """Do all the work to set up the environment so that a SWIG-generated
    library can be properly loaded. This involves setting paths defined in
    itkConfig."""

    def __init__(self) -> None:
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
        sys.path = [itkConfig.swig_lib, itkConfig.swig_py] + sys.path

    def load(self, name: str):
        self.setup()
        try:
            import importlib

            l_module = importlib.import_module(name)
            # since version 3.4: Use importlib.util.find_spec() instead.
            l_spec = importlib.util.find_spec(name)
            l_spec.loader.exec_module(l_module)  # pytype: disable=attribute-error
            return l_module
        finally:
            self.cleanup()

    def cleanup(self):
        os.chdir(self.old_cwd)
        sys.path = self.old_path


class ITKTemplateFeatures:
    """
    Objects to hold the 'template' features specified in the '*Config.py'
    files generated during swig configuration.

    (py_class_name, cpp_class_name, swig_class_name, class_in_module, template_parameters)
    ('Image',       'itk::Image',   'itkImageSS2',   True,            'signed short,2'),
    """

    def __init__(self, feature_tuple: Sequence[Union[str, bool]]) -> None:
        feature_length: int = len(feature_tuple)
        # ITK classes have exactly 5 elements in the tuple, otherwise they are swig classes
        self._is_itk_class: bool = feature_length == 5
        if feature_length < 3 or feature_length > 5:
            raise Exception(
                f"ERROR: Ivalid number of features specified (3 <= {feature_length} <= 5): {feature_tuple}."
            )
        self._py_class_name: str = feature_tuple[0]
        self._cpp_class_name: str = feature_tuple[1]
        self._swig_class_name: str = feature_tuple[2]
        self._class_in_module: bool = feature_tuple[3] if feature_length >= 4 else False
        self._template_parameters: Optional[str] = (
            feature_tuple[4] if feature_length == 5 else None
        )

    def is_itk_class(self) -> bool:
        return self._is_itk_class

    def get_python_class_name(self) -> str:
        return self._py_class_name

    def get_cpp_class_name(self) -> str:
        return self._cpp_class_name

    def get_swig_class_name(self) -> str:
        return self._swig_class_name

    def get_class_in_module(self) -> bool:
        return self._class_in_module

    def get_template_parameters(self) -> str:
        return self._template_parameters


_factories_loaded = []


def load_factories(factory_name: str) -> None:
    """Load the factories of the given name.

    Supported names include:

        - ImageIO
        - MeshIO
        - TransformIO
        - FFTImageFilterInit
    """
    # Only load factories once
    if factory_name in _factories_loaded:
        return

    import itk

    for module_name, data in itk_base_global_module_data.items():
        for name, factory_class_prefix in data.get_module_factories()[:2]:
            if name == factory_name:
                # Get the factory, loading new modules with itk_load_swig_module as necessary
                namespace = dict()
                itk_load_swig_module(module_name, namespace)
                factory = namespace[f"{factory_class_prefix}{name}Factory"]
                # Static method initializes factory overrides without adding to auto_pipeline
                factory.RegisterOneFactory()
    _factories_loaded.append(factory_name)


# Modules that need to load the listed factories.
# The factories are loaded after the module is loaded, which ensures any dependent code
# like FFT-dependent code, will have its factories registered if there is
# a module dependency on the ITKFFT module.
# We need a separate function to initialize these because itk_load_swig_module calls itself recursively,
# and we want to avoid circular dependencies.
needed_factories = {
    "ITKIOImageBase": "ImageIO",
    "ITKIOMeshBase": "MeshIO",
    "ITKIOTransformBase": "TransformIO",
    "ITKFFT": "FFTImageFilterInit",
}


def load_module_needed_factories(name):
    if name in needed_factories.keys():
        load_factories(needed_factories[name])


class ITKModuleInfo:
    """
    A structure to hold information loaded from the *Config.py
    files generated during swig wrapping.  The *Config.py
    files define actual names of the swig wrapped classes
    so that they may be used to build convenience dispatch
    factories from the itkTemplate base class.
    """

    def __init__(self, conf: str, snake_conf) -> None:
        import importlib

        module_name = os.path.splitext(conf)[0]
        content_info = importlib.import_module(f"itk.Configuration.{module_name}")
        if hasattr(content_info, "templates"):
            _templates = content_info.templates
        else:
            _templates = tuple()
        if hasattr(content_info, "depends"):
            self._depends = content_info.depends
        else:
            self._depends = tuple()
        if hasattr(content_info, "factories"):
            self._factories = content_info.factories
        else:
            self._factories = tuple()
        self._template_feature_tuples: List[ITKTemplateFeatures] = [
            ITKTemplateFeatures(tfeat) for tfeat in _templates
        ]

        snake_module_name = os.path.splitext(snake_conf)[0]
        try:
            snake_content_info = importlib.import_module(
                f"itk.Configuration.{snake_module_name}"
            )
        except ImportError:
            self._snake_case_functions: Sequence[str] = []
            return
        if hasattr(snake_content_info, "snake_case_functions"):
            self._snake_case_functions: Sequence[
                str
            ] = snake_content_info.snake_case_functions
        else:
            self._snake_case_functions: Sequence[str] = []

    def get_module_dependencies(self) -> Sequence[str]:
        return self._depends

    def get_all_template_features(self) -> Sequence[ITKTemplateFeatures]:
        return self._template_feature_tuples

    def get_module_factories(self) -> Sequence[Sequence[str]]:
        return self._factories

    def get_snake_case_functions(self) -> Sequence[str]:
        return self._snake_case_functions


def _initialize(l_module_data):
    # Make a list of all know modules (described in *Config.py files in the
    # config_py directory) and load the information described in those Config.py
    # files.
    candidate_config_path: str = os.path.join(itkConfig.path, "Configuration")
    if not os.path.isdir(candidate_config_path):
        error_message: str = f"WARNING: Invalid configuration directory requested: {candidate_config_path}"
        raise RuntimeError(error_message)

    files = os.listdir(candidate_config_path)
    known_modules: List[str] = sorted(f[:-9] for f in files if f.endswith("Config.py"))
    for module in known_modules:
        conf: str = f"{module}Config.py"
        snake_conf = f"{module}_snake_case.py"

        l_module_data[module] = ITKModuleInfo(conf, snake_conf)


itk_base_global_lazy_attributes: Dict[str, Any] = {}
itk_base_global_module_data: Dict[str, ITKModuleInfo] = {}
_initialize(itk_base_global_module_data)
del _initialize
