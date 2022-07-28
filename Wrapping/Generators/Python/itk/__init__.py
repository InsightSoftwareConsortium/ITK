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

"""itk: Top-level container package for ITK wrappers."""

# Must import module from python path, not local reference
# in order to maintain the singular value of the module values.
# `import .conf.itkConfig` is a different context than
# `import itkConfig`, even if they are the same file. The
# LazyLoading and other values may be different in the two contexts.
from itkConfig import ITK_GLOBAL_VERSION_STRING as __version__

from itk.support.extras import *
from itk.support.init_helpers import *
from itk.support.types import (
    itkCType,
    F,
    D,
    UC,
    US,
    UI,
    UL,
    SL,
    LD,
    ULL,
    SC,
    SS,
    SI,
    SLL,
    B,
    ST,
    IT,
    OT,
)


def _initialize_module():
    """
    A function to explicitly avoid polluting the global namespace
    """
    from .support.base import ITKModuleInfo, ITKTemplateFeatures

    # Needed to avoid problem with aliasing of itk.set (itkTemplate)
    # inside the itk namespace.  We need to explicitly specify the
    # use of the builtin set
    from builtins import set as _builtin_set

    def _get_lazy_attributes(local_lazy_attributes, l_module, l_data: ITKModuleInfo):
        """
        Set up lazy attribute relationships
        """
        for template_feature in l_data._template_feature_tuples:
            if template_feature._class_in_module:
                # insert in front front if in library
                local_lazy_attributes.setdefault(
                    template_feature._py_class_name, []
                ).insert(0, l_module)
            else:
                # append to end
                local_lazy_attributes.setdefault(
                    template_feature._py_class_name, []
                ).append(l_module)

        for function in l_data._snake_case_functions:
            # snake case always appended to end
            local_lazy_attributes.setdefault(function, []).append(l_module)
        # Remove duplicates in attributes, preserving only the first
        def _dedup(seq):
            seen = _builtin_set()
            seen_add = seen.add
            return [x for x in seq if not (x in seen or seen_add(x))]

        for k, v in local_lazy_attributes.items():
            local_lazy_attributes[k] = _dedup(v)

    from .support import base as _base
    from .support import lazy as _lazy
    from itkConfig import LazyLoading as _LazyLoading
    import sys
    import os
    import importlib

    if _LazyLoading:
        # If we are loading lazily (on-demand), make a dict mapping the available
        # classes/functions/etc. (read from the configuration modules) to the
        # modules they are declared in. Then pass that dict to a LazyITKModule
        # instance and (later) do some surgery on sys.modules so that the 'itk'
        # module becomes that new instance instead of what is executed from this
        # file.
        lazy_attributes = {}
        for module, data in _base.itk_base_global_module_data.items():
            _get_lazy_attributes(lazy_attributes, module, data)

        if isinstance(sys.modules[__name__], _lazy.LazyITKModule):
            # Handle reload case where we've already done this once.
            # If we made a new module every time, multiple reload()s would fail
            # because the identity of sys.modules['itk'] would always be changing.
            sys.modules[__name__].__init__(__name__, lazy_attributes)
            del lazy_attributes
        else:
            # Create a new LazyITKModule
            lzy_module = _lazy.LazyITKModule(__name__, lazy_attributes)

            # Pre-existing attributes need to be propagated too!
            # except for the lazy overridden elements
            exclusion_copy_list = ["__name__", "__loader__", "__builtins__"]
            for k, v in sys.modules[__name__].__dict__.items():
                if k not in exclusion_copy_list:
                    setattr(lzy_module, k, v)

            # Now override the default  sys.modules[__name__] (__name__  == 'itk' )
            sys.modules[__name__] = lzy_module
    else:
        # We're not lazy-loading. Just load the modules in the order specified in
        # the known_modules list for consistency.
        for module in _base.itk_base_global_module_data.keys():
            _base.itk_load_swig_module(module, sys.modules[__name__].__dict__)

    # Populate itk.ITKModuleName
    for module, data in _base.itk_base_global_module_data.items():
        attributes = {}
        _get_lazy_attributes(attributes, module, data)
        itk_module = _lazy.LazyITKModule(module, attributes)
        setattr(sys.modules[__name__], module, itk_module)

        # Check if the module installed its own init file and load it.
        # ITK Modules __init__.py must be renamed to __init_{module_name}__.py before packaging
        # the wheel to avoid overriding this file on installation.
        module_init_file = os.path.join(
            os.path.dirname(__file__), "__init_" + module.lower() + "__.py"
        )
        if not os.path.isfile(module_init_file):
            continue

        # Load the module definition from file path
        spec = importlib.util.spec_from_file_location(
            f"{module}.__init__", module_init_file
        )

        # Import and execute the __init__ file (this call will add the module binaries to sys path)
        loaded_module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(loaded_module)


# After 'lifting' external symbols into this itk namespace,
# Now do the initialization, and conversion to LazyLoading if necessary
_initialize_module()
