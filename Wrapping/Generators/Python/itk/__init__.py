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
# ==========================================================================

"""itk: Top-level container package for ITK wrappers."""

# Must import module from python path, not local reference
# in order to maintain the singular value of the module values.
# `import .conf.itkConfig` is a different context than
# `import itkConfig`, even if they are the same file. The
# LazyLoading and other values may be different in the two contexts.
from itkConfig import ITK_GLOBAL_VERSION_STRING as __version__

# Tests/lazy.py asserts itk.__package__ == "itk"; guard the invariant.
assert __package__ == "itk"

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

import threading as _threading

import itkConfig as _itkConfig
from itk.support import base as _base

# Recursive lock so transitive dependency loads from the same thread
# don't self-deadlock. Deliberately threading.RLock — a
# multiprocessing.RLock would pin the multiprocessing start method
# and break spawn/forkserver after `import itk`.
_lazy_load_lock = _threading.RLock()
_lazy_attribute_to_module: dict[str, str] = {}
_MISSING = object()


def __getattr__(name: str):
    if name.startswith("__") and name.endswith("__"):
        raise AttributeError(name)
    try:
        module_name = _lazy_attribute_to_module[name]
    except KeyError:
        raise AttributeError(
            f"module 'itk' has no attribute {name!r}"
        ) from None
    g = globals()
    with _lazy_load_lock:
        cached = g.get(name, _MISSING)
        if cached is not _MISSING:
            return cached
        namespace: dict = {}
        _base.itk_load_swig_module(module_name, namespace)
        g.update(namespace)
        if _itkConfig.DefaultFactoryLoading:
            _base.load_module_needed_factories(module_name)
    try:
        return g[name]
    except KeyError:
        raise AttributeError(
            f"module 'itk' has no attribute {name!r}"
        ) from None


def __dir__():
    return sorted(set(globals().keys()) | _lazy_attribute_to_module.keys())


def _initialize_module():
    """
    A function to explicitly avoid polluting the global namespace
    """
    import importlib
    import os
    import sys

    # Needed to avoid problem with aliasing of itk.set (itkTemplate)
    # inside the itk namespace.  We need to explicitly specify the
    # use of the builtin set
    from builtins import set as _builtin_set

    from .support import lazy as _lazy

    # Build the flat first-owner-wins map driving module-level
    # __getattr__, and populate base.itk_base_global_lazy_attributes
    # with the full set of owners per attribute (consumed by
    # support/template_class.py:_LoadModules).
    #
    # Precedence rules (mirroring the legacy _get_lazy_attributes +
    # __belong_lazy_attributes pipeline):
    #   - template_feature with _class_in_module=True: that module
    #     wins, even over a previously recorded owner.
    #   - template_feature with _class_in_module=False: first walk
    #     wins; later writers do not override.
    #   - snake_case_functions: first walk wins; never override.
    for module, data in _base.itk_base_global_module_data.items():
        for template_feature in data._template_feature_tuples:
            attr = template_feature._py_class_name
            _base.itk_base_global_lazy_attributes.setdefault(
                attr, _builtin_set()
            ).add(module)
            if template_feature._class_in_module:
                _lazy_attribute_to_module[attr] = module
            else:
                _lazy_attribute_to_module.setdefault(attr, module)
        for function in data._snake_case_functions:
            _base.itk_base_global_lazy_attributes.setdefault(
                function, _builtin_set()
            ).add(module)
            _lazy_attribute_to_module.setdefault(function, module)

    # Per-submodule LazyITKModule instances (itk.ITKCommon, ...).
    # Phase 03 will replace these with plain types.ModuleType + PEP 562
    # closures; for now they remain so the build stays green and the
    # pickle / cloudpickle paths exercised by Tests/lazy.py keep working.
    this_module = sys.modules[__name__]
    for module, data in _base.itk_base_global_module_data.items():
        attributes: dict[str, list[str]] = {}
        for template_feature in data._template_feature_tuples:
            if template_feature._class_in_module:
                attributes.setdefault(
                    template_feature._py_class_name, []
                ).insert(0, module)
            else:
                attributes.setdefault(
                    template_feature._py_class_name, []
                ).append(module)
        for function in data._snake_case_functions:
            attributes.setdefault(function, []).append(module)
        for k, v in attributes.items():
            seen = _builtin_set()
            attributes[k] = [m for m in v if not (m in seen or seen.add(m))]

        itk_module = _lazy.LazyITKModule(module, attributes)
        setattr(this_module, module, itk_module)

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


# Build the lazy-attribute map and materialise per-submodule
# LazyITKModule instances. This must run before any consumer attribute
# access; module-level __getattr__ depends on _lazy_attribute_to_module
# being populated.
_initialize_module()
