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

"""Builder for per-submodule lazy namespaces (``itk.ITKCommon``, ...).

Each submodule is a plain ``types.ModuleType`` instance wired with PEP 562
``__getattr__`` / ``__dir__`` callables, registered in
``sys.modules['itk.<Module>']`` so ``cloudpickle`` (used by the Dask
worker round-trip exercised in ``Tests/lazy.py``) can re-import it by
its dotted name.
"""

import importlib
import importlib.util
import sys
import types

import itkConfig as _itkConfig
from itk.support import base as _base


_MISSING = object()


def _make_itk_lazy_submodule(
    module_name: str,
    lazy_attributes: dict[str, list[str]],
    get_module_load_lock,
) -> types.ModuleType:
    """Build the lazy ``itk.<module_name>`` namespace.

    Parameters
    ----------
    module_name
        Bare submodule name (e.g. ``"ITKCommon"``); the resulting
        module is registered as ``itk.<module_name>``.
    lazy_attributes
        Mapping from attribute name to the list of owning submodule
        names. The first element of each list wins (matching the
        legacy ``__belong_lazy_attributes`` first-owner-wins rule).
    get_module_load_lock
        Callable ``(swig_module_name: str) -> RLock`` from
        ``itk/__init__.py`` that returns a per-SWIG-module RLock so
        unrelated SWIG modules can first-load in parallel while loads
        of the same module remain serialised across both top-level
        and per-submodule lazy paths.
    """
    m = types.ModuleType(f"itk.{module_name}")
    m.__package__ = "itk"  # PEP 366
    # PEP 451 ModuleSpec so importlib.util.find_spec, inspect, and IDE
    # introspection treat the synthetic submodule as a normal module.
    m.__spec__ = importlib.util.spec_from_loader(
        m.__name__, loader=None, origin="itk lazy submodule"
    )
    m.__loader__ = m.__spec__.loader

    belong: dict[str, str] = {k: v[0] for k, v in lazy_attributes.items() if v}

    def __getattr__(name: str):
        if name.startswith("__") and name.endswith("__"):
            raise AttributeError(name)
        try:
            target = belong[name]
        except KeyError:
            raise AttributeError(
                f"module {m.__name__!r} has no attribute {name!r}"
            ) from None
        with get_module_load_lock(target):
            cached = vars(m).get(name, _MISSING)
            if cached is not _MISSING:
                return cached
            namespace: dict = {}
            _base.itk_load_swig_module(target, namespace)
            for _attr_name, _attr_value in namespace.items():
                setattr(m, _attr_name, _attr_value)
            if _itkConfig.DefaultFactoryLoading:
                _base.load_module_needed_factories(target)
        result = vars(m).get(name, _MISSING)
        if result is _MISSING:
            raise AttributeError(
                f"module {m.__name__!r} has no attribute {name!r}"
            ) from None
        return result

    def __dir__():
        # Set literal — avoids resolving the bare name `set`, which
        # gets shadowed in this submodule's __dict__ by `itk.set`
        # (itkTemplate std::set) once any SWIG module loads into it.
        return sorted({*m.__dict__.keys(), *belong.keys()})

    m.__getattr__ = __getattr__
    m.__dir__ = __dir__

    # Pickle shim: a bare ``types.ModuleType`` has no reducer, so
    # ``pickle.dumps`` raises ``TypeError: cannot pickle 'module'
    # object`` on CPython 3.12 even when the instance is registered in
    # ``sys.modules``. A per-instance ``__reduce_ex__`` makes pickle
    # round-trip via ``importlib.import_module(name)``, which then
    # resolves through ``sys.modules`` to the same instance. Set as
    # an instance attribute (not on ``types.ModuleType``) so unrelated
    # modules in the process are unaffected.
    submodule_dotted_name = m.__name__

    def __reduce_ex__(_protocol: int):
        return (importlib.import_module, (submodule_dotted_name,))

    m.__reduce_ex__ = __reduce_ex__

    sys.modules[submodule_dotted_name] = m
    return m
