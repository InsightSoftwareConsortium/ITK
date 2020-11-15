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

"""itk: Top-level container package for ITK wrappers."""

from .conf.itkConfig import ITK_GLOBAL_VERSION_STRING as __version__

from .support.itkExtras import *
from .support.itkInitHelpers import *
from .support.itkTypes import *


def _initialize_module():
    """
    A function to explicitly avoid polluting the global namespace
    """

    def _get_lazy_attributes(local_lazy_attributes, l_module, l_data):
        template_feature_tuples = [
            (t[0], t[3]) if len(t) > 3 else (t[0], False) for t in l_data["templates"]
        ]
        for (template_name, is_in_library) in template_feature_tuples:
            if is_in_library:
                # insert in front front if in library
                local_lazy_attributes.setdefault(template_name, []).insert(0, l_module)
            else:
                # append to end
                local_lazy_attributes.setdefault(template_name, []).append(l_module)
        if "snake_case_functions" in l_data:
            for function in l_data["snake_case_functions"]:
                # snake case always appended to end
                local_lazy_attributes.setdefault(function, []).append(l_module)

    from .support import itkBase as _itkBase
    from .support import itkLazy as _itkLazy
    from .conf import itkConfig as _itkConfig
    import sys

    if _itkConfig.LazyLoading:
        # If we are loading lazily (on-demand), make a dict mapping the available
        # classes/functions/etc. (read from the configuration modules) to the
        # modules they are declared in. Then pass that dict to a LazyITKModule
        # instance and (later) do some surgery on sys.modules so that the 'itk'
        # module becomes that new instance instead of what is executed from this
        # file.
        lazy_attributes = {}
        for module, data in _itkBase.itk_base_global_module_data.items():
            _get_lazy_attributes(lazy_attributes, module, data)

        if isinstance(sys.modules[__name__], _itkLazy.LazyITKModule):
            # Handle reload case where we've already done this once.
            # If we made a new module every time, multiple reload()s would fail
            # because the identity of sys.modules['itk'] would always be changing.
            sys.modules[__name__].__init__(__name__, lazy_attributes)
            del lazy_attributes
        else:
            # Create a new LazyITKModule
            lzy_module = _itkLazy.LazyITKModule(__name__, lazy_attributes)

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
        for module in _itkBase.itk_base_global_module_data.keys():
            _itkBase.itk_load_swig_module(module, sys.modules[__name__].__dict__)

    # Populate itk.ITKModuleName
    for module, data in _itkBase.itk_base_global_module_data.items():
        attributes = {}
        _get_lazy_attributes(attributes, module, data)
        itk_module = _itkLazy.LazyITKModule(module, attributes)
        setattr(sys.modules[__name__], module, itk_module)


# After 'lifting' external symbols into this itk namespace,
# Now do the initialization, and conversion to LazyLoading if necessary
_initialize_module()
