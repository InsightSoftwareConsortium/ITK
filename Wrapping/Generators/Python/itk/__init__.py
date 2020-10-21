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

from itkConfig import ITK_GLOBAL_VERSION_STRING as _LOCAL_ITK_GLOBAL_VERSION_STRING

__version__ = _LOCAL_ITK_GLOBAL_VERSION_STRING
del _LOCAL_ITK_GLOBAL_VERSION_STRING


def _initialize_module():
    """
    A function to explicitly avoid polluting the global namespace
    """

    def _GetLazyAttributes(local_lazy_attributes, l_module, l_data):
        templateNames = [t[0] for t in l_data["templates"]]
        isInLibrary = [t[3] for t in l_data["templates"] if len(t) > 3]
        local_attributes = dict([(n, l_module) for n in templateNames])
        attributesInModule = dict(
            [(n, belongs) for n, belongs in zip(templateNames, isInLibrary)]
        )
        items = local_attributes.items()
        for kk, vv in items:
            if isInLibrary and attributesInModule[kk] is True:
                local_lazy_attributes.setdefault(kk, []).insert(0, vv)
            else:
                local_lazy_attributes.setdefault(kk, []).append(vv)
        if "snake_case_functions" in l_data:
            for function in l_data["snake_case_functions"]:
                local_lazy_attributes.setdefault(function, []).append(l_module)
        return l_module, l_data

    import itkBase
    import itkConfig
    import itkLazy
    import itkTypes
    import itkExtras
    import os
    import sys

    thisModule = sys.modules[__name__]
    if itkConfig.LazyLoading:
        # If we are loading lazily (on-demand), make a dict mapping the available
        # classes/functions/etc. (read from the configuration modules) to the
        # modules they are declared in. Then pass that dict to a LazyITKModule
        # instance and (later) do some surgery on sys.modules so that the 'itk'
        # module becomes that new instance instead of what is executed from this
        # file.
        lazyAttributes = {}
        for module, data in itkBase.module_data.items():
            _GetLazyAttributes(lazyAttributes, module, data)

        if isinstance(thisModule, itkLazy.LazyITKModule):
            # Handle reload case where we've already done this once.
            # If we made a new module every time, multiple reload()s would fail
            # because the identity of sys.modules['itk'] would always be changing.
            thisModule.__init__(__name__, lazyAttributes)
            del lazyAttributes
        else:
            thisModule = itkLazy.LazyITKModule(__name__, lazyAttributes)
    else:
        # We're not lazy-loading. Just load the modules in the order specified in
        # the known_modules list for consistency.
        for module in itkBase.known_modules:
            itkBase.LoadModule(module, thisModule.__dict__)

    # Regardless of how it was loaded, fill up the itk module with the ITK types
    # and extras.
    for k, v in itkTypes.__dict__.items():
        if k != "itkCType" and not k.startswith("_"):
            setattr(thisModule, k, v)
    for k, v in itkExtras.__dict__.items():
        if not k.startswith("_"):
            setattr(thisModule, k, v)

    # Populate itk.ITKModuleName
    for module, data in itkBase.module_data.items():
        attributes = {}
        module, data = _GetLazyAttributes(attributes, module, data)
        itkModule = itkLazy.LazyITKModule(module, attributes)
        setattr(thisModule, module, itkModule)

    # Set the __path__ attribute, which is required for this module to be used as a
    # package
    setattr(thisModule, "__path__", __path__)
    setattr(thisModule, "__spec__", __spec__)

    if itkConfig.LazyLoading:
        # this has to be the last step, else python gets confused about itkTypes
        # and itkExtras above. I'm not sure why...
        sys.modules[__name__] = thisModule
    else:
        # do some cleanup
        del module, thisModule, itkModule
        del itkBase, itkConfig, itkLazy, itkTypes, itkExtras, os, sys


# Now do the initialization
_initialize_module()
