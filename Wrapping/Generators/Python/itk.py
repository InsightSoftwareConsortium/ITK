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

"""itk.py : Top-level container module for ITK wrappers."""
import itkBase
import itkConfig
import itkLazy
import itkTypes
import itkExtras
import os
import sys

# silently import psyco
try:
    import psyco
    psyco.profile()
except ImportError:
    pass

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
        templateNames = [t[0] for t in data['templates']]
        attributes = dict([(n, module) for n in templateNames])
        lazyAttributes.update(attributes)
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
    if k != 'itkCType' and not k.startswith('_'):
        setattr(thisModule, k, v)
for k, v in itkExtras.__dict__.items():
    if not k.startswith('_'):
        setattr(thisModule, k, v)

if itkConfig.LazyLoading:
    # this has to be the last step, else python gets confused about itkTypes
    # and itkExtras above. I'm not sure why...
    sys.modules[__name__] = thisModule
else:
    # do some cleanup
    del module, thisModule
    del itkBase, itkConfig, itkLazy, itkTypes, itkExtras, os, sys
