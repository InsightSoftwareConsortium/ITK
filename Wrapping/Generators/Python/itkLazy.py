#==========================================================================
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
#==========================================================================*/
import types
import itkBase

not_loaded = 'not loaded'

def _lazy_itk_module_reconstructor(module_name, state):
    # Similar to copyreg._reconstructor
    lazy_module = types.ModuleType.__new__(LazyITKModule, state)
    types.ModuleType.__init__(lazy_module, module_name)

    return lazy_module

class LazyITKModule(types.ModuleType):

    """Subclass of ModuleType that implements a custom __getattribute__ method
    to allow lazy-loading of attributes from ITK sub-modules."""

    def __init__(self, name, lazy_attributes):
        types.ModuleType.__init__(self, name)
        for k, v in lazy_attributes.items():
            itkBase.lazy_attributes.setdefault(k,set()).update(v)
        self.__belong_lazy_attributes = dict( (k,v[0]) for k,v in lazy_attributes.items() if len(v) > 0 )
        for k in lazy_attributes:
            setattr(self, k, not_loaded)
        # For PEP 366
        setattr(self, '__package__', 'itk')
        setattr(self, 'lazy_attributes', lazy_attributes)
        setattr(self, 'loaded_lazy_modules', set())

    def __getattribute__(self, attr):
        value = types.ModuleType.__getattribute__(self, attr)
        if value is not_loaded:
            module = self.__belong_lazy_attributes[attr]
            namespace = {}
            itkBase.LoadModule(module, namespace)
            self.loaded_lazy_modules.add(module)
            for k, v in namespace.items():
                setattr(self, k, v)
            value = namespace[attr]
        return value

    # For pickle support
    def __reduce_ex__(self, proto):
        state = self.__getstate__()
        return _lazy_itk_module_reconstructor, (self.__name__, state), state

    # For pickle support
    def __getstate__(self):
        state = self.__dict__.copy()
        lazy_modules = list()
        # import ipdb; ipdb.set_trace()
        for key in self.lazy_attributes:
            if isinstance(state[key], LazyITKModule):
                lazy_modules.append((key, state[key].lazy_attributes))
            state[key] = not_loaded
        state['lazy_modules'] = lazy_modules

        return state

    # For pickle support
    def __setstate__(self, state):
        self.__dict__.update(state)
        for module_name, lazy_attributes in state['lazy_modules']:
            self.__dict__.add(module_name, LazyITKModule(module_name,
                lazy_attributes))

        for module in state['loaded_lazy_modules']:
            namespace = {}
            itkBase.LoadModule(module, namespace)
            for k, v in namespace.items():
                setattr(self, k, v)
