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


import types
import itkBase

not_loaded = 'not loaded'


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

    def __getattribute__(self, attr):
        value = types.ModuleType.__getattribute__(self, attr)
        if value is not_loaded:
            module = self.__belong_lazy_attributes[attr]
            namespace = {}
            itkBase.LoadModule(module, namespace)
            for k, v in namespace.items():
                setattr(self, k, v)
            value = namespace[attr]
        return value
