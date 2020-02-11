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
import itk

# Test PEP 366 compliance of LazyITKModule
assert(itk.__package__ == 'itk')
from itk import ITKCommon
assert(ITKCommon.__package__ == 'itk')

# Test pickling used bash Dask
try:
    import cloudpickle
    itkpickled = cloudpickle.dumps(itk)
    cloudpickle.loads(itkpickled)
except ImportError:
    pass
