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
import itk

# Test PEP 366 compliance of itk and per-submodule namespaces
assert itk.__package__ == "itk"
from itk import ITKCommon

assert ITKCommon.__package__ == "itk"

# PEP 562 __dir__: lazy attributes are visible to tab-completion and
# tooling without forcing a SWIG load. Only dunder attributes (which
# the lazy __getattr__ explicitly skips) have been touched up to this
# point, so the underlying SWIG modules have not been loaded yet --
# "Image" must show up in dir() but stay absent from the module's
# __dict__ until first access.
assert "Image" in dir(itk)
assert "Image" not in vars(itk)
assert "Image" in dir(ITKCommon)
assert "Image" not in vars(ITKCommon)

# Factory hook: under the default configuration, accessing a class
# whose module declares a needed factory (ITKIOImageBase -> ImageIO)
# must trigger the corresponding factory registration as a side effect
# of the lazy load. nodefaultfactories.py covers the disabled path.
import itkConfig

if itkConfig.DefaultFactoryLoading:
    factories_before = len(itk.ObjectFactoryBase.GetRegisteredFactories())
    _ = itk.ImageFileReader
    factories_after = len(itk.ObjectFactoryBase.GetRegisteredFactories())
    assert factories_after > factories_before

# stdlib pickle round-trip on a per-submodule namespace: the lazy
# submodule is registered in sys.modules and its instance __reduce_ex__
# resolves through importlib.import_module, so plain pickle (not just
# cloudpickle) must round-trip to the same instance.
import pickle

assert pickle.loads(pickle.dumps(itk.ITKCommon)) is itk.ITKCommon
assert pickle.loads(pickle.dumps(itk)) is itk

# Test pickling used bash Dask
_has_cloudpickle: bool = False
try:
    import cloudpickle

    _has_cloudpickle = True
except ImportError:
    _has_cloudpickle = False
    pass

if _has_cloudpickle:
    print("Using cloudpickle to test dumping and loading itk.")
    itkpickled = cloudpickle.dumps(itk)
    cloudpickle.loads(itkpickled)
else:
    print("cloudpickle module not available for testing.")
    pass
