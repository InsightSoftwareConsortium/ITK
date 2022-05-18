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

#
#  Verify FFT factories are registered and synchronized with ITKCommon
#
FFT_BASE_CLASSES = [
    "ComplexToComplex1DFFTImageFilter",
    "ComplexToComplexFFTImageFilter",
    "Forward1DFFTImageFilter",
    "ForwardFFTImageFilter",
    "HalfHermitianToRealInverseFFTImageFilter",
    "Inverse1DFFTImageFilter",
    "InverseFFTImageFilter",
    "RealToHalfHermitianForwardFFTImageFilter",
]

import itk

itk.auto_progress(2)
try:
    itk.force_load()
except ImportError as e:
    pass

# Verify ITKCommon factory list is not empty after loading all modules
assert len(itk.ObjectFactoryBase.GetRegisteredFactories()) > 0

# Verify that FFT factories are in the list
overrides_found = list()
for factory in itk.ObjectFactoryBase.GetRegisteredFactories():
    for override_name in factory.GetClassOverrideNames():
        for base_name in FFT_BASE_CLASSES:
            if base_name not in overrides_found and base_name in override_name:
                overrides_found.append(base_name)

assert all([base_name in overrides_found for base_name in FFT_BASE_CLASSES])
