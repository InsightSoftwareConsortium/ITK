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

# a short program to check that ITK's API is consistent across the library.

import itk
import sys
from itk.support.template_class import itkTemplate

itk.auto_progress(2)

# must force the load to return all the names with dir(itk)
itk.force_load()

wrongAPI = 0
totalAPI = 0

# a list of classes to exclude. Typically, the classes with a custom New()
# method, which return a subclass of the current class.
exclude = [
    # The following class API could be updated.
    "GDCMSeriesFileNames",
    "HistogramToRunLengthFeaturesFilter",
    "HistogramToTextureFeaturesFilter",
    "ScalarImageToRunLengthFeaturesFilter",
    "ScalarImageToTextureFeaturesFilter",
    # These classes are just ignored.
    "ScanlineFilterCommon",  # Segfault
    "ImageToVTKImageFilter",
    "templated_class",
    "auto_pipeline",
    "pipeline",
    "cvar",
]


def checkGetOutputConsistency(o, t):
    totalAPI = 0
    wrongAPI = 0
    try:
        i = o.New()
    except AttributeError:
        # We only care about object that can be constructed with New() since
        # we only worry about ITK ProcessObjects.
        return totalAPI, wrongAPI
    # Skip abstract classes
    if i.GetNameOfClass() != t:
        return totalAPI, wrongAPI
    # Only check itkProcessObjects
    if issubclass(o, itk.ProcessObject):
        totalAPI = 1
        numberOfIndexedOutputs = i.GetNumberOfIndexedOutputs()
        has_GetOutput = hasattr(i, "GetOutput")
        if ((numberOfIndexedOutputs == 0) == has_GetOutput) or (
            numberOfIndexedOutputs >= 1 and not has_GetOutput
        ):
            msg = (
                "%s: Wrong API: `GetOutput()` %s found, NumberOfIndexedOutputs: %d"
                % (
                    i.GetNameOfClass(),
                    "not" * (not has_GetOutput),
                    numberOfIndexedOutputs,
                )
            )
            print(msg, file=sys.stderr)
            wrongAPI = 1
        elif numberOfIndexedOutputs >= 1:
            i.GetOutput()
    return totalAPI, wrongAPI


# Test that `itkProcessObject` derived class have an API that is consistent
# with what is expected in the Python wrapping.
# Check that all the itkProcessObjects (to which a `__call()__` function is
# added in Python) can actually run that call function. More specifically,
# check that they...
for t in dir(itk):
    if t not in exclude:
        T = itk.__dict__[t]
        # first case - that's a templated class
        if isinstance(T, itkTemplate) and len(T) > 0:
            # Most templated object would simply instantiate the first type
            # listed if `New()` is called directly on the template type, but
            # this is not the case for all templates
            # (e.g. itk.ImageFileReader).
            # So we manually select the first type available.
            # Note: converting to `list` to be compatible with Python3
            o = T[list(T.keys())[0]]
            tot, w = checkGetOutputConsistency(o, t)
            totalAPI += tot
            wrongAPI += w

        else:
            tot, w = checkGetOutputConsistency(T, t)
            totalAPI += tot
            wrongAPI += w

print(f"{totalAPI} classes checked.")
if wrongAPI:
    print(f"{wrongAPI} classes are not providing the API.", file=sys.stderr)
    sys.exit(1)
