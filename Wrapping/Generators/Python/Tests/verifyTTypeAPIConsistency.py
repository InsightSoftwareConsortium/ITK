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

# a short program to check that ITK's API is consistent across the library.

import itk
import sys
import itkTemplate
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
"templated_class",
"auto_pipeline",
"pipeline"
]

def checkTTypeConsistency(o, t):
    totalAPI = 0
    wrongAPI = 0
    try:
        i = o.New()
    except AttributeError:
        # We only care about object that can be constructed with `New()`.
        return totalAPI, wrongAPI
    totalAPI = 1
    list_methods = dir(i)
    list_setttype = [k for k in list_methods if k.lower() == 'setttype']
    if list_setttype:
        msg = "%s: Wrong API: `setttype()` found (%s)" % (i.GetNameOfClass(), list_setttype)
        print(msg, file=sys.stderr)
        wrongAPI = 1
    return totalAPI, wrongAPI

# Test that no ITK templated object uses the keyword `ttype` which is used in
# Python to select the template types. Specifically, if a class was implementing
# a function that was called `SetTType`, this would collide with the `ttype`
# keyword that is being added to the `New()` function in `itkTemplate.py`.
for t in dir(itk):
    if t not in exclude:
        T = itk.__dict__[t]
        # Only check template classes.
        if isinstance(T,itkTemplate.itkTemplate) and len(T) > 0:
            # Most templated object would simply instantiate the first type
            # listed if `New()` is called directly on the template type, but
            # this is not the case for all templates
            # (e.g. itk.ImageFileReader).
            # So we manually select the first type available.
            # Note: converting to `list` to be compatible with Python3
            o = T[list(T.keys())[0]]
            tot, w = checkTTypeConsistency(o, t)
            totalAPI += tot
            wrongAPI += w


print("%s classes checked." % totalAPI)
if wrongAPI:
    print(
        "%s classes are using the method `setttype`." % wrongAPI,
        file=sys.stderr)
    sys.exit(1)
