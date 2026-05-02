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


import itk
from sys import argv

itk.auto_progress(2)

ref = itk.imread(argv[1], itk.UC)
seg = itk.imread(argv[2], itk.UC)

lom_filter = itk.LabelOverlapMeasuresImageFilter[itk.Image[itk.UC, 3]].New()
lom_filter.SetTargetImage(seg)
lom_filter.SetSourceImage(ref)
lom_filter.UpdateLargestPossibleRegion()

# GetLabelSetMeasures() returns std::unordered_map<LabelType, ...>, which SWIG
# cannot wrap as a Python dict across submodule boundaries.  Use the paired
# accessors GetLabels() + GetMeasureForLabel() for Python iteration.
labels = list(lom_filter.GetLabels())
print(f"Found {len(labels)} labels")
for label in sorted(labels):
    measure = lom_filter.GetMeasureForLabel(label)
    # Use the explicit Get* accessors (igenerator does not expose public data
    # members of a struct to Python; the m_Foo fields are not visible).
    print(f"Label: {label}, i: {measure.GetIntersection()}, u: {measure.GetUnion()}")
