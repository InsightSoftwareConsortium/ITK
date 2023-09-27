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
lsm = lom_filter.GetLabelSetMeasures()
for label, measure in lsm.items():
    print(f"Label: {label}, i: {measure.m_Intersection}, u: {measure.m_Union}")
