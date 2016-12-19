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

#
#  Test basic properties of modified times
#

import itk

#image and transform are in the same module, but filters are in a different module.
imType = itk.Image[itk.F, 2]
imTypeB = itk.Image[itk.UC, 2]
im = imType.New()

transType = itk.Transform[itk.D, 3]
trans = transType.New()

filtType = itk.AndImageFilter[imTypeB, imTypeB, imTypeB]
filt = filtType.New()

metricType = itk.ImageToImageMetricv4[imType, imType]
met = metricType.New()
#We modify them in the order image, transform, filter
for _ in range(3000):
    im.Modified()
trans.Modified()
met.Modified()
filt.Modified()

#and their Modified times should respect that order.
assert im.GetMTime() < trans.GetMTime()
assert trans.GetMTime() < met.GetMTime()
assert met.GetMTime() < filt.GetMTime()
