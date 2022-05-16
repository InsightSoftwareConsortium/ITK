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

dim = 2
IType = itk.Image[itk.F, dim]

pca_function = itk.PCAShapeSignedDistanceFunction[itk.D, dim, IType].New()
im = IType.New()
im.SetRegions([10, 10])
im.Allocate()
l = [im, im]
# Test that it is possible to use a list of image
pca_function.SetPrincipalComponentImages(l)
# Test that it is possible to use an std::vector of image
vec = itk.vector[IType]()
vec.push_back(im)
vec.push_back(im)
pca_function.SetPrincipalComponentImages(vec)
