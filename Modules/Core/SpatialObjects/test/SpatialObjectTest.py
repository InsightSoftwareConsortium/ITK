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

from __future__ import print_function

import itk

itk.auto_progress(2)

dim = 2
SOType = itk.SpatialObject[dim]
InternalImageType = itk.Image[itk.F, dim]
OutputPixelType = itk.UC
OutputImageType = itk.Image[OutputPixelType, dim]

ellipse = itk.EllipseSpatialObject[dim].New(Radius=[10, 5])
ellipse.GetObjectToParentTransform().SetOffset([20, 20])
ellipse.ComputeObjectToWorldTransform()

box = itk.BoxSpatialObject[dim].New(Size=20)
box.GetObjectToParentTransform().SetOffset([20, 40])
box.ComputeObjectToWorldTransform()

gaussian = itk.GaussianSpatialObject[dim].New(Radius=100)
gaussian.GetObjectToParentTransform().SetOffset([60, 60])
gaussian.GetObjectToParentTransform().SetScale(10)
gaussian.ComputeObjectToWorldTransform()

group = itk.GroupSpatialObject[dim].New()
group.AddSpatialObject(ellipse)
group.AddSpatialObject(box)
group.AddSpatialObject(gaussian)

filter_ = itk.SpatialObjectToImageFilter[SOType, InternalImageType].New(
    group, Size=[100, 100], UseObjectValue=True)
filter_.Update()
