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

import os
import sys

import itk

output_dir = sys.argv[1]

# Evaluate and round trip every wrapped (dimension, spline order) combination
for ptype, dimension, order in itk.BSplineTransform.keys():
    transform = itk.BSplineTransform[ptype, dimension, order].New()
    transform.SetTransformDomainMeshSize([4] * dimension)
    transform.SetTransformDomainPhysicalDimensions([10.0] * dimension)
    params = transform.GetParameters()
    params.Fill(0.5)
    transform.SetParameters(params)

    filename = os.path.join(output_dir, f"BSplineTransform_{dimension}_{order}.tfm")
    itk.transformwrite([transform], filename)
    read_transform = itk.transformread(filename)[0]

    for t in (transform, read_transform):
        point = t.TransformPoint([5.0] * dimension)
        for i in range(dimension):
            assert abs(point[i] - 5.5) < 1e-6, (dimension, order, point)
