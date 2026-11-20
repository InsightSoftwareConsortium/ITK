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

keys = itk.BSplineTransform.keys()
for dimension in {d for _, d, _ in keys}:
    orders = {o for _, d, o in keys if d == dimension}
    assert orders == {1, 2, 3}, (dimension, orders)
assert itk.template(itk.BSplineTransform.New())[1][2] == 3

# Evaluate and round trip every wrapped (dimension, spline order) combination
for ptype, dimension, order in keys:
    transform = itk.BSplineTransform[ptype, dimension, order].New()
    transform.SetTransformDomainMeshSize([4] * dimension)
    transform.SetTransformDomainPhysicalDimensions([10.0] * dimension)
    params = transform.GetParameters()
    params.Fill(0.5)
    transform.SetParameters(params)

    copies = [itk.transform_from_dict(itk.dict_from_transform(transform))]
    # The transform factory registers dimensions 2-4 only
    if dimension in (2, 3, 4):
        filename = os.path.join(output_dir, f"BSplineTransform_{dimension}_{order}.tfm")
        itk.transformwrite([transform], filename)
        copies.append(itk.transformread(filename)[0])

    for t in [transform] + copies:
        assert itk.template(t)[1][2] == order, (dimension, order, t)
        point = t.TransformPoint([5.0] * dimension)
        for i in range(dimension):
            assert abs(point[i] - 5.5) < 1e-6, (dimension, order, point)
