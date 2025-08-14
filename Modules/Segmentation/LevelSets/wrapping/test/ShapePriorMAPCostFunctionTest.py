# ==========================================================================
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
# ==========================================================================*/

import itk


def print_and_compare_arrays(a1, a2, name):
    print(f"Baseline {name}: {str(a1)}")
    print(f"{name}: {str(a2)}")
    if len(a1) != len(a2):
        raise Exception("Arrays have different sizes: %d vs %d" % (len(a1), len(a2)))
    for i in range(len(a1)):
        if a1[i] != a2[i]:
            raise Exception(
                "Values at index %d do not match: %f vs %f" % (i, a1[i], a2[i])
            )


InputPixelType = itk.F
Dimension = 2
ImageType = itk.Image[InputPixelType, Dimension]
costfunction = itk.ShapePriorMAPCostFunction[ImageType, InputPixelType].New()
# For debugging purposes, print `costfunction` to know the variables
# it contains.
print(costfunction)

# Test setting and getting weights
weights = itk.FixedArray[itk.D, 4]()
weights[0] = 1.0  # weight for contour fit term
weights[1] = 20.0  # weight for image fit term
weights[2] = 1.0  # weight for shape prior term
weights[3] = 1.0  # weight for pose prior term
costfunction.SetWeights(weights)
get_weights = costfunction.GetWeights()
print_and_compare_arrays(weights, get_weights, "weights")

# Set `mean` using a Python list and verify that the value in
# `ShapePriorMAPCostFunction` is the value we passed.
mean = [0, 1, 2]
costfunction.SetShapeParameterMeans(mean)
get_mean = costfunction.GetShapeParameterMeans()
print_and_compare_arrays(mean, get_mean, "mean")

# Set `mean` using a Python list and verify that if it contains
# values that are not a number, it returns an exception.
mean = [0, "should not be accepted"]
try:
    costfunction.SetShapeParameterMeans(mean)
except ValueError as v:
    # required_failure_msg="Expecting a sequence of values."
    required_failure_msg = "Expecting a sequence of int or float"
    if str(v) == required_failure_msg:
        pass
    else:
        print(f"FAILURE: '{str(v)}' != '{required_failure_msg}'")
        raise v

# Set `stddev` using a Python list and verify that the value in
# `ShapePriorMAPCostFunction` is the value we passed.
stddev = [1, 2, 3]
costfunction.SetShapeParameterStandardDeviations(stddev)
get_stddev = costfunction.GetShapeParameterStandardDeviations()
print_and_compare_arrays(stddev, get_stddev, "stddev")

# Set `mean` using an ITK Array list and verify that the value in
# `ShapePriorMAPCostFunction` is the value we passed.
mean = itk.Array[itk.D](Dimension)
mean.Fill(0.0)
costfunction.SetShapeParameterMeans(mean)
get_mean = costfunction.GetShapeParameterMeans()
print_and_compare_arrays(mean, get_mean, "mean")

# Set `stddev` using an ITK Array list and verify that the value in
# `ShapePriorMAPCostFunction` is the value we passed.
stddev = itk.Array[itk.D](Dimension)
stddev.Fill(1.0)
costfunction.SetShapeParameterStandardDeviations(stddev)
get_stddev = costfunction.GetShapeParameterStandardDeviations()
print_and_compare_arrays(stddev, get_stddev, "stddev")

# Check that an exception is returned if we pass the value `None`.
try:
    costfunction.SetShapeParameterStandardDeviations(None)
except TypeError as t:
    required_failure_msg = "Expecting an itkArrayD, or a sequence of values."
    if str(t) == required_failure_msg:
        pass
    else:
        print(f"FAILURE: '{str(t)}' != '{required_failure_msg}'")
        raise t

# Check that an exception is returned if we pass an object of the wrong type.
try:
    f = itk.MedianImageFilter.New()
    costfunction.SetShapeParameterStandardDeviations(f)
except ValueError as t:
    required_failure_msg = "Expecting a sequence of int or float"
    if str(t) == required_failure_msg:
        pass
    else:
        print(f"FAILURE: '{str(v)}' != '{required_failure_msg}'")
        raise t
