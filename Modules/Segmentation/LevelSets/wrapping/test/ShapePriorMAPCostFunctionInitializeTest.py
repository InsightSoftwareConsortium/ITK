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

"""End-to-end regression test for the issue #871 failure path.

Issue #871 reporter's observed error:

    RuntimeError: itkShapePriorMAPCostFunction.hxx:187:
    itk::ERROR: ShapePriorMAPCostFunction: ShapeParameterMeans does not
    have at least 3 number of elements.

That raise site is inside `ShapePriorMAPCostFunction::Initialize()`,
which the reporter reached via
`GeodesicActiveContourShapePriorLevelSetImageFilter.Update()`.  The
filter invokes `m_CostFunction->Initialize()` from its
`InitializeIteration` step (itkShapePriorSegmentationLevelSetImageFilter.hxx:91).

This test drives the exact raise-site code path from Python by:

 1. constructing a `PCAShapeSignedDistanceFunction` with a known
    `NumberOfPrincipalComponents` (the value returned by
    `GetNumberOfShapeParameters()`),
 2. wiring it into a `ShapePriorMAPCostFunction`,
 3. calling `SetShapeParameterMeans` and
    `SetShapeParameterStandardDeviations` with a native
    `itk.Array[itk.D]` -- the exact API surface that was silently
    dropping its argument before the fix in PR #1857,
 4. invoking `cost_function.Initialize()` -- identical to what the
    filter's InitializeIteration step runs.

Notes:

 * The full `GeodesicActiveContourShapePriorLevelSetImageFilter.Update()`
   pipeline cannot be driven from Python today because the required
   `SingleValuedNonLinearOptimizer` subclass (e.g. `AmoebaOptimizer`)
   is not in the Python wrapping.  `cost_function.Initialize()` is the
   identical C++ call the filter makes internally at the point where
   the #871 `RuntimeError` was raised, so driving it directly is the
   faithful equivalent.

 * The `PCAShapeSignedDistanceFunction` is used as the shape function
   because it is wrapped for Python.  Its `GetNumberOfShapeParameters`
   simply returns `m_NumberOfPrincipalComponents`, so setting that
   alone is enough to make `Initialize()` run the size check against
   a known target value.  No `MeanImage` / `PrincipalComponentImages`
   are required to reach the raise site.
"""

import itk

FAILURES = []


def check(condition, description):
    """Accumulate failures so every regression surfaces at once."""
    if condition:
        print(f"PASS  {description}")
    else:
        print(f"FAIL  {description}")
        FAILURES.append(description)


# ---------------------------------------------------------------------
# Positive case: native itk.Array[itk.D] of the right size.  Pre-fix,
# SetShapeParameterMeans silently stored an empty array, and the
# Initialize() check `means.Size() < shapeFunction->GetNumberOfShapeParameters()`
# raised the RuntimeError the reporter observed.  Post-fix, the array
# is stored correctly and Initialize() completes silently.
# ---------------------------------------------------------------------

Dimension = 2
InternalPixelType = itk.F
InternalImageType = itk.Image[InternalPixelType, Dimension]

ShapeFunctionType = itk.PCAShapeSignedDistanceFunction[
    itk.D, Dimension, InternalImageType
]
CostFunctionType = itk.ShapePriorMAPCostFunction[InternalImageType, InternalPixelType]

NumberOfPrincipalComponents = 3

shape_function = ShapeFunctionType.New()
shape_function.SetNumberOfPrincipalComponents(NumberOfPrincipalComponents)
check(
    shape_function.GetNumberOfShapeParameters() == NumberOfPrincipalComponents,
    "PCA shape function reports the expected number of shape parameters "
    f"({NumberOfPrincipalComponents})",
)

cost_function = CostFunctionType.New()
cost_function.SetShapeFunction(shape_function)

# Satisfy the preconditions in ShapePriorMAPCostFunctionBase::Initialize()
# -- ActiveRegion and FeatureImage must be non-null.  Contents are
# irrelevant to the issue #871 code path; the parent filter normally
# fills them in from the fast-marching output and the sigmoid
# edge-potential map.
NodeType = itk.LevelSetNode[InternalPixelType, Dimension]
NodeContainerType = itk.VectorContainer[itk.UI, NodeType]
active_region = NodeContainerType.New()
cost_function.SetActiveRegion(active_region)

feature_image = InternalImageType.New()
tiny_size = itk.Size[Dimension]()
tiny_size.Fill(2)
feature_image.SetRegions(tiny_size)
feature_image.Allocate()
feature_image.FillBuffer(0.0)
cost_function.SetFeatureImage(feature_image)

mean = itk.Array[itk.D](NumberOfPrincipalComponents)
mean.Fill(0.5)
stddev = itk.Array[itk.D](NumberOfPrincipalComponents)
stddev.Fill(1.25)
cost_function.SetShapeParameterMeans(mean)
cost_function.SetShapeParameterStandardDeviations(stddev)

# Round-trip check -- without the BUG fix these would be empty.
check(
    len(cost_function.GetShapeParameterMeans()) == NumberOfPrincipalComponents,
    "GetShapeParameterMeans() has the expected number of elements after "
    "Set from a native itk.Array",
)
check(
    len(cost_function.GetShapeParameterStandardDeviations())
    == NumberOfPrincipalComponents,
    "GetShapeParameterStandardDeviations() has the expected number of "
    "elements after Set from a native itk.Array",
)

# This is the exact C++ call that the filter performs at
# itkShapePriorSegmentationLevelSetImageFilter.hxx:91.  Pre-fix this
# raised the RuntimeError the reporter observed.
try:
    cost_function.Initialize()
except RuntimeError as e:
    if "does not have at least" in str(e):
        check(False, f"Initialize() regressed to the issue #871 error path: {e}")
    else:
        # Any *other* RuntimeError is outside the scope of this test.
        check(False, f"Initialize() raised an unexpected RuntimeError: {e}")
else:
    check(
        True,
        "cost_function.Initialize() completes without raising the "
        "'ShapeParameterMeans does not have at least N number of elements' "
        "RuntimeError (the issue #871 symptom)",
    )


# ---------------------------------------------------------------------
# Negative control: if the means/stddev arrays are *shorter* than the
# shape function's parameter count, Initialize() MUST still raise.
# This guards the BUG fix against going too far and silently masking
# genuinely undersized inputs.
# ---------------------------------------------------------------------

undersized_mean = itk.Array[itk.D](1)  # shape expects 3, only 1 provided
undersized_mean.Fill(0.0)
undersized_stddev = itk.Array[itk.D](NumberOfPrincipalComponents)
undersized_stddev.Fill(1.0)

negative_cost = CostFunctionType.New()
negative_cost.SetShapeFunction(shape_function)
negative_cost.SetActiveRegion(active_region)
negative_cost.SetFeatureImage(feature_image)
negative_cost.SetShapeParameterMeans(undersized_mean)
negative_cost.SetShapeParameterStandardDeviations(undersized_stddev)

try:
    negative_cost.Initialize()
except RuntimeError as e:
    check(
        "does not have at least" in str(e),
        "Initialize() still raises on genuinely undersized means array "
        f"(expected guard behavior, not regressed): {e}",
    )
else:
    check(
        False,
        "Initialize() silently accepted an undersized means array -- the "
        "sanity-check guard inside cost_function.Initialize() has regressed",
    )


# ---------------------------------------------------------------------

if FAILURES:
    print(f"\n{len(FAILURES)} assertion(s) failed:")
    for f in FAILURES:
        print(f"  - {f}")
    raise SystemExit(1)

print(
    "\nIssue #871 filter-initialization path is clean: the native itk.Array "
    "Set/Get round-trips through cost_function.Initialize() without "
    "triggering the 'does not have at least N number of elements' error."
)
