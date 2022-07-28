# ==========================================================================
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
# ==========================================================================

# For testing itk PointsLocator in Python
import itk

# Create points on a plane
PointSetType = itk.PointSet[itk.F, 3]
pointset = PointSetType.New()
pointset.SetPoint(0, [0, 0, 0])
pointset.SetPoint(1, [0, 1, 0])
pointset.SetPoint(2, [1, 0, 0])
pointset.SetPoint(3, [1, 1, 0])

# Create PointsLocator
PointType = itk.Point[itk.F, 3]
PointsContainerType = itk.VectorContainer[itk.IT, PointType]
pl = itk.PointsLocator[PointsContainerType].New()
pl.SetPoints(pointset.GetPoints())
pl.Initialize()

# Query tree for one point
vl = itk.VectorContainer[itk.IT, itk.IT].New()
pl.FindClosestNPoints(pointset.GetPoint(0), 3, vl)
assert vl.Size() == 3
assert vl.GetElement(0) == 0
assert vl.GetElement(1) == 1
assert vl.GetElement(2) == 2

# Check points within radius
pl.FindPointsWithinRadius(pointset.GetPoint(0), 2, vl)
assert vl.Size() == 4

pl.FindPointsWithinRadius(pointset.GetPoint(0), 1.1, vl)
assert vl.Size() == 3
assert vl.GetElement(0) == 0
assert vl.GetElement(1) == 1
assert vl.GetElement(2) == 2
