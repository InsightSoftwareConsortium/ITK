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
import numpy as np

Dimension = 3
PixelType = itk.Array.D
NumberOfPoints = 10
PixelDataSize = 5

MeshType = itk.Mesh[PixelType, Dimension]
mesh = MeshType.New()

# Create Vector Container and Store values in it for each Point
# For windows use itk.ULL
if hasattr(itk.VectorContainer, "ULAD"):
    IdentifierType = itk.UL
else:
    IdentifierType = itk.ULL

v = itk.VectorContainer[IdentifierType, PixelType].New()
v.Reserve(NumberOfPoints)

for i in range(NumberOfPoints):
    pixel_data_reference = v.CreateElementAt(i)
    pixel_data_reference.SetSize(PixelDataSize)
    pixel_data_reference.Fill(0)
    pixel_data_reference[0] = i
    pixel_data_reference[4] = i + 4

# Set the point data container
mesh.SetPointData(v)

assert mesh.GetPointData().Size() == NumberOfPoints
assert mesh.GetPointData().ElementAt(0)[0] == 0
assert mesh.GetPointData().ElementAt(0)[4] == 4
assert mesh.GetPointData().ElementAt(2)[0] == 2 + 0
assert mesh.GetPointData().ElementAt(2)[4] == 2 + 4

# resize the PixelDataSize to see if it can be altered successfully
PixelDataSize = 10
for i in range(NumberOfPoints):
    pixel_data_reference = v.CreateElementAt(i)
    pixel_data_reference.SetSize(PixelDataSize)
    pixel_data_reference.Fill(0)
    pixel_data_reference[0] = i
    pixel_data_reference[9] = i + 10

assert mesh.GetPointData().Size() == NumberOfPoints
assert mesh.GetPointData().ElementAt(0)[0] == 0
assert mesh.GetPointData().ElementAt(0)[9] == 10
assert mesh.GetPointData().ElementAt(2)[0] == 2 + 0
assert mesh.GetPointData().ElementAt(2)[9] == 2 + 10
