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
import pickle
import sys
import os

Dimension = 3
PixelType = itk.D
NumberOfPoints = 5
NumberOfCells = 6


MeshType = itk.Mesh[PixelType, Dimension]
mesh = MeshType.New()


# Set Points in the Mesh
PointType = itk.Point[itk.F, 3]
if os.name == 'nt':
    v_point = itk.VectorContainer[itk.ULL, PointType].New()
else:
    v_point = itk.VectorContainer[itk.UL, PointType].New()
v_point.Reserve(NumberOfPoints)

point = PointType()
for i in range(NumberOfPoints):
    point[0] = i + 1.0
    point[1] = i + 2.0
    point[2] = i + 3.0
    v_point.SetElement(i, point)

arr = itk.array_view_from_vector_container(v_point)
points_vc = itk.vector_container_from_array(arr.flatten())

mesh.SetPoints(points_vc)


# Set Cells in the Mesh as Triangle Cell
cells_array = np.zeros([NumberOfCells, Dimension], np.uint64)

# Insert point ids in that cell
for i in range(NumberOfCells):
    cells_array[i, :] = [i * 1, (i + 1) * 2, (i + 2) * 3]

# Insert the cells in mesh by converting to 1D array
cells_vector = itk.vector_container_from_array(cells_array.flatten())
# Test the API for same type of cells
mesh.SetCellsArray(cells_vector, itk.CommonEnums.CellGeometry_TRIANGLE_CELL)

# Set the name of mesh
mesh.SetObjectName("testmesh")

# Check the serialization of mesh
serialize_deserialize = pickle.loads(pickle.dumps(mesh))

assert serialize_deserialize.GetNumberOfPoints() == mesh.GetNumberOfPoints()
assert serialize_deserialize.GetNumberOfCells() == mesh.GetNumberOfCells()
assert serialize_deserialize.GetObjectName() == mesh.GetObjectName()

# Check if points are same
for i in range(NumberOfPoints):
    p1 = serialize_deserialize.GetPoint(i)
    p2 = mesh.GetPoint(i)
    assert p1 == p2

# Check if cells are same
cells_original = itk.array_from_vector_container(mesh.GetCellsArray())
cells_deserialized = itk.array_from_vector_container(
    serialize_deserialize.GetCellsArray()
)

cells_original = np.reshape(cells_original, [NumberOfCells, Dimension + 2])
cells_deserialized = np.reshape(cells_deserialized, [NumberOfCells, Dimension + 2])

for i in range(mesh.GetNumberOfCells()):
    # check the cell type
    assert cells_original[i][0] == cells_deserialized[i][0]
    # check the count of points in the cell
    assert cells_original[i][1] == cells_deserialized[i][1]
    # check the point ids in the cell
    assert cells_original[i][2] == cells_deserialized[i][2]
    assert cells_original[i][3] == cells_deserialized[i][3]
    assert cells_original[i][4] == cells_deserialized[i][4]


# Check dictionary set/get for ITK mesh
mesh["name"] = "testmesh1"
assert mesh["name"] == "testmesh1"

points_array = np.array([1, 2, 3, 4, 5, 6, 7, 8, 9], dtype="float32")
mesh["points"] = points_array
assert np.array_equal(mesh["points"], points_array)

points_data_array = np.array([10, 11], dtype="float64")
mesh["pointData"] = points_data_array
assert np.array_equal(mesh["pointData"], points_data_array)

cells_array = np.array(
    [itk.CommonEnums.CellGeometry_TRIANGLE_CELL, 3, 1, 2, 3], dtype=np.uint64
)
mesh["cells"] = cells_array
assert np.array_equal(mesh["cells"], cells_array)

cell_data_array = np.array([15], dtype="float64")
mesh["cellData"] = cell_data_array
assert np.array_equal(mesh["cellData"], cell_data_array)

try:
    import vtk
    from vtk.util import numpy_support

    # Test conversion of VTK to ITK mesh
    if len(sys.argv) > 1:
        # Read input VTK mesh
        filename = sys.argv[1]
        reader = vtk.vtkPolyDataReader()
        reader.SetFileName(filename)
        reader.Update()
        vtk_mesh = reader.GetOutput()

        # Set data for each point in the VTK mesh
        point_data = np.arange(vtk_mesh.GetNumberOfPoints()).astype("float")
        vtk_point_data = vtk_mesh.GetPointData()
        vtk_point_data.SetScalars(numpy_support.numpy_to_vtk(point_data))

        # Set data for each cell/poly in the VTK mesh
        cell_data = np.arange(vtk_mesh.GetNumberOfPolys()).astype("float")
        vtk_cell_data = vtk_mesh.GetCellData()
        vtk_cell_data.SetScalars(numpy_support.numpy_to_vtk(cell_data))

        vtk_cells_count = vtk_mesh.GetNumberOfPolys()

        # Get points array from VTK mesh
        points = vtk_mesh.GetPoints().GetData()
        points_numpy = np.array(points).flatten()

        # Get cells array from VTK mesh
        polys = vtk_mesh.GetPolys().GetData()
        polys_numpy = np.array(polys).flatten()

        # Testing for Triangle Mesh
        polys_numpy = np.reshape(polys_numpy, [vtk_cells_count, Dimension+1])

        # Extracting only the points by removing first column that denotes the VTK cell type
        polys_numpy = polys_numpy[:, 1:]
        polys_numpy = polys_numpy.flatten().astype(np.uint64)

        # Get point data from VTK mesh to insert in ITK Mesh
        point_data_numpy = np.array(vtk_mesh.GetPointData().GetScalars())

        # Get cell data from VTK mesh to insert in ITK Mesh
        cell_data_numpy = np.array(vtk_mesh.GetCellData().GetScalars())

        # Create a new ITK Mesh using data from VTK Mesh
        itk_mesh = MeshType.New()
        itk_mesh.SetPoints(itk.vector_container_from_array(points_numpy))
        itk_mesh.SetCellsArray(itk.vector_container_from_array(polys_numpy), itk.CommonEnums.CellGeometry_TRIANGLE_CELL)
        itk_mesh.SetPointData(itk.vector_container_from_array(point_data_numpy))
        itk_mesh.SetCellData(itk.vector_container_from_array(cell_data_numpy))

        assert itk_mesh.GetNumberOfPoints() == vtk_mesh.GetNumberOfPoints()
        assert itk_mesh.GetNumberOfCells() == vtk_mesh.GetNumberOfPolys()

        # Check if values are same in ITK and VTK Mesh
        assert np.array_equal(
            points_numpy, itk.array_from_vector_container(itk_mesh.GetPoints()).flatten()
        )
        assert np.array_equal(
            point_data_numpy, itk.array_from_vector_container(itk_mesh.GetPointData())
        )
        assert np.array_equal(
            cell_data_numpy, itk.array_from_vector_container(itk_mesh.GetCellData())
        )

        # Get only the point ids in each cell
        itk_cells = itk.array_from_vector_container(itk_mesh.GetCellsArray())
        itk_cells = np.reshape(itk_cells, [itk_mesh.GetNumberOfCells(), Dimension+2])
        itk_cells = itk_cells[:, 2:].flatten()
        assert np.array_equal(
            polys_numpy, itk_cells
        )
except ImportError:
    print('VTK import failed. Skipping the test.')
