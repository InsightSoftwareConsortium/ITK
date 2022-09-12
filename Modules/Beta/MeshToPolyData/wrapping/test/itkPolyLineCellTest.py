import itk
import numpy as np
import os

# Create a test mesh
mesh_input = itk.Mesh[itk.D, 3].New()

# Inserting 5 points in the Mesh
points_arr  = np.array([0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 1, 2, 1, 2, 3]).astype('float32')
points_vc = itk.vector_container_from_array(points_arr.flatten())
mesh_input.SetPoints(points_vc)

# Inserting 3 cells comprising 1 PolyLine Cell and 2 Line Cells
# LINES 3 10
# 2 0 1
# 3 0 2 5
# 2 3 4
before_cells_array = np.array([1, 2, 0, 1, 10, 3, 0, 2, 5, 1, 2, 3, 4]).astype('uint64')
mesh_input.SetCellsArray(itk.vector_container_from_array(before_cells_array))

# Convert Mesh to PolyData
filter = itk.MeshToPolyDataFilter[type(mesh_input)].New(Input=mesh_input)
filter.Update()
poly_data = filter.GetOutput()
assert(type(poly_data) == itk.PolyData[itk.D])
lines = poly_data.GetLines()

# Check the count of points in line cells of polydata
assert(lines.Size() == 10)

filter = itk.PolyDataToMeshFilter[type(poly_data)].New(Input=poly_data)
filter.Update()
mesh_output = filter.GetOutput()

assert(mesh_output.GetNumberOfPoints() == mesh_input.GetNumberOfPoints())
assert(mesh_output.GetNumberOfCells() == mesh_input.GetNumberOfCells())

# Check if points are same
for i in range(0, mesh_output.GetNumberOfPoints()):
    assert(mesh_output.GetPoint(i) == mesh_input.GetPoint(i))

# Check if cells are same
after_cells_array = itk.array_from_vector_container(mesh_output.GetCellsArray())
assert(np.array_equal(before_cells_array, after_cells_array))