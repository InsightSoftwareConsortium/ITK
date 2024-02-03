from pathlib import Path

test_input_path = Path(__file__).parent / ".." / ".." / ".." / "test" / "data" / "input"
test_output_path = Path(__file__).parent / ".." / ".." / ".." / "test" / "data" / "python"
test_output_path.mkdir(parents=True, exist_ok=True)

from itkwasm import FloatTypes, IntTypes, PixelTypes
from itkwasm_mesh_io import read_mesh

from itkwasm_mesh_to_poly_data import mesh_to_poly_data, poly_data_to_mesh

def test_cow_conversion():
    mesh = read_mesh(test_input_path / "cow.vtk")
    poly_data = mesh_to_poly_data(mesh)
    assert poly_data.numberOfPoints == 2903
    assert poly_data.polygonsBufferSize == 15593
    mesh_round_trip = poly_data_to_mesh(poly_data)
    assert mesh_round_trip.meshType.dimension == 3
    assert mesh_round_trip.meshType.pointComponentType == FloatTypes.Float32
    assert mesh_round_trip.meshType.cellComponentType == IntTypes.UInt32
    assert mesh_round_trip.meshType.pointPixelType == PixelTypes.Scalar
    assert mesh_round_trip.meshType.cellPixelType == PixelTypes.Scalar
    assert mesh_round_trip.numberOfPoints == 2903
    assert mesh_round_trip.numberOfCells == 3263

def test_cube_conversion():
    mesh = read_mesh(test_input_path / "cube.byu")
    poly_data = mesh_to_poly_data(mesh)
    assert poly_data.numberOfPoints == 8
    assert poly_data.polygonsBufferSize == 30
    mesh_round_trip = poly_data_to_mesh(poly_data)
    assert mesh_round_trip.meshType.dimension == 3
    assert mesh_round_trip.meshType.pointComponentType == FloatTypes.Float32
    assert mesh_round_trip.meshType.cellComponentType == IntTypes.UInt32
    assert mesh_round_trip.meshType.pointPixelType == PixelTypes.Scalar
    assert mesh_round_trip.meshType.cellPixelType == PixelTypes.Scalar
    assert mesh_round_trip.numberOfPoints == 8
    assert mesh_round_trip.numberOfCells == 6
