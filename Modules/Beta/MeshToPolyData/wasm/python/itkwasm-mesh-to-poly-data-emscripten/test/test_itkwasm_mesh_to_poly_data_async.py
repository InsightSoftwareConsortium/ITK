import pytest
import sys

if sys.version_info < (3,10):
    pytest.skip("Skipping pyodide tests on older Python", allow_module_level=True)

from pytest_pyodide import run_in_pyodide

from itkwasm_mesh_to_poly_data_emscripten import __version__ as test_package_version

@pytest.fixture
def package_wheel():
    return f"itkwasm_mesh_to_poly_data_emscripten-{test_package_version}-py3-none-any.whl"

@pytest.fixture
def input_data():
    from pathlib import Path
    input_base_path = Path('..', '..', 'test', 'data')
    test_files = [
        Path('input') / 'cow.vtk',
        Path('input') / 'cube.byu',
    ]
    data = {}
    for f in test_files:
        with open(input_base_path / f, 'rb') as fp:
            data[str(f.name)] = fp.read()
    return data

@run_in_pyodide(packages=['micropip'])
async def test_round_trip(selenium, input_data, package_wheel):
    import micropip
    await micropip.install([package_wheel, 'itkwasm-mesh-io'])

    def write_input_data_to_fs(input_data, filename):
        with open(filename, 'wb') as fp:
            fp.write(input_data[filename])

    from pathlib import Path
    from itkwasm import FloatTypes, IntTypes, PixelTypes
    from itkwasm_mesh_io import read_mesh_async

    from itkwasm_mesh_to_poly_data_emscripten import mesh_to_poly_data_async, poly_data_to_mesh_async

    test_file_path = 'cow.vtk'
    write_input_data_to_fs(input_data, test_file_path)

    mesh = await read_mesh_async(test_file_path)
    poly_data = await mesh_to_poly_data_async(mesh)
    assert poly_data.numberOfPoints == 2903
    assert poly_data.polygonsBufferSize == 15593
    mesh_round_trip = await poly_data_to_mesh_async(poly_data)
    assert mesh_round_trip.meshType.dimension == 3
    assert mesh_round_trip.meshType.pointComponentType == FloatTypes.Float32
    assert mesh_round_trip.meshType.cellComponentType == IntTypes.UInt32
    assert mesh_round_trip.meshType.pointPixelType == PixelTypes.Scalar
    assert mesh_round_trip.meshType.cellPixelType == PixelTypes.Scalar
    assert mesh_round_trip.numberOfPoints == 2903
    assert mesh_round_trip.numberOfCells == 3263


    test_file_path = 'cube.byu'
    write_input_data_to_fs(input_data, test_file_path)

    mesh = await read_mesh_async(test_file_path)
    poly_data = await mesh_to_poly_data_async(mesh)
    assert poly_data.numberOfPoints == 8
    assert poly_data.polygonsBufferSize == 30
    mesh_round_trip = await poly_data_to_mesh_async(poly_data)
    assert mesh_round_trip.meshType.dimension == 3
    assert mesh_round_trip.meshType.pointComponentType == FloatTypes.Float32
    assert mesh_round_trip.meshType.cellComponentType == IntTypes.UInt32
    assert mesh_round_trip.meshType.pointPixelType == PixelTypes.Scalar
    assert mesh_round_trip.meshType.cellPixelType == PixelTypes.Scalar
    assert mesh_round_trip.numberOfPoints == 8
    assert mesh_round_trip.numberOfCells == 6