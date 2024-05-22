#include "itkMesh.h"
#include "itkMeshFileReader.h"
#include "itkMeshToPolyDataFilter.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkPolyDataWriter.h"
#include "vtkNew.h"

using MeshType = itk::Mesh<float, 3>;

// Function to convert itk::Mesh to vtkPolyData
vtkSmartPointer<vtkPolyData> ConvertITKMeshToVTKPolyData(MeshType::Pointer itkMesh)
{
    typedef itk::MeshToPolyDataFilter<MeshType> MeshToPolyDataType;
    MeshToPolyDataType::Pointer meshToPolyData = MeshToPolyDataType::New();
    meshToPolyData->SetInput(itkMesh);
    meshToPolyData->Update();

    const auto polyDataItk = meshToPolyData->GetOutput();

    vtkSmartPointer<vtkPolyData> polyDataVtk = vtkSmartPointer<vtkPolyData>::New();

    // Transfer the points from polyDataItk to polyDataVtk
    vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
    const auto itkPoints = polyDataItk->GetPoints();
    for (itk::IdentifierType i = 0; i < itkPoints->size(); ++i)
    {
        const auto point = itkPoints->GetElement(i);
        points->InsertNextPoint(point[0], point[1], point[2]);
    }
    polyDataVtk->SetPoints(points);

    // Transfer the vertices from polyDataItk to polyDataVtk
    const auto verticesItk = polyDataItk->GetVertices();
    vtkNew<vtkCellArray> verticesVtk;
    for (itk::IdentifierType i = 0; i < verticesItk->size() / 2; ++i)
    {
        const vtkIdType vertex = static_cast<vtkIdType>(verticesItk->GetElement(i*2+1));
        verticesVtk->InsertNextCell(1, &vertex);
    }
    polyDataVtk->SetVerts(verticesVtk);

    // Transfer the lines from polyDataItk to polyDataVtk
    const auto linesItk = polyDataItk->GetLines();
    vtkNew<vtkCellArray> linesVtk;
    auto linesIt = linesItk->begin();
    while(linesIt != linesItk->end())
    {
        const auto numberOfPoints = *linesIt;
        ++linesIt;
        vtkNew<vtkIdList> line;
        for (itk::IdentifierType i = 0; i < numberOfPoints; ++i)
        {
            const vtkIdType pointId = static_cast<vtkIdType>(*linesIt);
            line->InsertNextId(pointId);
            ++linesIt;
        }
        linesVtk->InsertNextCell(line);
    }
    polyDataVtk->SetLines(linesVtk);

    // Transfer the polygons from polyDataItk to polyDataVtk
    const auto polygonsItk = polyDataItk->GetPolygons();
    vtkNew<vtkCellArray> polygonsVtk;
    auto polygonsIt = polygonsItk->begin();
    while (polygonsIt != polygonsItk->end())
    {
        const auto numberOfPoints = *polygonsIt;
        ++polygonsIt;
        vtkNew<vtkIdList> polygon;
        for (itk::IdentifierType i = 0; i < numberOfPoints; ++i)
        {
            const vtkIdType pointId = static_cast<vtkIdType>(*polygonsIt);
            polygon->InsertNextId(pointId);
            ++polygonsIt;
        }
        polygonsVtk->InsertNextCell(polygon);
    }
    polyDataVtk->SetPolys(polygonsVtk);

    // Transfer the triangle strips from polyDataItk to polyDataVtk
    const auto triangleStripsItk = polyDataItk->GetTriangleStrips();
    vtkNew<vtkCellArray> triangleStripsVtk;
    auto triangleStripsIt = triangleStripsItk->begin();
    while (triangleStripsIt != triangleStripsItk->end())
    {
        const auto numberOfPoints = *triangleStripsIt;
        ++triangleStripsIt;
        vtkNew<vtkIdList> triangleStrip;
        for (itk::IdentifierType i = 0; i < numberOfPoints; ++i)
        {
            const vtkIdType pointId = static_cast<vtkIdType>(*triangleStripsIt);
            triangleStrip->InsertNextId(pointId);
            ++triangleStripsIt;
        }
        triangleStripsVtk->InsertNextCell(triangleStrip);
    }
    polyDataVtk->SetStrips(triangleStripsVtk);

    // PointData and CellData could also be transferred, if needed.

    return polyDataVtk;
}

int main(int argc, char *argv[])
{
    if (argc < 3)
    {
        std::cerr << "Usage: " << argv[0] << " <input ITK mesh file> <output VTK file>" << std::endl;
        return EXIT_FAILURE;
    }

    const char *inputFilename = argv[1];
    const char *outputFilename = argv[2];

    // Read the input ITK mesh
    typedef itk::MeshFileReader<MeshType> ReaderType;
    ReaderType::Pointer reader = ReaderType::New();
    reader->SetFileName(inputFilename);

    try
    {
        reader->Update();
    }
    catch (itk::ExceptionObject &error)
    {
        std::cerr << "Error reading ITK mesh: " << error << std::endl;
        return EXIT_FAILURE;
    }

    MeshType::Pointer itkMesh = reader->GetOutput();

    // Convert ITK mesh to VTK PolyData
    vtkSmartPointer<vtkPolyData> vtkPolyData = ConvertITKMeshToVTKPolyData(itkMesh);

    // Write the VTK PolyData to file
    vtkSmartPointer<vtkPolyDataWriter> writer = vtkSmartPointer<vtkPolyDataWriter>::New();
    writer->SetFileName(outputFilename);
    writer->SetInputData(vtkPolyData);

    writer->Write();

    std::cout << "Successfully converted ITK mesh to VTK PolyData and saved to " << outputFilename << std::endl;

    return EXIT_SUCCESS;
}
