#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkConformalFlatteningFilter.h"

//
// ITK Headers
// 
#include "itkMesh.h"
#include "itkLineCell.h"
#include "itkTriangleCell.h"



#if 0
//
// VTK headers
//
#include "vtkPolyDataReader.h"
#include "vtkPolyData.h"
#include "vtkPoints.h"
#include "vtkCellArray.h"

#include "vtkPolyDataMapper.h"
#include "vtkRenderer.h"
#include "vtkRenderWindow.h"
#include "vtkActor.h"
#include "vtkRenderWindowInteractor.h"

#include "vtkCurvatures.h"
#include "vtkLookupTable.h"
#include "vtkPointData.h"
#include "vtkPolyDataNormals.h"
#include "vtkPolyDataWriter.h"
#endif



const unsigned int pointDimension   = 3;
const unsigned int maxCellDimension = 2;

typedef double vtkFloatingPointType;
typedef itk::Point<vtkFloatingPointType, pointDimension> ItkPoint;

typedef itk::DefaultStaticMeshTraits<
  vtkFloatingPointType,
  pointDimension,
  maxCellDimension,
  vtkFloatingPointType,
  vtkFloatingPointType  >       MeshTraits;

typedef itk::Mesh<
  vtkFloatingPointType,
  pointDimension,
  MeshTraits              >     MeshType;


typedef MeshType::PointsContainer::ConstIterator PointIterator;


typedef MeshType::CellType CellType;
typedef MeshType::CellsContainer::ConstIterator CellIterator;

typedef CellType::PointIdIterator PointIdIterator;



typedef itk::TriangleCell< CellType > TriangleCellType;


MeshType::Pointer vtkPolyDataToITKMesh(std::string inputFilename);
//vtkPolyData* readDataToPolyData(char* fName);
//vtkPolyData* ITKMeshToVtkPolyData(MeshType::Pointer mesh);
//void Display(vtkPolyData* polyData);
