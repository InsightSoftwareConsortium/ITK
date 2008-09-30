#include <itkQuadEdgeMesh.h>
#include <itkVTKPolyDataReader.h>

#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkQuadEdgeMeshDiscreteMinCurvatureEstimator.h"
#include "itkQuadEdgeMeshScalarDataVTKPolyDataWriter.h"

using namespace itk;

int itkQuadEdgeMeshMinCurvatureTest( int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cout <<"*** GaussianCurvature ***" <<std::endl;
    std::cout <<"This example requires at least one argument:" <<std::endl;
    std::cout <<" 1- FileName" <<std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 3;
  typedef double CoordType;

  typedef QuadEdgeMeshExtendedTraits <
    CoordType,
    Dimension,
    2,
    CoordType,
    CoordType,
    CoordType,
    bool,
    bool > Traits;

  typedef QuadEdgeMesh< CoordType, Dimension, Traits > MeshType;
  typedef QuadEdgeMeshDiscreteMinCurvatureEstimator<MeshType,MeshType>
    CurvatureFilterType;

  typedef VTKPolyDataReader< MeshType > ReaderType;

  ReaderType::Pointer reader = ReaderType::New( );
  reader->SetFileName( argv[1] );
  try
    {
    reader->Update( );
    }
  catch( itk::ExceptionObject & exp )
    {
    std::cerr << "Exception thrown while reading the input file " << std::endl;
    std::cerr << exp << std::endl;
    return EXIT_FAILURE;
    }

  MeshType::Pointer mesh = reader->GetOutput();

  CurvatureFilterType::Pointer min_curvature = CurvatureFilterType::New();
  min_curvature->SetInput( mesh );
  min_curvature->Update();

  MeshType::Pointer output = min_curvature->GetOutput();

  typedef QuadEdgeMeshScalarDataVTKPolyDataWriter< MeshType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( output );
  writer->SetFileName( "min_curvature.vtk" );
  writer->Update();

  return EXIT_SUCCESS;
}
