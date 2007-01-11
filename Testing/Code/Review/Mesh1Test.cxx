#include "itkQEMesh.h"

int Mesh1Test( int , char* [] )
{
    std::cout << "Testing points..." << std::endl;

    typedef double PixelType;
    const unsigned int Dimension = 3;
    typedef itkQE::Mesh< PixelType, Dimension > MeshType;

    MeshType::Pointer  mesh = MeshType::New();

    MeshType::PointType pts[ 4 ];

    pts[ 0 ][ 0 ] = -1.0; pts[ 0 ][ 1 ] = -1.0; pts[ 0 ][ 2 ] = 0.0;
    pts[ 1 ][ 0 ] =  1.0; pts[ 1 ][ 1 ] = -1.0; pts[ 1 ][ 2 ] = 0.0;
    pts[ 2 ][ 0 ] =  1.0; pts[ 2 ][ 1 ] =  1.0; pts[ 2 ][ 2 ] = 0.0;
    pts[ 3 ][ 0 ] = -1.0; pts[ 3 ][ 1 ] =  1.0; pts[ 3 ][ 2 ] = 0.0;

    mesh->SetPoint( 0, pts[ 0 ] );
    mesh->SetPoint( 1, pts[ 1 ] );
    mesh->SetPoint( 2, pts[ 2 ] );
    mesh->SetPoint( 3, pts[ 3 ] );

    if( mesh->GetNumberOfPoints() != 4 )
      {
      std::cout << "Not all points added." << std::endl;
      return EXIT_FAILURE;
      }

    typedef MeshType::PointsContainer::Iterator PointsIterator;
    PointsIterator  pointIterator = mesh->GetPoints()->Begin();
    PointsIterator end = mesh->GetPoints()->End();

    int nPoints = 0;
    while( pointIterator != end )
      {
      MeshType::PointType p = pointIterator.Value();

      if( p != pts[ nPoints ] )
        {
        std::cout << "Point N. " << nPoints << " differs." << std::endl;
        return EXIT_FAILURE;
        }

      pointIterator++;
      nPoints++;
      }

    if( nPoints != 4 )
      {
      std::cout << "Iteration didn't visit all points." << std::endl;
      return EXIT_FAILURE;

    }

    std::cout << "Test passed" << std::endl;
    return EXIT_SUCCESS;
}

// eof - Mesh1Test.cxx
