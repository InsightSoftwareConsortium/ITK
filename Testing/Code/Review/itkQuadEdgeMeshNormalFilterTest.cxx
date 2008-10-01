#include <itkVector.h>
#include <itkQuadEdgeMesh.h>
#include <itkVTKPolyDataReader.h>

#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkQuadEdgeMeshNormalFilter.h"

using namespace itk;
using namespace std;

int itkQuadEdgeMeshNormalFilterTest( int argc, char* argv[] )
{
    const unsigned int Dimension = 3;
    typedef double CoordType;
    typedef QuadEdgeMesh< CoordType, Dimension > InputMeshType;

    typedef Vector< CoordType, Dimension > VectorType;

    typedef QuadEdgeMeshExtendedTraits <
            VectorType,
            Dimension,
            2,
            CoordType,
            CoordType,
            VectorType,
            bool,
            bool > Traits;
    typedef QuadEdgeMesh < VectorType, Dimension, Traits > OutputMeshType;

    typedef VTKPolyDataReader< InputMeshType > ReaderType;

    ReaderType::Pointer reader = ReaderType::New( );
    reader->SetFileName( argv[1] );
    try
    {
      reader->Update( );
    }
    catch( itk::ExceptionObject & exp )
    {
      std::cerr << "Exception thrown while reading the input file " 
        << std::endl;
      std::cerr << exp << std::endl;
      return EXIT_FAILURE;
    }

    InputMeshType::Pointer mesh = reader->GetOutput( );

    typedef QuadEdgeMeshNormalFilter< InputMeshType, OutputMeshType > 
      NormalFilterType;
    NormalFilterType::Pointer normals = NormalFilterType::New( );
    normals->SetInput( mesh );
    normals->Update( );

    OutputMeshType::Pointer output = normals->GetOutput( );

//     OutputMeshType::PointDataContainerPointer pointdata =
//       output->GetPointData( );
// 
//     cout <<"*********************************" <<endl;
//     cout <<"Vertex Normal" <<endl;
//     for( OutputMeshType::PointDataContainerIterator
//           d_it = pointdata->Begin( );
//          d_it != pointdata->End( );
//          d_it++ )
//     {
//         cout <<d_it->Index( ) <<"  " <<d_it->Value( ) <<endl;
//     }
// 
//     cout <<endl;
//     cout <<"*********************************" <<endl;
//     cout <<"Face Normal" <<endl;
// 
//     OutputMeshType::CellDataContainerPointer celldata =
//       output->GetCellData( );
// 
// 
//     for( OutputMeshType::CellDataContainerIterator
//           n_it = celldata->Begin( );
//          n_it != celldata->End( );
//          n_it++ )
//     {
//         cout <<n_it->Index( ) <<"  " <<n_it->Value( ) <<endl;
//     }

    return EXIT_SUCCESS;
}
