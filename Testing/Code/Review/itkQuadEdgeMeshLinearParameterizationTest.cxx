// BELONG TO \REVIEW
#include <itkVTKPolyDataReader.h>
#include <itkVTKPolyDataWriter.h>
#include <itkQuadEdgeMesh.h>

// NEW
#include "itkQuadEdgeMeshBorderTransform.h"
#include "itkQuadEdgeMeshParamMatrixCoefficients.h"

// TIMING
#include <ctime>

#ifdef QuadEdgePARAM_TAUCS
    #include "TAUCSSparseSolverTraits.h"
#else
    #include "VNLIterativeSparseSolverTraits.h"
#endif

#include "itkQuadEdgeMeshParam.h"

using namespace itk;

int itkQuadEdgeMeshLinearParameterizationTest( int argc, char* argv[] )
{
    // ** ERROR MESSAGE AND HELP ** //
    if( argc < 5 )
    {
        std::cout <<"Requires 4 arguments: " <<std::endl;
        std::cout <<"1-Input file name " <<std::endl;
        std::cout <<"2-Border Type" <<std::endl;
        std::cout <<"   * 0: SQUARE" <<std::endl;
        std::cout <<"   * 1: DISK" <<std::endl;
        std::cout <<"3-CoefficientType Type" <<std::endl;
        std::cout <<"   * 0: OnesMatrixCoefficients" <<std::endl;
        std::cout <<"   * 1: InverseEuclideanDistanceMatrixCoefficients"
          <<std::endl;
        std::cout <<"   * 2: ConformalMatrixCoefficients" <<std::endl;
        std::cout <<"   * 3: AuthalicMatrixCoefficients" <<std::endl;
        std::cout <<"   * 4: HarmonicMatrixCoefficients" <<std::endl;
        std::cout <<"4-Output file name " <<std::endl;

        return EXIT_FAILURE;
    }


    // ** TYPEDEF **
    typedef double Coord;

    typedef QuadEdgeMesh< Coord, 3 >                      MeshType;
    typedef VTKPolyDataReader< MeshType >                 ReaderType;
    typedef VTKPolyDataWriter< MeshType >                 WriterType;
    typedef QuadEdgeMeshBorderTransform< MeshType, MeshType >     BorderTransformType;
#ifdef QuadEdgePARAM_TAUCS
    typedef TAUCSSolverTraits< Coord >                    SolverTraits;
#else
    typedef VNLIterativeSparseSolverTraits< Coord >       SolverTraits;
#endif
    typedef QuadEdgeMeshParam< MeshType, MeshType, SolverTraits > ParametrizationType;


    // ** READ THE FILE IN **
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

    MeshType::Pointer mesh = reader->GetOutput( );

    // ** CHOSE< COMPUTE AND SET BORDER TRANSFORM **
    // clock_t start = clock( );
    BorderTransformType::Pointer border_transform = BorderTransformType::New( );
    border_transform->SetInput( mesh );

    int border;
    std::stringstream ssout( argv[2] );
    ssout >>border;
    switch( border )  // choose border type
    {
        case 0: // square shaped domain
            border_transform->SetTransformType(
                BorderTransformType::SQUARE_BORDER_TRANSFORM ); 
            break;
        case 1: // disk shaped domain
            border_transform->SetTransformType(
                BorderTransformType::DISK_BORDER_TRANSFORM );   
            break;
        default: // handle .... user ....
            std::cerr <<"2nd argument must be "<<std::endl;
            std::cerr <<"0 for SQUARE BORDER TRANSFORM or "
              <<"1 for DISK BORDER TRANSFORM" <<std::endl;
            return EXIT_FAILURE;
    }

    // ** CHOOSE AND SET BARYCENTRIC WEIGHTS **
    OnesMatrixCoefficients< MeshType >                     coeff0;
    InverseEuclideanDistanceMatrixCoefficients< MeshType > coeff1;
    ConformalMatrixCoefficients< MeshType >                coeff2;
    AuthalicMatrixCoefficients< MeshType >                 coeff3;
    HarmonicMatrixCoefficients< MeshType >                 coeff4;

    ParametrizationType::Pointer param = ParametrizationType::New( );
    param->SetInput( mesh );
    param->SetBorderTransform( border_transform );

    int param_type;
    std::stringstream ssout3( argv[3] );
    ssout3 >>param_type;

    switch( param_type )
    {
        case 0:
            param->SetCoefficientsMethod( &coeff0 );
            break;
        case 1:
            param->SetCoefficientsMethod( &coeff1 );
            break;
        case 2:
            param->SetCoefficientsMethod( &coeff2 );
            break;
        case 3:
            param->SetCoefficientsMethod( &coeff3 );
            break;
        case 4:
            param->SetCoefficientsMethod( &coeff4 );
            break;
        default:
            std::cerr <<"3rd argument must be "<<std::endl;
            std::cerr <<"0, 1, 2, 3 or 4" <<std::endl;
            std::cerr <<"Here it is: " << param_type << std::endl;
            return EXIT_FAILURE;
    }

    // ** PROCESS **
    param->Update( );
    MeshType::Pointer output = param->GetOutput( );

    // ** TIMING **
    // clock_t end = clock( );
    // std::cout <<"Time: ";
    // std::cout <<static_cast< double >( end - start ) /
    //        static_cast< double >( CLOCKS_PER_SEC ) <<" s" <<std::endl;

    // ** WRITE OUTPUT **
    WriterType::Pointer writer = WriterType::New( );
    writer->SetInput( param->GetOutput( ) );
    writer->SetFileName( argv[4] );
    writer->Update( );

    // GET OUT OF HERE AND GET (YET ANOTHER) COFFEE
    return EXIT_SUCCESS;
}
