/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"

#include "VNLSparseLUSolverTraits.h"
#include "VNLIterativeSparseSolverTraits.h"

#include "itkParameterizationQuadEdgeMeshFilter.h"

template< typename TSolver >
int ParameterizationQuadEdgeMeshFilterTest( int argc, char* argv[] )
{
  // ** ERROR MESSAGE AND HELP ** //
  if( argc < 5 )
    {
    std::cout <<"Requires 4 arguments: " << std::endl;
    std::cout <<"1-Input file name " << std::endl;
    std::cout <<"2-Border Type" << std::endl;
    std::cout <<"   * 0: SQUARE" << std::endl;
    std::cout <<"   * 1: DISK" << std::endl;
    std::cout <<"3-CoefficientType Type" << std::endl;
    std::cout <<"   * 0: OnesMatrixCoefficients" << std::endl;
    std::cout <<"   * 1: InverseEuclideanDistanceMatrixCoefficients" << std::endl;
    std::cout <<"   * 2: ConformalMatrixCoefficients" << std::endl;
    std::cout <<"   * 3: AuthalicMatrixCoefficients" << std::endl;
    std::cout <<"   * 4: HarmonicMatrixCoefficients" << std::endl;
    std::cout <<"4-Solver type (0: iterative, 1: LU decomposition)" << std::endl;
    std::cout <<"5-Output file name " << std::endl;

    return EXIT_FAILURE;
    }


  // ** TYPEDEF **
  typedef typename TSolver::ValueType                                  Coord;

  typedef itk::QuadEdgeMesh< Coord, 3 >                                MeshType;
  typedef itk::MeshFileReader< MeshType >                              ReaderType;
  typedef itk::MeshFileWriter< MeshType >                              WriterType;
  typedef itk::BorderQuadEdgeMeshFilter< MeshType, MeshType >          BorderTransformType;


  // ** READ THE FILE IN **
  typename ReaderType::Pointer reader = ReaderType::New( );
  reader->SetFileName( argv[1] );

  try
    {
    reader->Update( );
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Exception thrown while reading the input file " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  typename MeshType::Pointer mesh = reader->GetOutput( );

  // ** CHOSE< COMPUTE AND SET BORDER TRANSFORM **
  typename BorderTransformType::Pointer border_transform = BorderTransformType::New( );
  border_transform->SetInput( mesh );
  // two following line for coverage
  border_transform->SetRadius( border_transform->GetRadius() );
  border_transform->GetNameOfClass();

  int border;
  std::stringstream ssout( argv[2] );
  ssout >>border;
  switch( border )  // choose border type
    {
    case 0: // square shaped domain
        border_transform->SetTransformType( BorderTransformType::SQUARE_BORDER_TRANSFORM );
        break;
    case 1: // disk shaped domain
        border_transform->SetTransformType( BorderTransformType::DISK_BORDER_TRANSFORM );
        break;
    default: // handle .... user ....
        std::cerr << "2nd argument must be " << std::endl;
        std::cerr << "0 for SQUARE BORDER TRANSFORM or "
          << "1 for DISK BORDER TRANSFORM" << std::endl;
        return EXIT_FAILURE;
    }
  std::cout << "Transform type is: " << border_transform->GetTransformType( );
  std::cout << std::endl;

  // ** CHOOSE AND SET BARYCENTRIC WEIGHTS **
  itk::OnesMatrixCoefficients< MeshType >                     coeff0;
  itk::InverseEuclideanDistanceMatrixCoefficients< MeshType > coeff1;
  itk::ConformalMatrixCoefficients< MeshType >                coeff2;
  itk::AuthalicMatrixCoefficients< MeshType >                 coeff3;
  itk::HarmonicMatrixCoefficients< MeshType >                 coeff4;

  typedef itk::ParameterizationQuadEdgeMeshFilter< MeshType, MeshType, TSolver >   ParametrizationType;
  typename ParametrizationType::Pointer param = ParametrizationType::New( );
  param->SetInput( mesh );
  param->SetBorderTransform( border_transform );

  int param_type;
  std::stringstream ssout3( argv[3] );
  ssout3 >> param_type;

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
      std::cerr << "3rd argument must be " << std::endl;
      std::cerr << "0, 1, 2, 3 or 4" << std::endl;
      std::cerr << "Here it is: " << param_type << std::endl;
      return EXIT_FAILURE;
    }

  // ** PROCESS **
  param->Update( );
  typename MeshType::Pointer output = param->GetOutput( );

  // ** WRITE OUTPUT **
  typename WriterType::Pointer writer = WriterType::New( );
  writer->SetInput( param->GetOutput( ) );
  writer->SetFileName( argv[5] );
  writer->Update( );

  // ** PRINT **
  std::cout << "BorderTransform: \n" << border_transform;
  std::cout << "Parametrization: \n" << param;

  // GET OUT OF HERE AND GET (YET ANOTHER) COFFEE
  return EXIT_SUCCESS;
}

int itkParameterizationQuadEdgeMeshFilterTest( int argc, char* argv[] )
{
  typedef double                                    TCoord;
  typedef VNLIterativeSparseSolverTraits< TCoord >  IterativeSolverTraits;
  typedef VNLSparseLUSolverTraits< TCoord >         LUSolverTraits;

  if( atoi( argv[4] ) == 0 )
    {
    return ParameterizationQuadEdgeMeshFilterTest< IterativeSolverTraits >( argc, argv );
    }
  else if( atoi( argv[4] ) == 1 )
    {
    return ParameterizationQuadEdgeMeshFilterTest< LUSolverTraits >( argc, argv );
    }
  else
    {
    return EXIT_FAILURE;
    }
}
