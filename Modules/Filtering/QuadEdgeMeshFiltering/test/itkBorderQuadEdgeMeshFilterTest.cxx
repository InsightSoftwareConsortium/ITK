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

#include "itkBorderQuadEdgeMeshFilter.h"

int itkBorderQuadEdgeMeshFilterTest( int argc, char* argv[] )
{
  // ** ERROR MESSAGE AND HELP ** //
  if( argc < 5 )
    {
    std::cout <<"Requires 4 arguments: " << std::endl;
    std::cout <<"1-Input file name " << std::endl;
    std::cout <<"2-Border Type" << std::endl;
    std::cout <<"   * 0: SQUARE" << std::endl;
    std::cout <<"   * 1: DISK" << std::endl;
    std::cout <<"3-Border Pick" << std::endl;
    std::cout <<"   * 0: LONGEST" << std::endl;
    std::cout <<"   * 1: LARGEST" << std::endl;
    std::cout <<"4-Output file name " << std::endl;

    return EXIT_FAILURE;
    }


  // ** TYPEDEF **
  typedef double Coord;

  typedef itk::QuadEdgeMesh< Coord, 3 >                        MeshType;
  typedef itk::MeshFileReader< MeshType >                      ReaderType;
  typedef itk::MeshFileWriter< MeshType >                      WriterType;
  typedef itk::BorderQuadEdgeMeshFilter< MeshType, MeshType >  BorderTransformType;


  // ** READ THE FILE IN **
  ReaderType::Pointer reader = ReaderType::New( );
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

  MeshType::Pointer mesh = reader->GetOutput( );

  // ** CHOSE< COMPUTE AND SET BORDER TRANSFORM **
  BorderTransformType::Pointer border_transform = BorderTransformType::New( );
  border_transform->SetInput( mesh );
  // two following line for coverage
  border_transform->SetRadius( border_transform->GetRadius() );
  border_transform->GetNameOfClass();

  int border = atoi( argv[2] );
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
        std::cerr << "0 for SQUARE BORDER TRANSFORM or 1 for DISK BORDER TRANSFORM" << std::endl;
        return EXIT_FAILURE;
    }
  std::cout << "Transform type is: " << border_transform->GetTransformType( );
  std::cout << std::endl;

  int pick = atoi( argv[3] );
  switch( pick )
    {
    case 0:
        border_transform->SetBorderPick( BorderTransformType::LONGEST );
        break;
    case 1:
        border_transform->SetBorderPick( BorderTransformType::LARGEST );
        break;
    default: // handle .... user ....
        std::cerr << "3rd argument must be " << std::endl;
        std::cerr << "0 for LONGEST BORDER or 1 for LARGEST BORDER" << std::endl;
        return EXIT_FAILURE;
    }
  std::cout << "Border picked is: " << border_transform->GetBorderPick( );
  std::cout << std::endl;

  MeshType::Pointer output = border_transform->GetOutput( );

  // ** WRITE OUTPUT **
  WriterType::Pointer writer = WriterType::New( );
  writer->SetInput( border_transform->GetOutput( ) );
  writer->SetFileName( argv[4] );
  writer->Update( );

  // ** PRINT **
  std::cout << "BorderTransform: \n" << border_transform;

  // GET OUT OF HERE AND GET (YET ANOTHER) COFFEE
  return EXIT_SUCCESS;
}
