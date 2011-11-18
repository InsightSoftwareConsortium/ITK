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


#include "itkSmoothingQuadEdgeMeshFilter.h"

int itkSmoothingQuadEdgeMeshFilterTest( int argc, char* argv[] )
{
  // ** ERROR MESSAGE AND HELP ** //
  if( argc < 4 )
    {
    std::cout <<"Requires 3 argument: " <<std::endl;
    std::cout <<"1-Input file name " <<std::endl;
    std::cout <<"2-Number Of Iterations " <<std::endl;
    std::cout <<"3-Relaxation Factor" <<std::endl;
    std::cout <<"4-Use Delaunay Conforming filter" <<std::endl;
    std::cout <<"5-Output file name " <<std::endl;
    return EXIT_FAILURE;
    }

  // ** TYPEDEF **
  typedef float Coord;
  const unsigned int Dimension = 3;

  typedef itk::QuadEdgeMesh< Coord, Dimension >  MeshType;
  typedef itk::MeshFileReader< MeshType >        ReaderType;
  typedef itk::MeshFileWriter< MeshType >        WriterType;

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

  unsigned int nb_iter;
  std::stringstream ssout( argv[2] );
  ssout >>nb_iter;

  double relaxation_factor;
  std::stringstream ssout2( argv[3] );
  ssout2 >>relaxation_factor;

  bool del_conf;
  std::stringstream ssout3( argv[4] );
  ssout3 >>del_conf;

  MeshType::Pointer mesh = reader->GetOutput( );

  itk::OnesMatrixCoefficients< MeshType > coeff0;

  typedef itk::SmoothingQuadEdgeMeshFilter< MeshType, MeshType > SmoothingType;
  SmoothingType::Pointer filter = SmoothingType::New( );
  filter->SetInput( mesh );
  filter->SetNumberOfIterations( nb_iter );
  filter->SetRelaxationFactor( relaxation_factor );
  filter->SetDelaunayConforming( del_conf );
  filter->SetCoefficientsMethod( &coeff0 );
  filter->Update();

  // ** WRITE OUTPUT **
  WriterType::Pointer writer = WriterType::New( );
  writer->SetInput( filter->GetOutput( ) );
  writer->SetFileName( argv[5] );
  writer->Update( );

  // ** PRINT **
  std::cout << filter;
  return EXIT_SUCCESS;
}
