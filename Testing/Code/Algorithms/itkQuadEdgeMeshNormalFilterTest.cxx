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
#include "itkVector.h"
#include "itkQuadEdgeMesh.h"
#include "itkVTKPolyDataReader.h"

#include "itkQuadEdgeMeshExtendedTraits.h"
#include "itkQuadEdgeMeshNormalFilter.h"

int itkQuadEdgeMeshNormalFilterTest( int argc, char* argv[] )
{
  if( argc < 2 )
    {
    std::cout <<"This program requires at least 1 argument" <<std::endl;
    std::cout <<"1- Input filename" <<std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int    Dimension = 3;
  typedef double        CoordType;

  typedef itk::QuadEdgeMesh< CoordType, Dimension > InputMeshType;

  typedef itk::Vector< CoordType, Dimension > VectorType;

  typedef itk::QuadEdgeMeshExtendedTraits <
    VectorType,
    Dimension,
    2,
    CoordType,
    CoordType,
    VectorType,
    bool,
    bool > Traits;

  typedef itk::QuadEdgeMesh < VectorType, Dimension, Traits > OutputMeshType;

  typedef itk::VTKPolyDataReader< InputMeshType > ReaderType;

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

  InputMeshType::Pointer mesh = reader->GetOutput( );

  typedef itk::QuadEdgeMeshNormalFilter< InputMeshType, OutputMeshType > NormalFilterType;

  NormalFilterType::Pointer normals = NormalFilterType::New( );
  normals->SetInput( mesh );
  normals->Update( );

  OutputMeshType::Pointer output = normals->GetOutput( );

  //
  //   FIXME
  //
  //     OutputMeshType::PointDataContainerPointer pointdata =
  //       output->GetPointData( );
  //
  //     std::cout <<"*********************************" <<endl;
  //     std::cout <<"Vertex Normal" <<endl;
  //     for( OutputMeshType::PointDataContainerIterator
  //           d_it = pointdata->Begin( );
  //          d_it != pointdata->End( );
  //          d_it++ )
  //       {
  //       std::cout <<d_it->Index( ) <<"  " <<d_it->Value( ) <<endl;
  //       }
  //
  //     std::cout <<endl;
  //     std::cout <<"*********************************" <<endl;
  //     std::cout <<"Face Normal" <<endl;
  //
  //     OutputMeshType::CellDataContainerPointer celldata =
  //       output->GetCellData( );
  //
  //
  //     for( OutputMeshType::CellDataContainerIterator
  //           n_it = celldata->Begin( );
  //          n_it != celldata->End( );
  //          n_it++ )
  //       {
  //       std::cout <<n_it->Index( ) <<"  " <<n_it->Value( ) <<endl;
  //       }

  // ** PRINT **
  std::cout << normals;
  return EXIT_SUCCESS;
}
