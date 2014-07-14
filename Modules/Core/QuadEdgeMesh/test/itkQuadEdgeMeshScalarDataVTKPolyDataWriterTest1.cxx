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

#include "itkQuadEdgeMesh.h"
#include "itkRegularSphereMeshSource.h"
#include "itkQuadEdgeMeshScalarDataVTKPolyDataWriter.h"

#include <iostream>

int itkQuadEdgeMeshScalarDataVTKPolyDataWriterTest1( int argc, char * argv [] )
{
  if( argc < 2 )
    {
    std::cerr << "Missing Arguments" << std::endl;
    std::cerr << "Usage" << std::endl;
    std::cerr << argv[0] << " outputFileName.vtk" << std::endl;
    return EXIT_FAILURE;
    }

  typedef itk::QuadEdgeMesh<float, 3>   MeshType;

  typedef itk::RegularSphereMeshSource< MeshType >  SphereMeshSourceType;

  SphereMeshSourceType::Pointer  mySphereMeshSource = SphereMeshSourceType::New();

  typedef SphereMeshSourceType::PointType   PointType;
  typedef SphereMeshSourceType::VectorType  VectorType;

  PointType center;
  center.Fill( 0.0 );

  VectorType scale;
  scale.Fill( 1.0 );

  mySphereMeshSource->SetCenter( center );
  mySphereMeshSource->SetResolution( 1 );
  mySphereMeshSource->SetScale( scale );

  mySphereMeshSource->Modified();

  try
    {
    mySphereMeshSource->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during source Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "mySphereMeshSource: " << mySphereMeshSource;

  MeshType::Pointer myMesh = mySphereMeshSource->GetOutput();

  PointType pt;
  pt.Fill( 0. );

  std::cout << "Testing itk::RegularSphereMeshSource "<< std::endl;

  myMesh->Print( std::cout );

  for( unsigned int i=0; i < myMesh->GetNumberOfPoints(); i++ )
    {
    myMesh->GetPoint(i, &pt);
    std::cout << "Point[" << i << "]: " << pt << std::endl;
    }

  typedef MeshType::CellsContainerPointer  CellsContainerPointer;
  typedef MeshType::CellType               CellType;

  CellsContainerPointer cells = myMesh->GetCells();

  unsigned faceId = 0;

  for( MeshType::CellsContainerIterator cells_it = cells->Begin();
       cells_it != cells->End();
       ++cells_it, faceId++ )
    {
    CellType* cellPointer = cells_it.Value();
    if( cellPointer->GetType() != 1 )
      {
      std::cout <<"Face " << faceId << " has " << cellPointer->GetNumberOfPoints()
                <<" points" << std::endl;
      }
    }

  std::cout << "Test End "<< std::endl;

  typedef itk::QuadEdgeMeshScalarDataVTKPolyDataWriter< MeshType >   WriterType;

  WriterType::Pointer writer = WriterType::New();

  writer->SetInput( mySphereMeshSource->GetOutput() );
  writer->SetFileName( argv[1] );

  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error during writer Update() " << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;

}
