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
#ifndef itkMeshFileTestHelper_h
#define itkMeshFileTestHelper_h

#include "itkMeshFileReader.h"
#include "itkMeshFileWriter.h"
#include <itksys/SystemTools.hxx>

template< typename TMesh >
int
TestPointsContainer( typename TMesh::PointsContainerPointer points0,
                     typename TMesh::PointsContainerPointer points1 )
{
  typedef TMesh                                           MeshType;
  typedef typename MeshType::PointsContainerConstIterator PointsContainerConstIterator;

  if ( points0.IsNotNull() && points1.IsNotNull() )
    {
    if( points0->Size() != points1->Size() )
      {
      std::cerr << "Input mesh and output mesh have different number of cells!" <<std::endl;
      return EXIT_FAILURE;
      }

    PointsContainerConstIterator pt0 = points0->Begin();
    PointsContainerConstIterator pt1 = points1->Begin();

    const double tol = 1e-6;
    while ( ( pt0 != points0->End() ) && ( pt1 != points1->End() ) )
      {
      if( pt0->Index() != pt1->Index() )
        {
        std::cerr << "Input mesh and output mesh are different in points!" << std::endl;
        std::cerr << "Input point ID  = "   << pt0.Index() << std::endl;
        std::cerr << "Output point ID = "   << pt1.Index() << std::endl;
        return EXIT_FAILURE;
        }
      if ( pt0.Value().SquaredEuclideanDistanceTo( pt1.Value() ) > tol )
        {
        std::cerr << "Input mesh and output mesh are different in points!" << std::endl;
        std::cerr << "Input point = "  << pt0.Value() << std::endl;
        std::cerr << "Output point = " << pt1.Value() << std::endl;
        return EXIT_FAILURE;
        }
       ++pt0;
       ++pt1;
      }
    }
  else
    {
    if ( points0.GetPointer() != points1.GetPointer() )
      {
      std::cerr << "Input mesh and output mesh are different in points!" << std::endl;
      std::cerr << "points0 = "  << points0.GetPointer() << std::endl;
      std::cerr << "points1 = " << points1.GetPointer() << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}

template< typename TMesh >
int
TestCellsContainer( typename TMesh::CellsContainerPointer cells0,
                    typename TMesh::CellsContainerPointer cells1 )
{
  typedef TMesh                                           MeshType;
  typedef typename MeshType::CellsContainerConstIterator  CellsContainerConstIterator;
  typedef typename MeshType::CellType::PointIdIterator    CellPointIdIterator;

  if ( cells0.IsNotNull() && cells1.IsNotNull() )
    {
    if( cells0->Size() != cells1->Size() )
      {
      std::cerr << "Input mesh and output mesh have different number of cells!" <<std::endl;
      return EXIT_FAILURE;
      }
    CellsContainerConstIterator ceIt0 = cells0->Begin();
    CellsContainerConstIterator ceIt1 = cells1->Begin();

    while ( ( ceIt0 != cells0->End() )  && ( ceIt1 != cells1->End() ) )
      {
      if ( ceIt0.Value()->GetType() != ceIt1.Value()->GetType() )
        {
        std::cerr << "Input mesh and output mesh are different in cell type!" << std::endl;
        return EXIT_FAILURE;
        }
      if ( ceIt0.Index() != ceIt1.Index() )
        {
        std::cerr << "Input mesh and output mesh have different cell IDs" << std::endl;
        std::cerr << "Input mesh cell ID: "   << ceIt0.Index() << std::endl;
        std::cerr << "Output mesh cell ID: "  << ceIt1.Index() << std::endl;
        return EXIT_FAILURE;
        }
      CellPointIdIterator pit0 = ceIt0.Value()->PointIdsBegin();
      CellPointIdIterator pit1 = ceIt1.Value()->PointIdsBegin();
      while ( pit0 != ceIt0.Value()->PointIdsEnd() )
        {
        if ( *pit0 != *pit1 )
          {
          std::cerr << "Input mesh and output mesh are different in cells!" << std::endl;
          return EXIT_FAILURE;
          }
        ++pit0;
        ++pit1;
        }
      ++ceIt0;
      ++ceIt1;
      }
    }
  else
    {
    if ( cells0.GetPointer() != cells1.GetPointer() )
      {
      std::cerr << "Input mesh and output mesh are different in cells!" << std::endl;
      std::cerr << "cells0 = "  << cells0.GetPointer() << std::endl;
      std::cerr << "cells1 = " << cells1.GetPointer() << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}

template< typename TMesh >
int
TestPointDataContainer( typename TMesh::PointDataContainerPointer pointData0,
                       typename TMesh::PointDataContainerPointer pointData1 )
{
  typedef TMesh                                         MeshType;
  typedef typename MeshType::PointDataContainerIterator PointDataContainerIterator;

  if ( pointData0.IsNotNull() && pointData1.IsNotNull() )
    {
    if( pointData0->Size() != pointData1->Size() )
      {
      std::cerr << "Input mesh and output mesh have different number of cells!" <<std::endl;
      return EXIT_FAILURE;
      }
    PointDataContainerIterator pdIt0 = pointData0->Begin();
    PointDataContainerIterator pdIt1 = pointData1->Begin();

    while ( ( pdIt0 != pointData0->End() ) && ( pdIt1 != pointData1->End() ) )
      {
      if( pdIt0->Index() != pdIt1->Index() )
        {
        std::cerr << "Input mesh and output mesh are different in point data!" << std::endl;
        std::cerr << "Input point ID  = "   << pdIt0.Index() << std::endl;
        std::cerr << "Output point ID = "   << pdIt1.Index() << std::endl;
        return EXIT_FAILURE;
        }
      if ( itk::Math::NotExactlyEquals(pdIt0.Value(), pdIt1.Value()) )
        {
        std::cerr << "Input mesh and output mesh are different in point data!" << std::endl;
        std::cerr << "Input = "  << pdIt0.Value() << std::endl;
        std::cerr << "Output = " << pdIt1.Value() << std::endl;
        return EXIT_FAILURE;
        }
      ++pdIt0;
      ++pdIt1;
      }
    }
  else
    {
    if ( pointData0.GetPointer() != pointData1.GetPointer() )
      {
      std::cerr << "Input mesh and output mesh are different in point data!" << std::endl;
      std::cerr << "pointData0 = "  << pointData0.GetPointer() << std::endl;
      std::cerr << "pointData1 = " << pointData1.GetPointer() << std::endl;
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}

template< typename TMesh >
int
TestCellDataContainer( typename TMesh::CellDataContainerPointer cellData0,
                      typename TMesh::CellDataContainerPointer cellData1 )
{
  typedef TMesh                                        MeshType;
  typedef typename MeshType::CellDataContainerIterator CellDataContainerIterator;

  if ( cellData0.IsNotNull() && cellData1.IsNotNull() )
    {
    if( cellData0->Size() != cellData1->Size() )
      {
      std::cerr << "Input mesh and output mesh have different number of cells!" <<std::endl;
      return EXIT_FAILURE;
      }

    CellDataContainerIterator cdIt0 = cellData0->Begin();
    CellDataContainerIterator cdIt1 = cellData1->Begin();
    while ( cdIt0 != cellData0->End() )
      {
      if( cdIt0->Index() != cdIt1->Index() )
        {
        std::cerr << "Input mesh and output mesh are different in cell data!" << std::endl;
        std::cerr << "Input cell ID  = "   << cdIt0.Index() << std::endl;
        std::cerr << "Output cell ID = "   << cdIt1.Index() << std::endl;
        return EXIT_FAILURE;
        }
      if ( itk::Math::NotExactlyEquals(cdIt0.Value(), cdIt1.Value()) )
        {
        std::cerr << "Input mesh and output mesh are different in cell data!" << std::endl;
        std::cerr << "Input = "  << cdIt0.Value() << std::endl;
        std::cerr << "Output = " << cdIt1.Value() << std::endl;
        return EXIT_FAILURE;
        }
      ++cdIt0;
      ++cdIt1;
      }
    }
  else
    {
    if ( cellData0.GetPointer() != cellData1.GetPointer() )
      {
      std::cerr << "Input mesh and output mesh are different in cell data!" << std::endl;
      std::cerr << "pointData0 = "  << cellData0.GetPointer() << std::endl;
      std::cerr << "pointData1 = " << cellData1.GetPointer() << std::endl;
      return EXIT_FAILURE;
      }
    }
  return EXIT_SUCCESS;
}

template< typename TMesh >
int
test(char *INfilename, char *OUTfilename, bool IsBinary)
{
  typedef TMesh MeshType;

  typedef itk::MeshFileReader< MeshType >       MeshFileReaderType;
  typedef typename MeshFileReaderType::Pointer  MeshFileReaderPointer;

  typedef itk::MeshFileWriter< MeshType >       MeshFileWriterType;
  typedef typename MeshFileWriterType::Pointer  MeshFileWriterPointer;

  MeshFileReaderPointer reader = MeshFileReaderType::New();
  reader->SetFileName(INfilename);
  try
    {
    reader->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    std::cerr << "Read file " << INfilename << " failed " << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  MeshFileWriterPointer writer = MeshFileWriterType::New();
  if( itksys::SystemTools::GetFilenameLastExtension(INfilename) ==
      itksys::SystemTools::GetFilenameLastExtension(OUTfilename) )
    {
    writer->SetMeshIO(reader->GetModifiableMeshIO());
    }
  writer->SetFileName(OUTfilename);
  writer->SetInput( reader->GetOutput() );

  // NOTE ALEX: we should document the usage
  if ( IsBinary )
    {
    writer->SetFileTypeAsBINARY();
    }

  try
    {
    writer->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    std::cerr << "Write file " << OUTfilename << " failed " << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  if ( !itksys::SystemTools::FilesDiffer(INfilename, OUTfilename) )
    {
    return EXIT_SUCCESS;
    }

  typename MeshFileReaderType::Pointer reader1 = MeshFileReaderType::New();
  reader1->SetFileName(OUTfilename);
  try
    {
    reader1->Update();
    }
  catch ( itk::ExceptionObject & err )
    {
    std::cerr << "Read file " << OUTfilename << " failed " << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
    }

  // Test points
  if( TestPointsContainer< MeshType >(  reader->GetOutput()->GetPoints(),
                                        reader1->GetOutput()->GetPoints() ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }


  // Test cells
  if( TestCellsContainer< MeshType >( reader->GetOutput()->GetCells(),
                                      reader1->GetOutput()->GetCells() ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }


  // Test point data
  if( TestPointDataContainer< MeshType >( reader->GetOutput()->GetPointData(),
                                          reader1->GetOutput()->GetPointData() ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }


  // Test cell data
  if( TestCellDataContainer< MeshType >( reader->GetOutput()->GetCellData(),
                                         reader1->GetOutput()->GetCellData() ) == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
#endif
