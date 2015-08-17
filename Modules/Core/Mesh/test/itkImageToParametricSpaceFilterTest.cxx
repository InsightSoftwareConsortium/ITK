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

#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageToParametricSpaceFilter.h"
#include "itkMesh.h"
#include "itkMath.h"

int itkImageToParametricSpaceFilterTest(int, char* [] )
{

  typedef float  ImagePixelType;

  // Declare the type for the images
  typedef itk::Image<ImagePixelType,2>        ImageType;
  typedef ImageType::Pointer                  ImagePointer;

  // Make the Mesh PointData type be an Image Index.
  typedef itk::Point<float,2>                 MeshPixelType;

  // Declare the types of the Mesh
  typedef itk::Mesh<MeshPixelType>  MeshType;

  // Declare the type for PointsContainerPointer
  typedef MeshType::PointsContainerPointer
                                        PointsContainerPointer;
  // Declare the type for Points
  typedef MeshType::PointType           PointType;

  // Create an input Mesh
  MeshType::Pointer inputMesh  = MeshType::New();

  // Insert data on the Mesh
  PointsContainerPointer  points = inputMesh->GetPoints();


  // Declare the type for the images
  typedef itk::ImageRegionIteratorWithIndex<ImageType> ImageIteratorType;

  // Declare the type for the filter
  typedef itk::ImageToParametricSpaceFilter<
                                       ImageType,
                                       MeshType   > FilterType;

  typedef FilterType::Pointer                     FilterPointer;


  ImagePointer imageX = ImageType::New();
  ImagePointer imageY = ImageType::New();
  ImagePointer imageZ = ImageType::New();

  ImageType::SizeType  size;
  ImageType::IndexType start;

  start.Fill(0);
  size[0] = 10;
  size[1] = 10;

  ImageType::RegionType region;
  region.SetSize( size );
  region.SetIndex( start );

  imageX->SetLargestPossibleRegion( region );
  imageY->SetLargestPossibleRegion( region );
  imageZ->SetLargestPossibleRegion( region );

  imageX->SetBufferedRegion( region );
  imageY->SetBufferedRegion( region );
  imageZ->SetBufferedRegion( region );

  imageX->SetRequestedRegion( region );
  imageY->SetRequestedRegion( region );
  imageZ->SetRequestedRegion( region );

  imageX->Allocate();
  imageY->Allocate();
  imageZ->Allocate();

  ImageIteratorType   ix( imageX, region );
  ImageIteratorType   iy( imageY, region );
  ImageIteratorType   iz( imageZ, region );

  ix.GoToBegin();
  iy.GoToBegin();
  iz.GoToBegin();


  while( ! ix.IsAtEnd() )
    {
      ix.Set( rand() );
      ++ix;
    }

  while( ! iy.IsAtEnd() )
    {
      iy.Set( rand() );
      ++iy;
    }

  while( ! iz.IsAtEnd() )
    {
      iz.Set( rand() );
      ++iz;
    }


  FilterPointer filter = FilterType::New();

  // Connect the inputs
  filter->SetInput( 0, imageX );
  filter->SetInput( 1, imageY );
  filter->SetInput( 2, imageZ );

  // Execute the filter
  filter->Update();

  // Get the Smart Pointer to the Filter Output
  MeshType::Pointer outputMesh = filter->GetOutput();

  // Get the the point container
  MeshType::PointsContainer::Iterator beginPoint =
                           outputMesh->GetPoints()->Begin();

  MeshType::PointsContainer::Iterator endPoint =
                           outputMesh->GetPoints()->End();

  MeshType::PointsContainer::Iterator pointIt = beginPoint;

  bool ok = true;

  ix.GoToBegin();
  iy.GoToBegin();
  iz.GoToBegin();

  while( pointIt != endPoint )
    {
    PointType point = pointIt.Value();
    if( itk::Math::NotExactlyEquals(point[0], ix.Value()) )
      {
      ok = false;
      break;
      }
    if( itk::Math::NotExactlyEquals(point[1], iy.Value()) )
      {
      ok = false;
      break;
      }
    if( itk::Math::NotExactlyEquals(point[2], iz.Value()) )
      {
      ok = false;
      break;
      }

    ++pointIt;
    ++ix;
    ++iy;
    ++iz;
    }

  // All objects should be automatically destroyed at this point

  if( !ok )
    {
    return EXIT_FAILURE;
    }
  return EXIT_SUCCESS;

}
