/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageToParametricSpaceFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkImageToParametricSpaceFilter.h"
#include "itkMesh.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"

int itkImageToParametricSpaceFilterTest(int, char* [] ) 
{

  typedef float  ImagePixelType;

  // Declare the type for the images
  typedef itk::Image<ImagePixelType,2>        ImageType;
  typedef ImageType::Pointer                  ImagePointer;
  typedef ImageType::IndexType                IndexType;

  // Make the Mesh PointData type be an Image Index.
  typedef itk::Point<float,2>                 MeshPixelType;
  
  // Declare the types of the Mesh
  typedef itk::Mesh<MeshPixelType>  MeshType;

  // Declare the type for PointsContainer
  typedef MeshType::PointsContainer     PointsContainerType;

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
    if( point[0] != ix.Value() )
      {
      ok = false;
      break;
      }
    if( point[1] != iy.Value() )
      {
      ok = false;
      break;
      }
    if( point[2] != iz.Value() )
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




