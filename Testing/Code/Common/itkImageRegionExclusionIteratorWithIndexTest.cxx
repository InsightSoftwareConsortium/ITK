/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionExclusionIteratorWithIndexTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>

#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionExclusionIteratorWithIndex.h"
#include "itkImageRegionExclusionConstIteratorWithIndex.h"




int itkImageRegionExclusionIteratorWithIndexTest(int, char* [] )
{

  const unsigned int ImageDimension = 3;

  typedef itk::Index< ImageDimension >             IndexPixelType;
  typedef unsigned char                            ValuePixelType;

  typedef itk::Image< IndexPixelType, ImageDimension >  IndexImageType;
  typedef itk::Image< ValuePixelType, ImageDimension >  ValueImageType;

  IndexImageType::Pointer myIndexImage = IndexImageType::New();
  
  IndexImageType::SizeType size;

  size[0] = 7;
  size[1] = 7;
  size[2] = 7;

  IndexImageType::IndexType start;
  start.Fill(0);

  IndexImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myIndexImage->SetLargestPossibleRegion( region );
  myIndexImage->SetBufferedRegion( region );
  myIndexImage->SetRequestedRegion( region );
  myIndexImage->Allocate();

  ValueImageType::Pointer  myValueImage  = ValueImageType::New();

  myValueImage->SetLargestPossibleRegion( region );
  myValueImage->SetBufferedRegion( region );
  myValueImage->SetRequestedRegion( region );
  myValueImage->Allocate();

  typedef itk::ImageRegionIteratorWithIndex< ValueImageType >     ValueIteratorType;
  typedef itk::ImageRegionIteratorWithIndex< IndexImageType >     IndexIteratorType;

  const unsigned char normalRegionValue    = 100;
  const unsigned char exclusionRegionValue = 200;

  std::cout << "Initializing an image of indices and an image of values" << std::endl;

  // Initialize the Image
  IndexIteratorType ii( myIndexImage, region );
  ValueIteratorType iv( myValueImage, region );

  ii.GoToBegin();
  iv.GoToBegin();

  while( !ii.IsAtEnd() )
    {
    ii.Set( ii.GetIndex() );
    iv.Set( normalRegionValue );  
    ++ii;
    ++iv;
    }
  
  IndexImageType::RegionType exclusionRegion;
 
  IndexImageType::SizeType exclusionSize;

  exclusionSize[0] = 3;
  exclusionSize[1] = 3;
  exclusionSize[2] = 3;

  IndexImageType::IndexType exclusionStart;

  exclusionStart[0] = 2;
  exclusionStart[1] = 2;
  exclusionStart[2] = 2;
  
  exclusionRegion.SetIndex( exclusionStart );
  exclusionRegion.SetSize(  exclusionSize  );
  
  std::cout << "Initializing the exclusion region on the image of values " << std::endl;
  // Set a different value inside the exclusion region
  ValueIteratorType ive( myValueImage, exclusionRegion );

  ive.GoToBegin();
  while( !ive.IsAtEnd() )
    {
    ive.Set( exclusionRegionValue );
    ++ive;
    }


  std::cout << "Starting walk with the exclusion iterator... ";
  typedef itk::ImageRegionExclusionIteratorWithIndex< IndexImageType > ExclusionIndexIteratorType;
  typedef itk::ImageRegionExclusionIteratorWithIndex< ValueImageType > ExclusionValueIteratorType;

  ExclusionValueIteratorType ev( myValueImage, region );
  ExclusionIndexIteratorType ei( myIndexImage, region );

  ev.SetExclusionRegion( exclusionRegion );
  ei.SetExclusionRegion( exclusionRegion );
  
  unsigned int numberOfPixelsVisited = 0;
  const unsigned int pixelsToVisit  = region.GetNumberOfPixels() - 
                                      exclusionRegion.GetNumberOfPixels();

  ev.GoToBegin();
  ei.GoToBegin();
  while( !ev.IsAtEnd() )
    {
    if( ei.Get() != ei.GetIndex() )
      {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It should be at " << ei.GetIndex();
      std::cout << " but it is at   " << ei.Get() << std::endl;
      return EXIT_FAILURE;
      }

    if( ev.Get() != normalRegionValue )
      {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << ev.GetIndex() << std::endl;
      return EXIT_FAILURE;
      }
    ++numberOfPixelsVisited; 
    ++ei;
    ++ev;
    }

  if( numberOfPixelsVisited != pixelsToVisit )
    {
    std::cout << "Error in exclusion iterator " << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " Ok ! " << std::endl;


  std::cout << "Testing the iterator backwars... ";

  numberOfPixelsVisited = 0;
  ev.GoToEnd();
  ei.GoToEnd();
  while( !ev.IsAtBegin() )
    {
    if( ei.Get() != ei.GetIndex() )
      {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It should be at " << ei.GetIndex();
      std::cout << " but it is at   " << ei.Get() << std::endl;
      return EXIT_FAILURE;
      }

    if( ev.Get() != normalRegionValue )
      {
      std::cout << "Error in exclusion iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << ev.GetIndex() << std::endl;
      return EXIT_FAILURE;
      }
    ++numberOfPixelsVisited; 
    --ei;
    --ev;
    }

  if( numberOfPixelsVisited != pixelsToVisit )
    {
    std::cout << "Error in exclusion iterator" << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " Ok ! " << std::endl;


  std::cout << "Starting walk with the exclusion Const iterator... ";
  typedef itk::ImageRegionExclusionConstIteratorWithIndex< IndexImageType > ExclusionIndexConstIteratorType;
  typedef itk::ImageRegionExclusionConstIteratorWithIndex< ValueImageType > ExclusionValueConstIteratorType;

  ExclusionValueConstIteratorType cev( myValueImage, region );
  ExclusionIndexConstIteratorType cei( myIndexImage, region );

  cev.SetExclusionRegion( exclusionRegion );
  cei.SetExclusionRegion( exclusionRegion );
  
  numberOfPixelsVisited = 0;

  cev.GoToBegin();
  cei.GoToBegin();
  while( !cev.IsAtEnd() )
    {
    if( cei.Get() != cei.GetIndex() )
      {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It should be at " << cei.GetIndex();
      std::cout << " but it is at   " << cei.Get() << std::endl;
      return EXIT_FAILURE;
      }

    if( cev.Get() != normalRegionValue )
      {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << ev.GetIndex() << std::endl;
      return EXIT_FAILURE;
      }
    ++numberOfPixelsVisited; 
    ++cei;
    ++cev;
    }

  if( numberOfPixelsVisited != pixelsToVisit )
    {
    std::cout << "Error in exclusion const iterator " << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " Ok ! " << std::endl;


  std::cout << "Testing the Const iterator backwars... ";

  numberOfPixelsVisited = 0;
  cev.GoToEnd();
  cei.GoToEnd();
  while( !cev.IsAtBegin() )
    {
    if( cei.Get() != cei.GetIndex() )
      {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It should be at " << cei.GetIndex();
      std::cout << " but it is at   " << cei.Get() << std::endl;
      return EXIT_FAILURE;
      }

    if( cev.Get() != normalRegionValue )
      {
      std::cout << "Error in exclusion const iterator " << std::endl;
      std::cout << "It is stepping into the exclusion region " << std::endl;
      std::cout << "Entry point = " << cev.GetIndex() << std::endl;
      return EXIT_FAILURE;
      }
    ++numberOfPixelsVisited; 
    --cei;
    --cev;
    }

  if( numberOfPixelsVisited != pixelsToVisit )
    {
    std::cout << "Error in exclusion const iterator " << std::endl;
    std::cout << "It is not visiting all the pixels it should" << std::endl;
    std::cout << numberOfPixelsVisited << " pixels were visited instead of ";
    std::cout << pixelsToVisit << std::endl;
    return EXIT_FAILURE;
    }
  std::cout << " Ok ! " << std::endl;

  std::cout << "Test PASSED ! " << std::endl;

  return EXIT_SUCCESS;

  }



