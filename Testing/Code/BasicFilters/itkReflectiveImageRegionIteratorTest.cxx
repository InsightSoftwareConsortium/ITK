/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkReflectiveImageRegionIteratorTest.cxx
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
#include "itkNumericTraits.h"
#include "itkReflectiveImageRegionIterator.h"
#include "itkReflectiveImageRegionConstIterator.h"
#include "itkImageRegionIteratorWithIndex.h"


int itkReflectiveImageRegionIteratorTest(int, char**)
{
  std::cout << "Creating an image" << std::endl;
  const unsigned int Dimension = 4;
  typedef itk::Index<Dimension>             PixelType;
  typedef itk::Image<PixelType,Dimension>   ImageType;
  typedef itk::Image<int,Dimension>         ImageVisitsType;

  typedef itk::ImageRegionIteratorWithIndex<ImageType>       IteratorType;
  typedef itk::ImageRegionIteratorWithIndex<ImageVisitsType> IteratorVisitsType;

  ImageType::Pointer myImage = ImageType::New();
  
  ImageType::SizeType size = {{4,4,4,4}};

  ImageType::IndexType start;
  start.Fill(0);

  ImageType::RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  myImage->SetLargestPossibleRegion( region );
  myImage->SetBufferedRegion( region );
  myImage->SetRequestedRegion( region );
  myImage->Allocate();

  ImageVisitsType::Pointer visitImage = ImageVisitsType::New();

  visitImage->SetLargestPossibleRegion( region );
  visitImage->SetRequestedRegion( region );
  visitImage->SetBufferedRegion( region );
  visitImage->Allocate();

  IteratorType        nit( myImage,    region );
  IteratorVisitsType  vit( visitImage, region );

  // Store information on the Image
  std::cout << "Storing data in the image ... " << std::endl;
  nit.GoToBegin();
  vit.GoToBegin();
  while( !nit.IsAtEnd() )
    {
    // set the pixel index as value
    nit.Set( nit.GetIndex() );      
    // Set the number of visits to zero
    vit.Set( itk::NumericTraits< ImageVisitsType::PixelType >::Zero );
    ++nit;
    ++vit;
    } 
  


  typedef itk::ReflectiveImageRegionConstIterator< ImageType > 
                                                  ReflectiveIteratorType;
  ReflectiveIteratorType rit( myImage, region );

  typedef itk::ReflectiveImageRegionIterator< ImageVisitsType > 
                                                  ReflectiveVisitsIteratorType;

  ReflectiveVisitsIteratorType rvt( visitImage, region );

  // Verification 
  std::cout << "Verifying the reflective iterator... " << std::endl;;

  rit.GoToBegin();
  rvt.GoToBegin();
  while( !rit.IsAtEnd() )
    {
    PixelType value = rit.Get();
    ImageType::IndexType index = rit.GetIndex();
    rvt.Set( rvt.Get() + 1 );
    if( value != index ) 
      {
      std::cerr << "Error :  at Index " << index << std::endl;
      std::cerr << "It is pointing to " << value << std::endl;
      }
    ++rit;
    ++rvt;
    }




  // Each element should be visited 2 ^ # of dimensions
  // each left shift = multiply by 2
  int visits = ( 1 << (ImageType::ImageDimension)); 
  int failed = 0;

  // Verify the number of visits
  vit.GoToBegin();
  while( !vit.IsAtEnd() )
    {
    if (vit.Get() != visits)
      {
      std::cout << vit.GetIndex() << " should not = " << vit.Get() << std::endl;
      failed++;
      }
    ++vit;
    }

  std::cout << std::endl;

  if ( failed )
    {
    std::cout << "      FAILED !" << std::endl << std::endl;
    return EXIT_FAILURE;
    }

  
  std::cout << "      PASSED !" << std::endl << std::endl;
  return EXIT_SUCCESS;

}



