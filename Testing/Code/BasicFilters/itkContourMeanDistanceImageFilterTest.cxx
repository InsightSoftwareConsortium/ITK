/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContourMeanDistanceImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#if defined(_MSC_VER)
#pragma warning ( disable : 4786 )
#endif

#include "itkContourMeanDistanceImageFilter.h"
#include "itkImageRegionIterator.h"
#include "itkFilterWatcher.h"

int itkContourMeanDistanceImageFilterTest(int, char* [] )
{

  typedef unsigned int Pixel1Type;
  typedef float Pixel2Type;
  enum { ImageDimension = 3 };

  typedef itk::Image<Pixel1Type,ImageDimension> Image1Type;
  typedef itk::Image<Pixel2Type,ImageDimension> Image2Type;

  Image1Type::Pointer image1 = Image1Type::New();
  Image2Type::Pointer image2 = Image2Type::New();

  Image1Type::SizeType size;
  size.Fill( 50 );
  
  image1->SetRegions( size );
  image2->SetRegions( size );

  image1->Allocate();
  image2->Allocate();

  image1->FillBuffer( itk::NumericTraits<Pixel1Type>::Zero );
  image2->FillBuffer( itk::NumericTraits<Pixel2Type>::Zero );

  typedef Image1Type::RegionType RegionType;
  RegionType region1;
  RegionType region2;

  typedef Image1Type::IndexType IndexType;
  IndexType index;

  size.Fill( 20 );
  index.Fill( 10 );
  region1.SetSize( size );
  region1.SetIndex( index );

  size.Fill( 15 );
  index.Fill( 20 );
  region2.SetSize( size );
  region2.SetIndex( index );

  itk::ImageRegionIterator<Image1Type> it1( image1, region1 );
  Pixel1Type count = itk::NumericTraits<Pixel1Type>::Zero;
  while ( !it1.IsAtEnd() )
    {
    it1.Set( ++count );
    ++it1;
    }

  itk::ImageRegionIterator<Image2Type> it2( image2, region2 );
  while ( !it2.IsAtEnd() )
    {
    it2.Set( 7.2 );
    ++it2;
    }


  // compute the directed Mean distance h(image1,image2)
  {
  typedef itk::ContourMeanDistanceImageFilter<Image1Type,Image2Type> FilterType;
  FilterType::Pointer filter = FilterType::New();
  FilterWatcher watcher(filter, "filter");

  filter->SetInput1( image1 );
  filter->SetInput2( image2 );
  filter->Update();
  filter->Print( std::cout );


  // check results
  
  FilterType::RealType trueDistance = 8.07158;
    // vcl_sqrt( static_cast<double>(ImageDimension) );
  FilterType::RealType distance = filter->GetMeanDistance();

  std::cout << " True     distance: " << trueDistance << std::endl;
  std::cout << " Computed distance: " << distance << std::endl;

  if ( vnl_math_abs( trueDistance - distance ) > 0.1 )
    {
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }
  }

  // compute the directed Mean distance h(image2,image1)
  {
  typedef itk::ContourMeanDistanceImageFilter<Image2Type,Image1Type> FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput1( image2 );
  filter->SetInput2( image1 );
  filter->Update();


  // check results
  FilterType::RealType trueDistance = 8.07158;
  FilterType::RealType distance = filter->GetMeanDistance();

  std::cout << " True     distance: " << trueDistance << std::endl;
  std::cout << " Computed distance: " << distance << std::endl;

  if ( vnl_math_abs( trueDistance - distance ) > 0.1 )
    {
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }
  }

  std::cout << "Test passed. " << std::endl;
  return EXIT_SUCCESS;
  
}
