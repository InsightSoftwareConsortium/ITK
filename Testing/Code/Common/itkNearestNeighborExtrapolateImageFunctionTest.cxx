/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNearestNeighborExtrapolateImageFunctionTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkNearestNeighborExtrapolateImageFunction.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"

/**
 * This module tests the functionality of the 
 * NearestNeighborExtrapolateImageFunction class.
 *
 */
int itkNearestNeighborExtrapolateImageFunctionTest( int, char *[])
{
  typedef double CoordRep;
  const unsigned int ImageDimension = 2;
  typedef unsigned char PixelType;
  typedef itk::Image<PixelType,ImageDimension> ImageType;

  ImageType::SizeType imageSize;
  imageSize[0] = 5;
  imageSize[1] = 7;
  ImageType::RegionType imageRegion( imageSize );

  ImageType::Pointer image = ImageType::New();
  image->SetRegions( imageRegion );
  image->Allocate();
  
  typedef itk::ImageRegionIterator<ImageType> Iterator;
  Iterator iter( image, imageRegion );
  iter.GoToBegin();
  unsigned char counter = 0;

  while( !iter.IsAtEnd() )
    {
    iter.Set( counter++ );
    ++iter;
    }

  // set up the extrapolator
  typedef itk::NearestNeighborExtrapolateImageFunction<ImageType,CoordRep> FunctionType;
  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  FunctionType::IndexType index;
  FunctionType::PointType point;
  FunctionType::OutputType value;
  FunctionType::OutputType trueValue;

  // evaluate at point inside the image
  point[0] = 2.25; 
  point[1] = 3.25;
  value = function->Evaluate( point );

  trueValue = vnl_math_rnd( point[0] ) +
    ( vnl_math_rnd( point[1] ) )  * static_cast<double>( imageSize[0] );

  std::cout << "Point: " << point << " Value: " << value << std::endl;
  if ( value != trueValue )
    {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE; 
    }

  // evaluate at point outside the image
  point[0] = 2.25; 
  point[1] = 8.0;
  value = function->Evaluate( point );

  trueValue = vnl_math_rnd( point[0] ) +
    ( 6.0 )  * static_cast<double>( imageSize[0] );

  std::cout << "Point: " << point << " Value: " << value << std::endl;
  if ( value != trueValue )
    {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE; 
    }

  // evaluate at index inside the image
  index[0] = 4;
  index[1] = 5;
  value = function->EvaluateAtIndex( index );

  trueValue = static_cast<double>( index[0] ) +
    static_cast<double>( index[1] )  * static_cast<double>( imageSize[0] );

  std::cout << "Index: " << index << " Value: " << value << std::endl;
  if ( value != trueValue )
    {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE; 
    }


  // evaluate at index outside the image
  index[0] = 8;
  index[1] = -1;
  value = function->EvaluateAtIndex( index );

  trueValue = static_cast<double>( 4 ) +
    static_cast<double>( 0 )  * static_cast<double>( imageSize[0] );

  std::cout << "Index: " << index << " Value: " << value << std::endl;
  if ( value != trueValue )
    {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE; 
    }

  return EXIT_SUCCESS;
}


