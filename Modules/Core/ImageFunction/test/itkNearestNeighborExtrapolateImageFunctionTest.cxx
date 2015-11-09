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

#include "itkMath.h"
#include "itkNearestNeighborExtrapolateImageFunction.h"
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
  const   unsigned int VectorDimension = 4;
  typedef itk::Vector< PixelType, VectorDimension >     VectorPixelType;
  typedef itk::Image<PixelType,ImageDimension>          ImageType;
  typedef itk::Image< VectorPixelType, ImageDimension > VectorImageType;

  ImageType::SizeType imageSize;
  imageSize[0] = 5;
  imageSize[1] = 7;
  ImageType::RegionType imageRegion( imageSize );

  ImageType::Pointer image = ImageType::New();
  image->SetRegions( imageRegion );
  image->Allocate();

  VectorImageType::Pointer vectorimage = VectorImageType::New();
  vectorimage->SetRegions( imageRegion );
  vectorimage->Allocate();

  typedef itk::ImageRegionIterator<ImageType> Iterator;
  Iterator iter( image, imageRegion );
  iter.GoToBegin();
  unsigned char counter = 0;

  while( !iter.IsAtEnd() )
    {
    iter.Set( counter++ );
    ++iter;
    }

  typedef itk::ImageRegionIterator<VectorImageType> VectorIterator;
  VectorIterator vectoriter( vectorimage, imageRegion );
  vectoriter.GoToBegin();
  counter = 0;

  while( !vectoriter.IsAtEnd() )
    {
    VectorPixelType & vectorpixel = vectoriter.Value();
    vectorpixel.Fill( counter++ );
    ++vectoriter;
    }

  // set up the extrapolator
  typedef itk::NearestNeighborExtrapolateImageFunction<ImageType,CoordRep> FunctionType;
  FunctionType::Pointer function = FunctionType::New();

  typedef itk::NearestNeighborExtrapolateImageFunction<VectorImageType,CoordRep> VectorFunctionType;
  VectorFunctionType::Pointer vectorfunction = VectorFunctionType::New();

  function->SetInputImage( image );

  vectorfunction->SetInputImage( vectorimage );

  FunctionType::IndexType index;
  FunctionType::PointType point;
  FunctionType::OutputType value;
  FunctionType::OutputType trueValue;

  VectorFunctionType::OutputType vectorvalue;
  VectorFunctionType::OutputType trueVectorValue;

  // evaluate at point inside the image
  point[0] = 2.25;
  point[1] = 3.25;
  value = function->Evaluate( point );

  vectorvalue = vectorfunction->Evaluate( point );

  trueValue = itk::Math::Round<int>( point[0] ) +
    ( itk::Math::Round<int>( point[1] ) )  * static_cast<double>( imageSize[0] );
  trueVectorValue.Fill( trueValue );

  std::cout << "Point: " << point << " Value: " << value
            << " Vector Value: " << vectorvalue << std::endl;
  if ( itk::Math::NotAlmostEquals(value, trueValue) )
    {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  if ( vectorvalue != trueVectorValue )
    {
    std::cout << "Vector Value not the same as trueVectorValue: " << trueVectorValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  // evaluate at point outside the image
  point[0] = 2.25;
  point[1] = 8.0;
  value = function->Evaluate( point );

  trueValue = itk::Math::Round<int>( point[0] ) +
    ( 6.0 )  * static_cast<double>( imageSize[0] );

  std::cout << "Point: " << point << " Value: " << value << std::endl;
  if ( itk::Math::NotAlmostEquals(value, trueValue) )
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
  if ( itk::Math::NotAlmostEquals(value, trueValue) )
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
  if ( itk::Math::NotAlmostEquals(value, trueValue) )
    {
    std::cout << "Value not the same as trueValue: " << trueValue << std::endl;
    std::cout << "Test failed. " << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
