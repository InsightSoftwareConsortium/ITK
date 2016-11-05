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

#include "itkAtanImageFilter.h"
#include "itkAtanImageAdaptor.h"
#include "itkMath.h"
#include "itkSubtractImageFilter.h"
#include "itkTestingMacros.h"


int itkAtanImageFilterAndAdaptorTest( int, char* [] )
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the pixel types of the images
  typedef float                PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, ImageDimension > InputImageType;
  typedef itk::Image< PixelType, ImageDimension > OutputImageType;

  // Declare appropriate Iterator types for each image
  typedef itk::ImageRegionIteratorWithIndex<
                                  InputImageType >  InputIteratorType;
  typedef itk::ImageRegionIteratorWithIndex<
                                  OutputImageType > OutputIteratorType;

  // Declare the type of the index to access images
  typedef itk::Index< ImageDimension >         IndexType;

  // Declare the type of the size
  typedef itk::Size< ImageDimension >          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion< ImageDimension >   RegionType;

  // Create the input image
  InputImageType::Pointer inputImage = InputImageType::New();

  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Create one iterator for the Input Image (this is a light object)
  InputIteratorType it( inputImage, inputImage->GetBufferedRegion() );

  // Initialize the content of Image A
  const double value = itk::Math::pi / 6.0;
  it.GoToBegin();
  while( !it.IsAtEnd() )
    {
    it.Set( value );
    ++it;
    }

  // Declare the type for the Atan filter
  typedef itk::AtanImageFilter< InputImageType, OutputImageType > FilterType;

  // Create the Filter
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, AtanImageFilter,
    UnaryFunctorImageFilter );

  // Set the input image
  filter->SetInput( inputImage );

  filter->SetFunctor( filter->GetFunctor() );

  // Execute the filter
  filter->Update();

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Create an iterator for going through the image output
  OutputIteratorType ot( outputImage, outputImage->GetRequestedRegion() );

  // Check the content of the result image
  const OutputImageType::PixelType epsilon = 1e-6;
  ot.GoToBegin();
  it.GoToBegin();
  while( !ot.IsAtEnd() )
    {
    const InputImageType::PixelType  input  = it.Get();
    const OutputImageType::PixelType output = ot.Get();
    const OutputImageType::PixelType arctangent  = std::atan(input);
    if( !itk::Math::FloatAlmostEqual( arctangent, output, 10, epsilon ) )
      {
      std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
      std::cerr << "Error " << std::endl;
      std::cerr << " std::atan( " << input << ") = " << arctangent << std::endl;
      std::cerr << " differs from " << output;
      std::cerr << " by more than " << epsilon << std::endl;
      return EXIT_FAILURE;
      }
    ++ot;
    ++it;
    }


  //
  // Test the itk::AtanImageAdaptor
  //

  typedef itk::AtanImageAdaptor< InputImageType,
                          OutputImageType::PixelType > AdaptorType;

  AdaptorType::Pointer atanAdaptor = AdaptorType::New();

  EXERCISE_BASIC_OBJECT_METHODS( atanAdaptor, AtanImageAdaptor,
    ImageAdaptor );

  atanAdaptor->SetImage( inputImage );

  typedef itk::SubtractImageFilter<
                        OutputImageType,
                        AdaptorType,
                        OutputImageType > DiffFilterType;

  DiffFilterType::Pointer diffFilter = DiffFilterType::New();

  diffFilter->SetInput1( outputImage );
  diffFilter->SetInput2( atanAdaptor );

  diffFilter->Update();

  // Get the filter output
  OutputImageType::Pointer diffImage = diffFilter->GetOutput();

  // Check the content of the diff image
  //

  // Create an iterator for going through the image output
  OutputIteratorType dt( diffImage, diffImage->GetRequestedRegion() );

  dt.GoToBegin();
  while( !dt.IsAtEnd() )
    {
    const OutputImageType::PixelType diff = dt.Get();
    if( std::fabs( diff ) > epsilon )
      {
      std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
      std::cerr << "Error comparing results with Adaptors" << std::endl;
      std::cerr << " difference = " << diff << std::endl;
      std::cerr << " differs from 0 ";
      std::cerr << " by more than " << epsilon << std::endl;
      return EXIT_FAILURE;
      }
    ++dt;
    }

  return EXIT_SUCCESS;

}
