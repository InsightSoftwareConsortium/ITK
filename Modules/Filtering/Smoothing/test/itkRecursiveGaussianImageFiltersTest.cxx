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
// Disable warning for long symbol names in this file only

#include "itkImage.h"
#include "itkRecursiveGaussianImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionConstIterator.h"
#include "itkTestingMacros.h"

#include <algorithm>
#include <numeric>


int itkRecursiveGaussianImageFiltersTest(int, char* [] )
{

  {  // 3D test

  // Define the dimension of the images
  const unsigned int myDimension = 3;

  // Declare the types of the images
  typedef itk::Image<float, myDimension>           myImageType;

  // Declare the type of the index to access images
  typedef itk::Index<myDimension>             myIndexType;

  // Declare the type of the size
  typedef itk::Size<myDimension>              mySizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion<myDimension>        myRegionType;

  // Create the image
  myImageType::Pointer inputImage  = myImageType::New();


  // Define their size, and start index
  mySizeType size;
  size[0] = 100;
  size[1] = 100;
  size[2] = 100;

  myIndexType start;
  start.Fill(0);

  myRegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex<myImageType>  myIteratorType;


  // Create one iterator for the Input Image A (this is a light object)
  myIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the content of Image A
  std::cout << "Input Image initialization " << std::endl;
  while( !it.IsAtEnd() )
  {
    it.Set( 0.0 );
    ++it;
  }

  size[0] = 60;
  size[1] = 60;
  size[2] = 60;

  start[0] = 20;
  start[1] = 20;
  start[2] = 20;

  // Create one iterator for an internal region
  region.SetSize( size );
  region.SetIndex( start );
  myIteratorType itb( inputImage, region );

  // Initialize the content the internal region
  while( !itb.IsAtEnd() )
  {
    itb.Set( 100.0 );
    ++itb;
  }

  // Declare the type for the  Gaussian  filter
  typedef itk::RecursiveGaussianImageFilter<
                                              myImageType,
                                              myImageType
                                                        >  myGaussianFilterType;


  // Create a  Filter
  myGaussianFilterType::Pointer filter = myGaussianFilterType::New();


  // Connect the input images
  filter->SetInput( inputImage );
  filter->SetDirection( 2 );  // apply along Z
  filter->SetOrder( myGaussianFilterType::ZeroOrder );


  // Execute the filter
  std::cout << "Executing Smoothing filter...";
  filter->Update();
  std::cout << " Done !" << std::endl;


  // Create a  Filter
  myGaussianFilterType::Pointer filter1 = myGaussianFilterType::New();


  // Connect the input images
  filter1->SetInput( inputImage );
  filter1->SetDirection( 2 );  // apply along Z
  filter1->SetOrder( myGaussianFilterType::FirstOrder );


  // Execute the filter1
  std::cout << "Executing First Derivative filter...";
  filter1->Update();
  std::cout << " Done !" << std::endl;

  // Create a  Filter
  myGaussianFilterType::Pointer filter2 = myGaussianFilterType::New();

  // Connect the input images
  filter2->SetInput( inputImage );
  filter2->SetDirection( 2 );  // apply along Z
  filter2->SetOrder( myGaussianFilterType::SecondOrder );

  // Execute the filter2
  std::cout << "Executing Second Derivative filter...";
  filter2->Update();
  std::cout << " Done !" << std::endl;
  }

  { // Test normalizations factors using a 1D image
  std::cout << "Test normalizations factors using a 1-D image" << std::endl;

  typedef float                         PixelType;
  typedef itk::Image< PixelType, 1 >    ImageType;

  typedef ImageType::SizeType           SizeType;
  typedef ImageType::IndexType          IndexType;
  typedef ImageType::RegionType         RegionType;
  typedef ImageType::SpacingType        SpacingType;

  typedef itk::NumericTraits< PixelType >::RealType    PixelRealType;

  SizeType size;
  size[0] = 21;

  IndexType start;
  start[0] = 0;

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  SpacingType spacing;
  spacing[0] = 1.0;

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetRegions( region );
  inputImage->Allocate();
  inputImage->SetSpacing( spacing );
  inputImage->FillBuffer( itk::NumericTraits< PixelType >::ZeroValue() );

  IndexType index;
  index[0] = ( size[0] - 1 ) / 2;  // the middle pixel

  inputImage->SetPixel( index, static_cast< PixelType >( 1000.0 ) );

  typedef itk::RecursiveGaussianImageFilter<
                            ImageType, ImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( inputImage );

  std::cout << "Testing normalization across scales...  ";
  { // begin of test for normalization across scales

    filter->SetNormalizeAcrossScale( true );

    const double sigmaA = 2.0;
    filter->SetSigma( sigmaA );
    filter->Update();

    const PixelType valueA = filter->GetOutput()->GetPixel( index );


    filter->SetNormalizeAcrossScale( false );
    const double sigmaB = 2.0;
    filter->SetSigma( sigmaB );

    filter->Update();

    const PixelType valueB = filter->GetOutput()->GetPixel( index );

    // note: for scale space normalization, no scaling should occur
    // The additional scale-space testing is performed in a separate
    // test.
    if( std::fabs( valueB - valueA  ) > 1e-4 )
      {
      std::cout << "FAILED !" << std::endl;
      std::cerr << "Error, Normalization across scales is failing" << std::endl;
      std::cerr << "Central pixel at sigma = " << sigmaA << " = " << valueA << std::endl;
      std::cerr << "Central pixel at sigma = " << sigmaB << " = " << valueB << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "PASSED !" << std::endl;
      }

    } // end of test for normalization across scales

  std::cout << "Testing normalization...  ";
  { // begin of test for normalization

    filter->SetNormalizeAcrossScale( true );

    // size of image is 21, so a sigma of 2 gives up 5 std-devs and
    // an expected error of >1e-5 due to truncation
    const double sigmaA = 2.0;
    filter->SetSigma( sigmaA );
    filter->Update();

    // the input is an impulse with a value of 1000
    // the resulting convolution should aproximatly sum to the same

    typedef itk::ImageRegionConstIterator< ImageType > IteratorType;
    IteratorType  it( filter->GetOutput(), filter->GetOutput()->GetBufferedRegion() );

    std::vector<double> values;

    while ( !it.IsAtEnd() )
      {
      values.push_back( it.Get() );
      ++it;
      }

    // sort from smallest to largest for best numerical precision
    std::sort( values.begin(), values.end() );

    double total = std::accumulate( values.begin(), values.end(), 0.0 );

    // 1000.0 is the value of the impulse
    // compute absolute normalized error
    double error = std::fabs( total - 1000.0 )/1000.0;
    if ( error > 1e-3)
      {
      std::cout << "FAILED !" << std::endl;
      std::cerr << "Error, Normalization  is failing" << std::endl;
      std::cerr << "Value of impulse is 1000.0" << std::endl;
      std::cerr << "Total value after convolution is " << total << std::endl;
      std::cout << "error: " << error << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "PASSED !" << std::endl;
      }

    } // end of test for normalization

    std::cout << "Testing derivatives normalization " << std::endl;

    { // begin of test for normalization among derivatives
    filter->SetNormalizeAcrossScale( false );

    // Since one side of the Gaussian is monotonic we can
    // use the middle-value theorem: The value of the derivative at
    // index[0] - 2 must be bounded by the estimation of the derivative
    // at index[0] -1 and index[0] -3. In the following we compute an
    // estimation of derivatives by partial differences at this two
    // positions and use them as bounds for the value of the first order
    // derivative returned by the filter.

    const double sigmaC = 3.0;
    filter->SetSigma( sigmaC );

    filter->SetZeroOrder();
    filter->Update();

    index[0] = ( size[0] - 1 ) / 2;  // the middle pixel
    const PixelRealType valueA = filter->GetOutput()->GetPixel( index );

    index[0] -= 2;
    const PixelRealType valueB = filter->GetOutput()->GetPixel( index );

    index[0] -= 2;
    const PixelRealType valueC = filter->GetOutput()->GetPixel( index );

    const PixelRealType derivativeLowerBound = ( valueA - valueB ) / 2.0;
    const PixelRealType derivativeUpperBound = ( valueB - valueC ) / 2.0;

    // Now let's get the first derivative value computed by the filter
    filter->SetFirstOrder();
    filter->Update();


    index[0] = ( size[0] - 1 ) / 2;  // the middle pixel
    index[0] -= 2;

    const PixelRealType derivativeValue = filter->GetOutput()->GetPixel( index );

    std::cout << "   first derivative normalization...  ";
    if( ( derivativeLowerBound > derivativeValue ) ||
        ( derivativeUpperBound < derivativeValue )  )
      {
      std::cout << "FAILED !" << std::endl;
      std::cerr << "The value of the first derivative at index " << index[0] << std::endl;
      std::cerr << "is = " << derivativeValue << std::endl;
      std::cerr << "which is outside the bounds = [ " << derivativeLowerBound;
      std::cerr << " : " << derivativeUpperBound << " ] " << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "PASSED !" << std::endl;
      }


    // Now do the similar testing between First Derivative and Second
    // derivative.
    filter->SetFirstOrder();
    filter->Update();

    index[0] = ( size[0] - 1 ) / 2;  // the middle pixel
    const PixelRealType value1A = filter->GetOutput()->GetPixel( index );

    index[0] -= 2;
    const PixelRealType value1B = filter->GetOutput()->GetPixel( index );

    index[0] -= 2;
    const PixelRealType value1C = filter->GetOutput()->GetPixel( index );

    // NOTE that the second derivative in this region is monotonic but decreasing.
    const PixelRealType secondDerivativeLowerBound = ( value1A - value1B ) / 2.0;
    const PixelRealType secondDerivativeUpperBound = ( value1B - value1C ) / 2.0;

    // Now let's get the second derivative value computed by the filter
    filter->SetSecondOrder();
    filter->Update();


    index[0] = (( size[0] - 1 ) / 2) - 2; // where to sample the second derivative

    const PixelRealType secondDerivativeValue = filter->GetOutput()->GetPixel( index );

    std::cout << "   second derivative normalization...  ";
    if( ( secondDerivativeLowerBound > secondDerivativeValue ) ||
        ( secondDerivativeUpperBound < secondDerivativeValue )  )
      {
      std::cout << "FAILED !" << std::endl;
      std::cerr << "The value of the second derivative at index " << index[0] << std::endl;
      std::cerr << "is = " << secondDerivativeValue << std::endl;
      std::cerr << "which is outside the bounds = [ " << secondDerivativeLowerBound;
      std::cerr << " : " << secondDerivativeUpperBound << " ] " << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "PASSED !" << std::endl;
      }


    } // end of test for normalization among derivatives

  // Print out all the values for the zero, first and second order
  filter->SetNormalizeAcrossScale( false );
  filter->SetSigma( 2.0 );

  ImageType::ConstPointer outputImage = filter->GetOutput();
  typedef itk::ImageRegionConstIterator< ImageType > IteratorType;
  IteratorType  it( outputImage, outputImage->GetBufferedRegion() );

  std::cout << std::endl << std::endl;
  std::cout << "Smoothed image " << std::endl;
  filter->SetZeroOrder();
  filter->Update();
  it.GoToBegin();
  while( ! it.IsAtEnd() )
    {
    std::cout << it.Get() << std::endl;
    ++it;
    }

  // Now compute the first derivative
  std::cout << std::endl << std::endl;
  std::cout << "First Derivative " << std::endl;
  filter->SetFirstOrder();
  filter->Update();
  it.GoToBegin();
  while( ! it.IsAtEnd() )
    {
    std::cout << it.Get() << std::endl;
    ++it;
    }


  // Now compute the first derivative
  std::cout << std::endl << std::endl;
  std::cout << "Second Derivative " << std::endl;
  filter->SetSecondOrder();
  filter->Update();
  it.GoToBegin();
  while( ! it.IsAtEnd() )
    {
    std::cout << it.Get() << std::endl;
    ++it;
    }

  filter->SetSigma( 0.0 );
  TRY_EXPECT_EXCEPTION( filter->Update() )
  }

  {
  std::cout << "Test InPlace filtering using a 1-D image" << std::endl;

  typedef float                         PixelType;
  typedef itk::Image< PixelType, 1 >    ImageType;

  typedef ImageType::SizeType           SizeType;
  typedef ImageType::IndexType          IndexType;
  typedef ImageType::RegionType         RegionType;
  typedef ImageType::SpacingType        SpacingType;

  SizeType size;
  size[0] = 21;

  IndexType start;
  start[0] = 0;

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  SpacingType spacing;
  spacing[0] = 1.0;

  ImageType::Pointer inputImage = ImageType::New();
  inputImage->SetRegions( region );
  inputImage->Allocate();
  inputImage->SetSpacing( spacing );
  inputImage->FillBuffer( itk::NumericTraits< PixelType >::ZeroValue() );

  IndexType index;
  index[0] = ( size[0] - 1 ) / 2;  // the middle pixel

  inputImage->SetPixel( index, static_cast< PixelType >( 1.0 ) );

  typedef itk::RecursiveGaussianImageFilter< ImageType, ImageType > FilterType;
  FilterType::Pointer filter = FilterType::New();
  filter->SetInput( inputImage );
  filter->SetSigma( 1 );

  // coverage for set/get methods
  filter->SetOrder( FilterType::ZeroOrder );
  if ( FilterType::ZeroOrder  != filter->GetOrder() )
    {
    std::cerr << "SetOrder/GetOrder failure!" << std::endl;
    return EXIT_FAILURE;
    }


  // Check behavior of InPlace
  filter->InPlaceOn();
  filter->Update();

  ImageType::ConstPointer outputImage = filter->GetOutput();
  typedef itk::ImageRegionConstIterator< ImageType > IteratorType;
  IteratorType  it( outputImage, outputImage->GetBufferedRegion() );

  it.GoToBegin();
  while( ! it.IsAtEnd() )
    {
    std::cout << it.Get() << std::endl;
    ++it;
    }

  std::cout << "input buffer region: " << inputImage->GetBufferedRegion() << std::endl;
  std::cout << "output buffer region: " << outputImage->GetBufferedRegion() << std::endl;

  if ( inputImage->GetBufferedRegion().GetNumberOfPixels() != 0 )
    {
    std::cerr << "Failure for filter to run in-place!" << std::endl;
    return EXIT_FAILURE;
    }

  filter->SetSigma( 0.0 );
  TRY_EXPECT_EXCEPTION( filter->Update() )

  }


  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}
