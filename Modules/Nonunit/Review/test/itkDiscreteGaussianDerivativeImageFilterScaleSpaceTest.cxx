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

#include "itkDiscreteGaussianDerivativeImageFilter.h"

namespace
{

bool NormalizeSineWave( double frequencyPerImage, unsigned int order, double pixelSpacing = 1.0 )
{
  // For an image f(x) = sin ( w*x ), where w is a measure of
  // frequency, this method verifies that the normalized scale-scale
  // is with in reasonable tolerance of the theoretical value.

  const unsigned int ImageDimension = 1;
  const unsigned int imageSize = 1024;

  const double tolerance1 = std::pow( .001, 1.0 / order ); // still larger than it should be!

  double frequency = frequencyPerImage * 2.0 * itk::Math::pi / ( imageSize * pixelSpacing );

  // The theoretical maximal value should occur at this sigma
  double sigmaMax = std::sqrt( double( order ) ) / frequency;

  // The theoreical maximal value of the derivative, obtained at sigmaMax
  double expectedMax = std::pow( double(order), order *0.5 ) * std::exp( -0.5 * order );

  typedef itk::Image< double, ImageDimension > ImageType;
  ImageType::Pointer image = ImageType::New();

  ImageType::SizeType size;
  size.Fill( imageSize );

  image->SetRegions( ImageType::RegionType( size ) );
  image->Allocate();

  ImageType::SpacingType spacing;
  spacing.Fill( pixelSpacing );

  image->SetSpacing( spacing );

  itk::ImageRegionIterator< ImageType > iter( image, image->GetBufferedRegion() );

  // Create a sine wave image
  while( !iter.IsAtEnd() )
    {
    ImageType::PointType p;
    image->TransformIndexToPhysicalPoint( iter.GetIndex(), p );
    const double x = p[0];
    double value = std::sin( x * frequency );

    iter.Set( value );
    ++iter;
    }

  typedef itk::DiscreteGaussianDerivativeImageFilter< ImageType, ImageType >
    GaussianFilterType;
  GaussianFilterType::Pointer filter = GaussianFilterType::New();

  filter->SetInput( image );
  filter->SetVariance( itk::Math::sqr( sigmaMax ) );
  filter->SetOrder( order );
  filter->SetUseImageSpacing( true );
  filter->SetMaximumError( .0005 );
  filter->SetMaximumKernelWidth( 500 );
  filter->SetNormalizeAcrossScale( true );

  ImageType::Pointer outputImage = filter->GetOutput();
  outputImage->Update();

  // Maximal value of the first derivative
  double maxLx = itk::NumericTraits< double >::NonpositiveMin();

  itk::ImageRegionConstIterator< ImageType > oiter( outputImage, outputImage->GetBufferedRegion() );

  while ( !oiter.IsAtEnd() )
    {
    maxLx = std::max( maxLx, oiter.Get() );
    ++oiter;
    }

  // Check if the maximal is obtained with a little bit smaller Gaussian
  filter->SetVariance( itk::Math::sqr( sigmaMax * 0.95 ) );
  outputImage->Update();
  oiter.GoToBegin();

  while ( !oiter.IsAtEnd() )
    {
    if ( maxLx < oiter.Get() &&
      !itk::Math::FloatAlmostEqual( maxLx, oiter.Get(), 10, tolerance1 ) )
      {
      std::cout.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance1 ) ) ) );
      std::cout << "Error at period: " << 1.0 / frequency << std::endl;
      std::cout << "Expected maximal value: " << maxLx << ", differs from: "
        << oiter.Get() << " by more than " << tolerance1 << std::endl;
      return false;
      }
    ++oiter;
    }

  // Check if the maximumal value is obtained with a little bit bigger Gaussian
  filter->SetVariance( itk::Math::sqr( sigmaMax * 1.05 ) );
  outputImage->Update();
  oiter.GoToBegin();

  while ( !oiter.IsAtEnd() )
    {
    if ( maxLx < oiter.Get() &&
      !itk::Math::FloatAlmostEqual( maxLx, oiter.Get(), 10, tolerance1 ) )
      {
      std::cout.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance1 ) ) ) );
      std::cout << "Error at period: " << 1.0 / frequency << std::endl;
      std::cout << "Expected maximal value: " << maxLx << ", differs from: "
        << oiter.Get() << " by more than " << tolerance1 << std::endl;
      return false;
      }
    ++oiter;
    }

  const double tolerance2 = 0.01;
  if ( !itk::Math::FloatAlmostEqual( maxLx, expectedMax, 10, tolerance2 ) )
    {
    std::cout << "Error at frequency: " << frequencyPerImage << std::endl;
    std::cout << "Expected maximal value: " << expectedMax << ", differs from: "
      << maxLx << " by more than " << tolerance2 << std::endl;
    return false;
    }

  return true;
}

}

int itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest( int, char* [] )
{
  bool pass = true;

  // Testing first order Gaussian
  pass  &= NormalizeSineWave( 16, 1 );
  pass  &= NormalizeSineWave( 32, 1 );
  pass  &= NormalizeSineWave( 64, 1 );

  // Testing second order Gaussian
  pass  &= NormalizeSineWave( 16, 2 );
  pass  &= NormalizeSineWave( 32, 2 );
  pass  &= NormalizeSineWave( 64, 2 );

  // Testing spacing invariance
  pass  &= NormalizeSineWave( 16, 2, 0.01 );
  pass  &= NormalizeSineWave( 16, 2, 100 );

  if ( !pass )
    {
    std::cout << "Test failed!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
