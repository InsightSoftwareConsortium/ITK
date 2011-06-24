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
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#endif

#include "itkImage.h"
#include "itkDiscreteGaussianDerivativeImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkImageRegionIterator.h"

namespace
{

bool NormalizeSineWave( double frequencyPerImage, unsigned int order, double pixelSpacing = 1.0 )
{
  // for an image f(x) = sin ( w*x ), where w is a measure of
  // frequency, this method verifies that the normalized scale-scale
  // is with in reasonable tolerance of the theoretical value.

  const unsigned int ImageDimension = 1;
  const unsigned int imageSize = 1024;
  const double       tol = vcl_pow( .001, 1.0 / order ); // this is
                                                         // still
                                                         // larger
                                                         // then it
                                                         // should be!

  double frequency = frequencyPerImage * 2.0 * vnl_math::pi / ( imageSize * pixelSpacing );

  // The theoretical maximal value should occour at this sigma
  double sigma_max = vcl_sqrt( double( order ) ) / frequency;

  // the theoreical maximal value of the derivative, obtained at sigma_max
  double expected_max = vcl_pow( double(order), order *0.5 ) * vcl_exp( -0.5 * order );

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

  // create sine wave image
  while( !iter.IsAtEnd() )
    {
    ImageType::PointType p;
    image->TransformIndexToPhysicalPoint( iter.GetIndex(), p );
    const double x = p[0];
    double       value = vcl_sin( x * frequency );

    iter.Set( value );
    ++iter;
    }

  std::cout << "variance: " << vnl_math_sqr( sigma_max ) << std::endl;

  typedef itk::DiscreteGaussianDerivativeImageFilter<ImageType, ImageType> GaussianFilterType;
  GaussianFilterType::Pointer filter = GaussianFilterType::New();
  filter->SetInput( image );
  filter->SetVariance( vnl_math_sqr( sigma_max ) );
  filter->SetOrder( order );
  filter->SetUseImageSpacing( true );
  filter->SetMaximumError( .0005 );
  filter->SetMaximumKernelWidth( 500 );
  filter->SetNormalizeAcrossScale( true );

  ImageType::Pointer outputImage = filter->GetOutput();
  outputImage->Update();

  // maximal value of the first derivative
  double maxLx = itk::NumericTraits<double>::NonpositiveMin();

  itk::ImageRegionConstIterator< ImageType > oiter( outputImage, outputImage->GetBufferedRegion() );

  while ( !oiter.IsAtEnd() )
    {
    maxLx = std::max( maxLx, oiter.Get() );
    ++oiter;
    }

  // check if the maximal is obtained with a little bit smaller Gaussian
  filter->SetVariance( vnl_math_sqr( sigma_max*0.95 ) );
  outputImage->Update();
  oiter.GoToBegin();

  while ( !oiter.IsAtEnd() )
    {
    if ( maxLx < oiter.Get() &&
         vcl_abs( maxLx - oiter.Get() ) > tol )
      {
      std::cout << "FAIL: For period: " << 1.0/frequency
                << " maxLx: " << maxLx
                << " tolerance exceeded by: "
                << vcl_abs( maxLx - oiter.Get() ) << std::endl;
      return false;
      }
    ++oiter;
    }

  // check if the maximal is obtained with a little bit bigger Gaussian
  filter->SetVariance( vnl_math_sqr( sigma_max*1.05 ) );
  outputImage->Update();
  oiter.GoToBegin();

  while ( !oiter.IsAtEnd() )
    {
    if ( maxLx < oiter.Get() &&
         vcl_abs( maxLx - oiter.Get() ) > tol )
      {
      std::cout << "FAIL:  For period: " << 1.0/frequency
                << " maxLx: " << maxLx
                << " tolerance exceeded by: "
                << vcl_abs( maxLx - oiter.Get() ) << std::endl;
      return false;
      }
    ++oiter;
    }

  std::cout << "f: " << frequencyPerImage << " max: " << maxLx
            << " expected max: " << expected_max << std::endl;

  if (  vcl_abs( maxLx - expected_max ) > .01 )
    {
    std::cout << "FAIL: tolerance of expected max exceeded!" << std::endl;
    }

  return true;

}

}

int itkDiscreteGaussianDerivativeImageFilterScaleSpaceTest(int, char* [] )
{
  bool pass = true;

  std::cout << " Testing First Order Gaussian" << std::endl;
  pass  &= NormalizeSineWave( 16, 1 );
  pass  &= NormalizeSineWave( 32, 1 );
  pass  &= NormalizeSineWave( 64, 1 );

  std::cout << " Testing Second Order Gaussian" << std::endl;
  pass  &= NormalizeSineWave( 16, 2 );
  pass  &= NormalizeSineWave( 32, 2 );
  pass  &= NormalizeSineWave( 64, 2 );

  std::cout << " Testing Spacing Invariance" << std::endl;
  pass  &= NormalizeSineWave( 16, 2, 0.01 );
  pass  &= NormalizeSineWave( 16, 2, 100 );

  if ( !pass )
    {
    std::cout << "Test Failed!" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
