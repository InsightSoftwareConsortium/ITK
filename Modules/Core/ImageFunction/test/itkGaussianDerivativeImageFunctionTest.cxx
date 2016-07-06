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

#include "itkGaussianDerivativeImageFunction.h"
#include "itkTestingMacros.h"


template< typename TPixel >
int TestGaussianDerivativeImageFunction()
{
  const unsigned int Dimension = 2;
  typedef TPixel                             PixelType;
  typedef itk::Image< PixelType, Dimension > ImageType;

  // Create and allocate the image
  typename ImageType::Pointer      image = ImageType::New();
  typename ImageType::SizeType     size;
  typename ImageType::IndexType    start;
  typename ImageType::RegionType   region;

  size[0] = 50;
  size[1] = 50;

  start.Fill( 0 );
  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate(true); // initialize buffer to zero

  // Fill the image with a straight line
  for( unsigned int i = 0; i < 50; ++i )
    {
    typename ImageType::IndexType ind;
    ind[0] = i;
    ind[1] = 25;
    image->SetPixel(ind,1);
    ind[1] = 26;
    image->SetPixel(ind,1);
    }

  // Test the derivative of Gaussian image function
  typedef itk::GaussianDerivativeImageFunction< ImageType > DoGFunctionType;
  typename DoGFunctionType::Pointer DoG = DoGFunctionType::New();

  DoG->SetInputImage( image );

  std::cout << "Testing Set/GetSigma(): ";

  DoG->SetSigma(2.0);
  const double* sigma = DoG->GetSigma();
  for(unsigned int i = 0; i < Dimension; ++i)
    {
    if( sigma[i] != 2.0)
      {
      std::cerr << "[FAILED]" << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << "[PASSED] " << std::endl;


  std::cout << "Testing Set/GetExtent(): ";

  DoG->SetExtent(4.0);
  const double* ext = DoG->GetExtent();
  for( unsigned int i = 0; i < Dimension; ++i )
    {
    if( ext[i] != 4.0)
      {
      std::cerr << "[FAILED]" << std::endl;
      return EXIT_FAILURE;
      }
    }
  std::cout << "[PASSED] " << std::endl;

  std::cout << "Testing consistency within Index/Point/ContinuousIndex: ";
  itk::Index< Dimension > index;
  index.Fill(25);
  typename DoGFunctionType::OutputType gradientIndex;
  gradientIndex = DoG->EvaluateAtIndex( index );

  typename DoGFunctionType::PointType pt;
  pt[0] = 25.0;
  pt[1] = 25.0;
  typename DoGFunctionType::OutputType gradientPoint;
  gradientPoint = DoG->Evaluate( pt );

  typename DoGFunctionType::ContinuousIndexType continuousIndex;
  continuousIndex.Fill(25);
  typename DoGFunctionType::OutputType gradientContinuousIndex;
  gradientContinuousIndex = DoG->EvaluateAtContinuousIndex( continuousIndex );

  if( gradientIndex != gradientPoint || gradientIndex != gradientContinuousIndex )
    {
    std::cerr << "[FAILED] : " << gradientIndex << " : " << gradientPoint << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED] " << std::endl;
  gradientPoint.Normalize(); // normalize the vector

  std::cout << "Testing Evaluate() : ";

  if( (gradientPoint[0] > 0.1) || ( std::fabs(gradientPoint[1] + 1.0 ) > 10e-4 ) )
    {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED] " << std::endl;

  pt[0] = 25.0;
  pt[1] = 26.0;
  gradientPoint = DoG->Evaluate( pt );

  gradientPoint.Normalize(); // normalize the vector;

  std::cout << "Testing Evaluate() : ";

  if( (gradientPoint[0] > 0.1) || ( std::fabs(gradientPoint[1] - 1.0 ) > 10e-4 ) )
    {
    std::cerr << "[FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED] " << std::endl;
  return EXIT_SUCCESS;
}

int itkGaussianDerivativeImageFunctionTest( int, char* [] )
{

  // Exercise basic object methods
  // Done outside the helper function in the test because GCC is limited
  // when calling overloaded base class functions.
  const unsigned int Dimension = 2;
  typedef float                               PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef itk::GaussianDerivativeImageFunction< ImageType > DoGFunctionType;

  DoGFunctionType::Pointer DoG = DoGFunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( DoG, GaussianDerivativeImageFunction, ImageFunction );


  std::cout << "\nTesting derivative of Gaussian image function for float" << std::endl;
  if( TestGaussianDerivativeImageFunction< float >() == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }
  std::cout << "\nTesting derivative of Gaussian image function for unsigned short" << std::endl;
  if( TestGaussianDerivativeImageFunction< unsigned short >() == EXIT_FAILURE )
    {
    return EXIT_FAILURE;
    }

  std::cout << "\nTesting Gaussian Derivative Spatial Function:";

  typedef itk::GaussianDerivativeSpatialFunction<double,1>  GaussianDerivativeFunctionType;
  GaussianDerivativeFunctionType::Pointer f = GaussianDerivativeFunctionType::New();

  f->SetScale(1.0);
  if(f->GetScale() != 1.0)
    {
    std::cerr << "Get Scale : [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  f->SetNormalized(true);
  if(!f->GetNormalized())
    {
    std::cerr << "GetNormalized : [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  GaussianDerivativeFunctionType::ArrayType s;
  s[0] = 1.0;
  f->SetSigma(s);
  if(f->GetSigma()[0] != 1.0)
    {
    std::cerr << "GetSigma : [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  GaussianDerivativeFunctionType::ArrayType m;
  m[0] = 0.0;
  f->SetMean(m);
  if(f->GetMean()[0] != 0.0)
    {
    std::cerr << "GetMean : [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  f->SetDirection(0);
  if(f->GetDirection() != 0)
    {
    std::cerr << "GetDirection : [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  GaussianDerivativeFunctionType::InputType point;
  point[0] = 0.0;

  if(f->Evaluate(point) != 0.0)
    {
    std::cerr << "Evaluate: [FAILED]" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << f << std::endl;
  std::cout << "[PASSED] " << std::endl;

  std::cout << "GaussianDerivativeImageFunctionTest: [DONE] " << std::endl;
  return EXIT_SUCCESS;
}
