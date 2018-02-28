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
#include "itkArray.h"
#include "itkResampleImageFilter.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkPhaseCorrelationImageRegistrationMethod.h"
#include "itkMaxPhaseCorrelationOptimizer.h"
#include "itkImageFileWriter.h"
#include "itkCastImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

namespace itk
{

template < unsigned int NDimension,
     typename TFixedImagePixel,
     typename TMovingImagePixel >
int PhaseCorrelationRegistrationFiles( int argc, char* argv[] )
{
  bool pass = true;

  constexpr unsigned int Dimension = NDimension;
  using FixedPixelType = TFixedImagePixel;
  using MovingPixelType = TMovingImagePixel;
  using FixedImageType = itk::Image< FixedPixelType, Dimension>;
  using MovingImageType = itk::Image< TMovingImagePixel, Dimension>;

  using FixedReaderType = itk::ImageFileReader< FixedImageType >;
  typename FixedReaderType::Pointer fixedReader = FixedReaderType::New();
  fixedReader->SetFileName( argv[1] );
  const FixedImageType * fixedImage = fixedReader->GetOutput();

  using MovingReaderType = itk::ImageFileReader< MovingImageType >;
  typename MovingReaderType::Pointer movingReader = MovingReaderType::New();
  movingReader->SetFileName( argv[2] );
  const MovingImageType * movingImage = movingReader->GetOutput();

  // Registration method
  using PhaseCorrelationMethodType = \
    itk::PhaseCorrelationImageRegistrationMethod< FixedImageType, MovingImageType >;
  typename PhaseCorrelationMethodType::Pointer phaseCorrelationMethod = PhaseCorrelationMethodType::New();
  phaseCorrelationMethod->SetFixedImage( fixedImage );
  phaseCorrelationMethod->SetMovingImage( movingImage );

  // Operator type
  using OperatorType = itk::PhaseCorrelationOperator< typename itk::NumericTraits< TFixedImagePixel >::RealType, NDimension >;
  typename OperatorType::Pointer pcmOperator = OperatorType::New();
  phaseCorrelationMethod->SetOperator( pcmOperator );

  // Optimizer type
  using OptimizerType = itk::MaxPhaseCorrelationOptimizer< PhaseCorrelationMethodType >;
  typename OptimizerType::Pointer pcmOptimizer = OptimizerType::New();
  phaseCorrelationMethod->SetOptimizer( pcmOptimizer );

  // Transform type
  using TransformType = typename PhaseCorrelationMethodType::TransformType;
  using ParametersType = typename TransformType::ParametersType;

  //
  // Execute the registration.
  // This can potentially throw an exception
  //
  try
    {
    phaseCorrelationMethod->Update();
    }
  catch( itk::ExceptionObject & error )
    {
    std::cerr << error << std::endl;
    pass = false;
    }

  //
  // Get registration result and validate it.
  //
  ParametersType finalParameters     = phaseCorrelationMethod->GetTransformParameters();
  ParametersType transformParameters = phaseCorrelationMethod->GetOutput()->Get()->GetParameters();

  const unsigned int numberOfParameters = finalParameters.Size();
  ParametersType actualParameters( numberOfParameters );
  for (unsigned int ii = 3; ii < 2 + numberOfParameters; ++ii )
    {
    if( argc < ii + 1 )
      {
      std::cerr << "Did not find baseline transform component in argument: " << ii << std::endl;
      pass = false;
      return EXIT_FAILURE;
      }
    actualParameters[ii - 3] = atof( argv[ii] );
    }


  const double tolerance = 1.0;  // equivalent to 1 pixel.

  // Validate first two parameters (introduced by image source)
  for( unsigned int ii = 0; ii < numberOfParameters; ++ii )
    {
    // the parameters are negated in order to get the inverse transformation.
    // this only works for comparing translation parameters....
    std::cout << finalParameters[ii] << " == "
              << actualParameters[ii] << " == "
              << transformParameters[ii] << std::endl;

    if(  ( itk::Math::abs( finalParameters[ii] - actualParameters[ii] ) > tolerance ) ||
         ( itk::Math::abs( transformParameters[ii] - actualParameters[ii] ) > tolerance ) )
      {
      std::cerr << "Tolerance exceeded at component " << ii << std::endl;
      pass = false;
      }
    }

  if( !pass )
    {
    std::cout << "Test FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED." << std::endl;
  return EXIT_SUCCESS;
}

}


int itkPhaseCorrelationImageRegistrationMethodTestFiles( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0] << " <fixedImageFile> <movingImageFile>" << std::endl;
    return EXIT_FAILURE;
    }

  return itk::PhaseCorrelationRegistrationFiles< 2, unsigned short, unsigned short >( argc, argv );
}
