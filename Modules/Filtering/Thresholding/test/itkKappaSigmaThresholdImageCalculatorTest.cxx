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

#include "itkKappaSigmaThresholdImageCalculator.h"
#include "itkImageFileReader.h"


int itkKappaSigmaThresholdImageCalculatorTest( int argc, char * argv [] )
{
  if( argc < 5 )
    {
    std::cerr << "Missing arguments" << std::endl;
    std::cerr << "Usage:" << std::endl;
    std::cerr << argv[0] << std::endl;
    std::cerr << "inputImage numberOfIterations sigmaFactor expectedThreshold" << std::endl;
    return EXIT_FAILURE;
    }

  typedef signed short                          PixelType;
  const unsigned int                            Dimension = 2;

  typedef itk::Image< PixelType, Dimension >     ImageType;
  typedef itk::Image< unsigned char, Dimension > MaskType;
  typedef itk::ImageFileReader< ImageType >      ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  typedef itk::KappaSigmaThresholdImageCalculator< ImageType, MaskType >  CalculatorType;

  std::cout << "Testing Kappa Sigma Image Calulator:\n";

  try
    {
    reader->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  /* Create and initialize the calculator */
  CalculatorType::Pointer calculator = CalculatorType::New();
  calculator->SetImage( reader->GetOutput() );
  calculator->SetNumberOfIterations( atoi( argv[2] ) );
  calculator->SetSigmaFactor( atof( argv[3] ) );
  calculator->SetMaskValue( 255 );

  // Exercise Get methods
  std::cout << "Number of iterations = " << calculator->GetNumberOfIterations() << std::endl;
  std::cout << "Sigma factor         = " << calculator->GetSigmaFactor() << std::endl;
  std::cout << "Mask value           = " << calculator->GetMaskValue() << std::endl;


  calculator->Compute();

  PixelType threshold = calculator->GetOutput();

  std::cout << "calculator: " << calculator;
  std::cout << "Threshold: " << threshold;
  std::cout << std::endl;

  // Note that this notion of "expected" value is only for regression testing of the class.
  // In a typical usage of this class, you will simply take the calculator->GetOutput().
  PixelType expectedThreshold = atoi( argv[4] );

  if( itk::Math::abs( expectedThreshold - threshold ) > 1e-3 )
    {
    std::cerr << "Test failed" << std::endl;
    std::cerr << "Expected threshold = " << expectedThreshold << std::endl;
    std::cerr << "bu Found threshold = " << threshold << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
