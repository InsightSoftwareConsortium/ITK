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

#include "itkTobogganImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTextOutput.h"
#include "itkTestingMacros.h"


int itkTobogganImageFilterTest( int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Missing parameters" << std::endl;
    std::cerr << "Usage: " << argv[0] << " InputImage OutputImage" << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned char                                 PixelType;
  typedef float                                         FloatPixelType;
  typedef itk::Image< PixelType, Dimension >            InputImageType;
  typedef itk::Image< FloatPixelType, Dimension >       FloatImageType;
  typedef itk::Image< PixelType, Dimension >            OutputImageType;
  typedef itk::Image< itk::IdentifierType, Dimension >  LongImageType;


  // Create a pipeline
  typedef itk::CastImageFilter< InputImageType, FloatImageType >
                                           InputCastFilterType;
  typedef itk::TobogganImageFilter< FloatImageType >
                                           FilterType;
  typedef itk::CastImageFilter< LongImageType, OutputImageType >
                                           OutputCastFilterType;
  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter< FloatImageType, FloatImageType >
                                           GMGaussianType;


  itk::ImageFileReader< InputImageType >::Pointer reader =
    itk::ImageFileReader< InputImageType >::New();

  reader->SetFileName( argv[1] );

  TRY_EXPECT_NO_EXCEPTION( reader->Update() );


  FilterType::Pointer toboggan = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( toboggan, TobogganImageFilter,
    ImageToImageFilter );

  InputCastFilterType::Pointer inputCaster = InputCastFilterType::New();
  GMGaussianType::Pointer gmgaussian = GMGaussianType::New();

  inputCaster->SetInput( reader->GetOutput() );
  gmgaussian->SetInput( inputCaster->GetOutput() );
  gmgaussian->SetSigma( 15.0 );

  toboggan->SetInput( gmgaussian->GetOutput() );


  TRY_EXPECT_NO_EXCEPTION( toboggan->Update() );


  // Cast the output of the Toboggan filter
  OutputCastFilterType::Pointer outputCaster = OutputCastFilterType::New();
  outputCaster->SetInput( toboggan->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( outputCaster->Update() );

  // Write the output
  typedef itk::ImageFileWriter< OutputImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( outputCaster->GetOutput() );
  writer->SetFileName( argv[2] );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
