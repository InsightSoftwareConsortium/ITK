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

#include "itkKappaSigmaThresholdImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int itkKappaSigmaThresholdImageFilterTest(int argc, char* argv[] )
{

  if( argc < 5 )
    {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImageFile outputImageFile iterations sigmaFactor";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  using InputPixelType = unsigned char;
  using MaskPixelType = unsigned char;
  using OutputPixelType = unsigned char;

  constexpr unsigned int Dimension = 2;

  using InputImageType = itk::Image< InputPixelType,  Dimension >;
  using MaskImageType = itk::Image< MaskPixelType,   Dimension >;
  using OutputImageType = itk::Image< OutputPixelType, Dimension >;

  using FilterType = itk::KappaSigmaThresholdImageFilter<
    InputImageType, MaskImageType, OutputImageType >;

  using ReaderType = itk::ImageFileReader< InputImageType >;

  using WriterType = itk::ImageFileWriter< OutputImageType >;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  WriterType::Pointer writer = WriterType::New();

  itk::SimpleFilterWatcher watcher(filter);

  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );
  writer->SetInput( filter->GetOutput() );

  filter->SetOutsideValue( 0 );
  filter->SetInsideValue( 255 );
  filter->SetMaskValue( 255 );
  filter->SetSigmaFactor( std::stod( argv[3] ) );
  filter->SetNumberOfIterations( std::stoi( argv[4] ) );

  filter->Print( std::cout );

  std::cout << " GetOutsideValue()       = " << filter->GetOutsideValue() << std::endl;
  std::cout << " GetInsideValue()        = " << filter->GetInsideValue() << std::endl;
  std::cout << " GetMaskValue()          = " << filter->GetMaskValue() << std::endl;
  std::cout << " GetSigmaFactor()        = " << filter->GetSigmaFactor() << std::endl;
  std::cout << " GetNumberOfIterations() = " << filter->GetNumberOfIterations() << std::endl;

  filter->Update();

  std::cout << "Computed Threshold is: " << filter->GetThreshold() << std::endl;

  writer->SetFileName( argv[2] );
  writer->Update();

  return EXIT_SUCCESS;
}
