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

#include "itkOtsuThresholdImageFilter.h"

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int itkOtsuThresholdImageFilterTest(int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputImageFile outputImageFile [numberOfHistogramBins flipOutputIntensities]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  using InputPixelType = short;
  using OutputPixelType = unsigned char;

  using InputImageType = itk::Image< InputPixelType,  2 >;
  using OutputImageType = itk::Image< OutputPixelType, 2 >;

  using FilterType = itk::OtsuThresholdImageFilter<
               InputImageType, OutputImageType >;

  using ReaderType = itk::ImageFileReader< InputImageType >;

  using WriterType = itk::ImageFileWriter< OutputImageType >;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  WriterType::Pointer writer = WriterType::New();

  itk::SimpleFilterWatcher watcher(filter);

#if defined(ITKV4_COMPATIBILITY)
  TEST_EXPECT_TRUE( filter->GetReturnBinMidpoint() );
#else
  TEST_EXPECT_TRUE( !filter->GetReturnBinMidpoint() );
#endif

  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );
  if( argc > 3 )
    {
    filter->SetNumberOfHistogramBins (std::stoi(argv[3]));
    }
  if( argc > 4 )
    {
    bool flipOutputIntensities = std::stoi(argv[4]);
    if( flipOutputIntensities )
      {
      // Flip the inside and outside values.
      FilterType::OutputPixelType outsideValue = filter->GetInsideValue();
      FilterType::OutputPixelType insideValue = filter->GetOutsideValue();
      filter->SetInsideValue( insideValue );
      filter->SetOutsideValue( outsideValue );
      }
    }
  if( argc > 5 )
    {
    bool returnBinMidpoint =  static_cast< bool >( std::stoi( argv[5] ) );
    TEST_SET_GET_BOOLEAN( filter, ReturnBinMidpoint, returnBinMidpoint );
    }
  filter->Update();

  // Test GetMacros
  unsigned long numberOfHistogramBins = filter->GetNumberOfHistogramBins();
  std::cout << "filter->GetNumberOfHistogramBins(): "
            << numberOfHistogramBins
            << std::endl;
  std::cout << "Computed Threshold is: "
            << itk::NumericTraits<FilterType::InputPixelType>::PrintType(filter->GetThreshold())
            << std::endl;
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );
  writer->Update();

  return EXIT_SUCCESS;
}
