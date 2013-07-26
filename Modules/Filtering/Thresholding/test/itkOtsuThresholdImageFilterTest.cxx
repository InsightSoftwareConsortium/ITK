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
#include "itkFilterWatcher.h"

int itkOtsuThresholdImageFilterTest(int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFile [numberOfHistogramBins flipOutputIntensities]";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  typedef  short          InputPixelType;
  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;

  typedef itk::OtsuThresholdImageFilter<
               InputImageType, OutputImageType >  FilterType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;

  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  FilterType::Pointer filter = FilterType::New();
  WriterType::Pointer writer = WriterType::New();

  FilterWatcher watcher(filter);

  reader->SetFileName( argv[1] );
  filter->SetInput( reader->GetOutput() );
  if( argc > 3 )
  {
    filter->SetNumberOfHistogramBins (atoi(argv[3]));
  }
  if( argc > 4 )
  {
    bool flipOutputIntensities = atoi(argv[4]);
    if( flipOutputIntensities )
    {
      // Flip the inside and outside values.
      FilterType::OutputPixelType outsideValue = filter->GetInsideValue();
      FilterType::OutputPixelType insideValue = filter->GetOutsideValue();
      filter->SetInsideValue( insideValue );
      filter->SetOutsideValue( outsideValue );
    }
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
