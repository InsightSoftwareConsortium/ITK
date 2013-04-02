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

#include <iostream>
#include "itkDerivativeImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkSimpleFilterWatcher.h"

int itkDerivativeImageFilterTest(int argc, char *argv [] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile normalizedOutputImageFile ";
    std::cerr << " derivativeOrder direction" << std::endl;
    return EXIT_FAILURE;
    }


  // Test using an unsigned integral pixel type and generate a signed
  // integral pixel type
  typedef   unsigned short  InputPixelType;
  typedef   short           OutputPixelType;

  const unsigned int Dimension = 2;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;


  typedef itk::ImageFileReader< InputImageType  >  ReaderType;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetFileName( argv[1] );

  // Define the filter
  typedef itk::DerivativeImageFilter<
               InputImageType, OutputImageType >  FilterType;

  FilterType::Pointer filter = FilterType::New();

  // setup the filter
  filter->SetOrder(     atoi( argv[3] ) );
  filter->SetDirection( atoi( argv[4] ) );

  itk::SimpleFilterWatcher watcher(filter, "Derivative");

  // wire the pipeline
  filter->SetInput( reader->GetOutput() );

  // Write the output
  typedef itk::Image< unsigned char, Dimension >  WriteImageType;

  typedef itk::RescaleIntensityImageFilter<
                                  OutputImageType,
                                  WriteImageType >    NormalizeFilterType;

  typedef itk::ImageFileWriter< WriteImageType >       NormalizedWriterType;

  NormalizeFilterType::Pointer normalizer = NormalizeFilterType::New();
  NormalizedWriterType::Pointer normalizedWriter = NormalizedWriterType::New();

  normalizer->SetInput( filter->GetOutput() );
  normalizedWriter->SetInput( normalizer->GetOutput() );

  normalizer->SetOutputMinimum(   0 );
  normalizer->SetOutputMaximum( 255 );

  normalizedWriter->SetFileName( argv[2] );
  normalizedWriter->Update();

  return EXIT_SUCCESS;
}
