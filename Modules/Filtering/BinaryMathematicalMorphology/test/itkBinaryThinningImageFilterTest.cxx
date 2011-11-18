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

#include "itkImageFileReader.h"
#include "itkBinaryThinningImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkImageFileWriter.h"

int itkBinaryThinningImageFilterTest(int argc, char* argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " inputImageFile outputImageFile";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  typedef  short          InputPixelType;
  typedef  unsigned char  OutputPixelType;

  typedef itk::Image< InputPixelType,  2 >   InputImageType;
  typedef itk::Image< OutputPixelType, 2 >   OutputImageType;

  typedef itk::ImageFileReader< InputImageType >  ReaderType;
  typedef itk::BinaryThinningImageFilter< InputImageType, InputImageType >  ThinningType;
  typedef itk::RescaleIntensityImageFilter< InputImageType, OutputImageType > RescaleType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  ReaderType::Pointer reader = ReaderType::New();
  ThinningType::Pointer thinning = ThinningType::New();
  RescaleType::Pointer rescaler = RescaleType::New();
  WriterType::Pointer writer = WriterType::New();

  // Set up the reader
  reader->SetFileName( argv[1] );

  // Set up the filter parameters.
  thinning->SetInput( reader->GetOutput() );

  // Rescale the image so that it can be seen.
  rescaler->SetInput( thinning->GetOutput() );
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);

  // Write out the test image
  writer->SetFileName( argv[2] );
  writer->SetInput( rescaler->GetOutput() );
  try
    {
    writer->Update();
    }
  catch( itk::ExceptionObject & excep )
    {
    std::cerr << "Exception caught !" << std::endl;
    std::cerr << excep << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
