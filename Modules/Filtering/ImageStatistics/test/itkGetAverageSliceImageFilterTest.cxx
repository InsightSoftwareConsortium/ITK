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

#include "itkGetAverageSliceImageFilter.h"
#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"

int itkGetAverageSliceImageFilterTest(int argc, char *argv[] )
{
  typedef short PixelType;
  static ITK_CONSTEXPR_VAR int ImageDimension = 3;

  typedef itk::Image<PixelType,ImageDimension>     InputImageType;
  typedef itk::Image<PixelType,ImageDimension>     OutputImageType;
  typedef itk::Image<unsigned char,ImageDimension> WriteImageType;
  typedef itk::ImageSeriesReader< InputImageType > ReaderType;
  typedef itk::GetAverageSliceImageFilter<InputImageType,OutputImageType>
                                                   GetAveragerType;
  typedef itk::ImageSeriesWriter<OutputImageType,WriteImageType>
                                                   WriterType;
  typedef itk::GDCMSeriesFileNames                 SeriesFileNames;
  typedef itk::GDCMImageIO                         ImageIOType;

  if (argc < 3)
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputDICOMDirectory outputFile" << std::endl;
    return EXIT_FAILURE;
    }

  // Get the input filenames
  SeriesFileNames::Pointer names = SeriesFileNames::New();

  // Get the DICOM filenames from the directory
  names->SetInputDirectory( argv[1] );

  // Create the reader
  ImageIOType::Pointer gdcmIO = ImageIOType::New();
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetImageIO( gdcmIO );
  try
    {
    reader->SetFileNames( names->GetInputFileNames() );
    reader->Update();
    }
  catch (itk::ExceptionObject &excp)
    {
    std::cerr << "Error reading the series" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }

  // GetAverage the input images
  GetAveragerType::Pointer average = GetAveragerType::New();
  average->SetInput( reader->GetOutput() );
  average->SetAveragedOutDimension( 2 );

  try
    {
    average->Update();
    }
  catch ( itk::ExceptionObject &excp)
    {
    std::cerr << "Error running the average filter" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
    }
  average->GetOutput()->Print(std::cout);

  average->Print( std::cout );

  try
    {
    WriterType::Pointer writer = WriterType::New();
    writer->SetFileName ( argv[2] );

    writer->SetInput(average->GetOutput());
    writer->Update();
    }
  catch( itk::ExceptionObject & excp )
    {
    std::cerr << "Error writing the series" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;

    }
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;

}
