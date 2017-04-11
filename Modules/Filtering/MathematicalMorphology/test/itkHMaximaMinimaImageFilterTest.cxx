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
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"
#include "itkHMaximaImageFilter.h"
#include "itkHMinimaImageFilter.h"
#include "itkTestingMacros.h"

int itkHMaximaMinimaImageFilterTest( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << " inputImageFile";
    std::cerr << " outputImageFile height" << std::endl;
    return EXIT_FAILURE;
    }

  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;

  typedef unsigned short   InputPixelType;
  typedef short            InternalPixelType;
  typedef unsigned char    OutputPixelType;

  typedef itk::Image< InputPixelType, Dimension >     InputImageType;
  typedef itk::Image< InternalPixelType, Dimension >  InternalImageType;
  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;

  // Readers/writers
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< OutputImageType >  WriterType;

  // Define the itk::HMaximaImageFilter filter type
  typedef itk::HMaximaImageFilter<
                            InputImageType,
                            InternalImageType > HMaximaFilterType;
  // Define the itk::HMinimaImageFilter filter type
  typedef itk::HMinimaImageFilter<
                            InternalImageType,
                            OutputImageType > HMinimaFilterType;


  // Creation of reader and writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer = WriterType::New();

  // Create the filters
  HMaximaFilterType::Pointer hMaximaFilter = HMaximaFilterType::New();
  HMinimaFilterType::Pointer hMinimaFilter = HMinimaFilterType::New();

  FilterWatcher watchHmaxima( hMaximaFilter, "HMaximaImageFilter" );
  FilterWatcher watchHminima( hMinimaFilter, "HMinimaImageFilter" );

  // Set up the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Set up the filters
  hMaximaFilter->SetInput( reader->GetOutput() );
  hMaximaFilter->SetHeight( static_cast< InputPixelType >( atof( argv[3] ) ) );

  hMinimaFilter->SetInput( hMaximaFilter->GetOutput() );
  hMinimaFilter->SetHeight( static_cast< InputPixelType >( atof( argv[3] ) ) );

  // Run the filter
  writer->SetInput( hMinimaFilter->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
