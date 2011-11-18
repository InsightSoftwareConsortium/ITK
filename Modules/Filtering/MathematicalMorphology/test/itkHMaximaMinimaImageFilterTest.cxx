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

//

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkFilterWatcher.h"
#include "itkHMaximaImageFilter.h"
#include "itkHMinimaImageFilter.h"

int itkHMaximaMinimaImageFilterTest( int argc, char * argv[] )
{
  if( argc < 4 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile  height" << std::endl;
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
  typedef unsigned char    WritePixelType;

  typedef itk::Image< InputPixelType,  Dimension >    InputImageType;
  typedef itk::Image< InternalPixelType,  Dimension > InternalImageType;
  typedef itk::Image< OutputPixelType, Dimension >    OutputImageType;
  typedef itk::Image< WritePixelType, Dimension >     WriteImageType;


  // readers/writers
  typedef itk::ImageFileReader< InputImageType  >  ReaderType;
  typedef itk::ImageFileWriter< WriteImageType >   WriterType;

  // define the hmaxima filter
  typedef itk::HMaximaImageFilter<
                            InputImageType,
                            InternalImageType >  HmaximaFilterType;
  // define the hminima filter
  typedef itk::HMinimaImageFilter<
                            InternalImageType,
                            OutputImageType >  HminimaFilterType;


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();

  // Create the filters
  HmaximaFilterType::Pointer  hmaxima = HmaximaFilterType::New();
  FilterWatcher watchHmaxima(hmaxima,"hmaxima");
  HminimaFilterType::Pointer  hminima = HminimaFilterType::New();

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName(  argv[2] );

  // Setup the hmaxima method
  hmaxima->SetInput(  reader->GetOutput() );
  hmaxima->SetHeight( static_cast<InputPixelType>(atof(argv[3])) );

  // Setup the hminima method
  hminima->SetInput(  hmaxima->GetOutput() );
  hminima->SetHeight( static_cast<InputPixelType>(atof(argv[3])) );

  // Run the filter
  writer->SetInput( hminima->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;

}
