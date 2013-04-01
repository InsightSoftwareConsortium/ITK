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

#include "itkGrayscaleConnectedClosingImageFilter.h"


int itkGrayscaleConnectedClosingImageFilterTest( int argc, char * argv[] )
{
  if( argc < 5 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile seedX seedY " << std::endl;
    return EXIT_FAILURE;
    }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  const unsigned int Dimension = 2;

  typedef unsigned char   InputPixelType;
  typedef unsigned char   OutputPixelType;
  typedef unsigned char   WritePixelType;

  typedef itk::Image< InputPixelType,  Dimension >   InputImageType;
  typedef itk::Image< OutputPixelType, Dimension >   OutputImageType;
  typedef itk::Image< WritePixelType, Dimension >    WriteImageType;


  // readers/writers
  typedef itk::ImageFileReader< InputImageType  > ReaderType;
  typedef itk::ImageFileWriter< WriteImageType >  WriterType;

  // define the connected closing filter
  typedef itk::GrayscaleConnectedClosingImageFilter<
                            InputImageType,
                            OutputImageType >  ConnectedClosingFilterType;


  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();

  // Create the filter
  ConnectedClosingFilterType::Pointer  connectedClosing = ConnectedClosingFilterType::New();
  FilterWatcher watcher(connectedClosing, "connectedClosing");

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName(  argv[2] );

  // Setup the connectedopening method
  connectedClosing->SetInput(  reader->GetOutput() );

  InputImageType::IndexType seed;
  seed[0] = atoi(argv[3]);
  seed[1] = atoi(argv[4]);
  connectedClosing->SetSeed(seed);

  // Run the filter
  writer->SetInput( connectedClosing->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}
