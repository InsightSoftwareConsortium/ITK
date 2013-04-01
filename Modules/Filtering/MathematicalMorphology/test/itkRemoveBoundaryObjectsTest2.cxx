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

#include "itkGrayscaleGrindPeakImageFilter.h"
#include "itkXorImageFilter.h"

int itkRemoveBoundaryObjectsTest2( int argc, char * argv[] )
{
  if( argc < 3 )
    {
    std::cerr << "Usage: " << std::endl;
    std::cerr << argv[0] << "  inputImageFile  ";
    std::cerr << " outputImageFile  " << std::endl;
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

  // define the fillhole filter
  typedef itk::GrayscaleGrindPeakImageFilter<
                            InputImageType,
                            OutputImageType >  GrindPeakFilterType;

  // define the xor and not filters
  typedef itk::XorImageFilter<InputImageType, InputImageType, OutputImageType>
    XorFilterType;

  // Creation of Reader and Writer filters
  ReaderType::Pointer reader = ReaderType::New();
  WriterType::Pointer writer  = WriterType::New();

  // Create the filter
  GrindPeakFilterType::Pointer  grindpeak = GrindPeakFilterType::New();

  // Create the xor and not filter
  XorFilterType::Pointer xorfilter = XorFilterType::New();

  // Setup the input and output files
  reader->SetFileName( argv[1] );
  writer->SetFileName( argv[2] );

  // Setup the grindpeak method
  grindpeak->SetInput( reader->GetOutput() );

  // Setup the xor and not
  xorfilter->SetInput1( grindpeak->GetOutput() );
  xorfilter->SetInput2( reader->GetOutput() );

  // Run the filter
  writer->SetInput( xorfilter->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}
