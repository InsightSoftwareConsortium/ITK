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
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkTestingMacros.h"


int itkBinaryImageToLabelMapFilterTest2( int argc, char * argv [] )
{

  if( argc != 6 )
  {
    std::cerr << "usage: " << argv[0];
    std::cerr << "inputBinaryImage outputLabelImage";
    std::cerr << "foregroundValue backgroundValue NumThreads";
    std::cerr << std::endl;
    return EXIT_FAILURE;
  }

  const unsigned int Dimension = 2;

  typedef unsigned char   BinaryPixelType;
  typedef unsigned short  LabelPixelType;

  typedef itk::Image< BinaryPixelType, Dimension >      ImageType;
  typedef itk::LabelObject< LabelPixelType, Dimension > LabelObjectType;
  typedef itk::LabelMap< LabelObjectType >              LabelMapType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::BinaryImageToLabelMapFilter< ImageType, LabelMapType > ImageToLabelType;
  ImageToLabelType::Pointer imageToLabel = ImageToLabelType::New();

  imageToLabel->SetInput( reader->GetOutput() );
  imageToLabel->SetFullyConnected(true);
  imageToLabel->SetInputForegroundValue( atoi(argv[3]) );
  imageToLabel->SetOutputBackgroundValue( atoi(argv[4]) );
  imageToLabel->SetNumberOfThreads( atoi(argv[5]) );
  imageToLabel->Update();

  std::cout << "There are " << imageToLabel->GetOutput()->GetNumberOfLabelObjects() << " objects." << std::endl;

  TEST_EXPECT_EQUAL( imageToLabel->GetOutput()->GetNumberOfLabelObjects(), imageToLabel->GetNumberOfObjects() );

  typedef itk::LabelMapToLabelImageFilter< LabelMapType, ImageType> LabelToImageType;
  LabelToImageType::Pointer labelToImage = LabelToImageType::New();
  labelToImage->SetInput( imageToLabel->GetOutput() );

  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetFileName( argv[2] );
  writer->SetInput( labelToImage->GetOutput() );
  writer->Update();

  return EXIT_SUCCESS;
}
